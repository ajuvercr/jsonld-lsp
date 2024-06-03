use core::fmt;
use std::{io::Cursor, ops::Range, str::FromStr, sync::Arc};

use chumsky::prelude::Simple;
use futures::lock::Mutex;
use hashbrown::HashMap;
use iref::IriBuf;
use json_ld::{ContextLoader, ExtractContext};
use json_ld_syntax::context::{AnyValueMut, Value};
use locspan::Span;
use lsp_types::{CompletionItemKind, Position, SemanticToken, SemanticTokenType};
use ropey::Rope;
use tracing::debug;

use crate::{
    backend::Client,
    contexts::filter_definition,
    model::Spanned,
    parent::ParentingSystem,
    semantics::semantic_tokens,
    utils::{position_to_offset, ReqwestLoader},
};

use self::{
    parent::JsonNode,
    parser::{Json, JsonFormatter},
    tokenizer::{tokenize, JsonToken},
};

use super::{CurrentLangState, Lang, LangState, Node, SimpleCompletion};

pub mod parent;
pub mod parser;
pub mod tokenizer;

pub type Cache = Arc<Mutex<HashMap<String, Value<Span>>>>;

async fn load_ctx(cache: &Cache, id: &str) -> Value<Span> {
    {
        let c = cache.lock().await;

        if let Some(cached) = c.get(id) {
            return cached.clone();
        }
    }

    let mut loader = ReqwestLoader::default();
    let ctx = loader
        .load_context(IriBuf::from_string(id.to_string()).unwrap())
        .await
        .unwrap();

    let out = ctx.into_document().into_value();

    let mut c = cache.lock().await;
    c.insert(id.to_string(), out.clone());
    out
}

async fn set_ctx(cache: &Cache, id: &str, json: String) {
    use json_ld::syntax::{Parse as _, Value};

    let value = Value::parse_str(
        &json,
        |span| span, // keep the source `Span` of each element as metadata.
    )
    .expect("unable to parse file");

    let ctx = ExtractContext::extract_context(value).unwrap();

    let mut c = cache.lock().await;
    c.insert(id.to_string(), ctx.into_value());
}

pub struct JsonLd {
    id: String,
    cache: Cache,
    rope: Rope,
}

impl fmt::Debug for JsonLd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "JsonLd {{ id: {}, json: {} }}", self.id, self.rope)
    }
}

impl JsonLd {
    fn new(id: String, cache: Cache) -> Self {
        Self {
            id,
            cache,
            rope: Rope::from(""),
        }
    }
}

impl Node<JsonToken> for JsonNode {
    fn leaf<'a>(&'a self) -> Option<&'a JsonToken> {
        match self {
            JsonNode::Leaf(ref x) => Some(x),
            _ => None,
        }
    }
}

impl Lang for JsonLd {
    type State = Cache;

    type Token = JsonToken;

    type TokenError = Simple<char>;

    type Element = Json;

    type ElementError = Simple<JsonToken>;

    type RenameError = ();

    type PrepareRenameError = ();

    type Node = JsonNode;
    type NodeLeaf = JsonToken;

    const LANG: &'static str = "jsonld";
    const CODE_ACTION: bool = false;
    const HOVER: bool = true;

    const TRIGGERS: &'static [&'static str] = &["@", "\""];
    const LEGEND_TYPES: &'static [SemanticTokenType] = &[
        SemanticTokenType::VARIABLE,
        SemanticTokenType::STRING,
        SemanticTokenType::NUMBER,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::ENUM_MEMBER,
    ];

    #[tracing::instrument(skip(self), fields(id=self.id))]
    fn format(
        &mut self,
        state: &CurrentLangState<Self>,
        options: lsp_types::FormattingOptions,
    ) -> Option<String> {
        if !state.element.last_is_valid() {
            return None;
        }
        let mut indent = String::new();
        for _ in 0..options.tab_size {
            indent += " ";
        }
        let mut formatter = JsonFormatter { indent, inc: 0 };
        let mut cursor = Cursor::new(Vec::new());
        formatter.format(&state.element.current, &mut cursor).ok()?;
        String::from_utf8(cursor.into_inner()).ok()
    }

    #[tracing::instrument(skip(self), fields(id=self.id))]
    fn tokenize(&mut self, source: &str) -> (Vec<Spanned<Self::Token>>, Vec<Self::TokenError>) {
        self.rope = Rope::from(source.to_owned());
        tokenize(source)
    }

    #[tracing::instrument(skip(self), fields(id=self.id))]
    fn parse(
        &mut self,
        source: &str,
        tokens: &Vec<Spanned<Self::Token>>,
    ) -> (Spanned<Self::Element>, Vec<Self::ElementError>) {
        parser::parse(source, tokens.clone())
    }

    fn parents(&self, element: &Spanned<Self::Element>) -> ParentingSystem<Spanned<Self::Node>> {
        debug!("generating parents");
        parent::system(element.clone())
    }

    #[tracing::instrument(skip(self, state, apply), fields(id=self.id))]
    fn special_semantic_tokens(
        &self,
        state: &CurrentLangState<Self>,
        mut apply: impl FnMut(Range<usize>, lsp_types::SemanticTokenType) -> (),
    ) {
        let parents = &state.parents.last_valid;
        parents
            .iter()
            .flat_map(|(_, x)| x.as_kv())
            .filter(|(ref x, _)| x.value() == "@id")
            .map(|(_, id)| &parents[*id])
            .filter(|x| x.is_leaf())
            .map(|x| x.clone().map(|x| x.into_leaf()))
            .filter(|x| x.is_string())
            .for_each(|x| apply(x.span().clone(), SemanticTokenType::VARIABLE));

        parents
            .iter()
            .filter(|(_, x)| x.is_kv())
            .map(|(_, x)| x.to_kv())
            .for_each(|(x, _)| {
                if x.value().starts_with("@") {
                    apply(x.span().clone(), SemanticTokenType::KEYWORD);
                } else {
                    apply(x.span().clone(), SemanticTokenType::PROPERTY);
                }
            });
    }

    #[tracing::instrument(skip(self, state), fields(id=self.id))]
    fn prepare_rename(
        &self,
        state: &CurrentLangState<Self>,
        pos: usize,
    ) -> Result<(std::ops::Range<usize>, String), Self::PrepareRenameError> {
        let parents = &state.parents.last_valid;
        parents
            .iter()
            .map(|(_, x)| x)
            .filter(|x| x.is_kv())
            .map(|x| x.value().clone().into_kv())
            .filter(|(x, _)| x.value() == "@id")
            .map(|(_, id)| &parents[id])
            .filter(|x| x.span().contains(&pos))
            .filter_map(|x| x.as_leaf().map(|y| (x.span().clone(), y)))
            .find_map(|(range, x)| x.as_string().cloned().map(|x| (range, x)))
            .ok_or(())
    }

    #[tracing::instrument(skip(self, state), fields(id=self.id))]
    fn rename(
        &self,
        state: &CurrentLangState<Self>,
        pos: usize,
        new_name: String,
    ) -> Result<Vec<(std::ops::Range<usize>, String)>, Self::RenameError> {
        let (_, name) = self.prepare_rename(state, pos)?;
        let parents = &state.parents.last_valid;

        Ok(parents
            .iter()
            .map(|(_, x)| x)
            .filter(|x| x.is_kv())
            .map(|x| x.value().clone().into_kv())
            .filter(|(x, _)| x.value() == "@id")
            .map(|(_, id)| &parents[id])
            .filter_map(|x| x.as_leaf().map(|y| (x.span().clone(), y)))
            .filter_map(|(range, x)| x.as_string().cloned().map(|x| (range, x)))
            .filter(|(_, x)| x == &name)
            .map(|(range, _)| (range, new_name.clone()))
            .collect())
    }

    fn new(id: String, cache: Cache) -> (Self, CurrentLangState<Self>) {
        (Self::new(id, cache), Default::default())
    }
}

#[async_trait::async_trait]
impl<C: Client + Send + Sync + 'static> LangState<C> for JsonLd {
    #[tracing::instrument(skip(self, state, _client), fields(id=self.id))]
    async fn update(&mut self, state: &CurrentLangState<Self>, _client: &C) {
        debug!("update parents");

        if state.parents.last_is_valid() {
            if let Ok(bytes) = state.parents.last_valid.to_json_vec() {
                let st = String::from_utf8(bytes).unwrap();
                set_ctx(&self.cache, &self.id, st).await;
            }
        }
    }

    #[tracing::instrument(skip(self, state), fields(id=self.id))]
    async fn do_semantic_tokens(&mut self, state: &CurrentLangState<Self>) -> Vec<SemanticToken> {
        semantic_tokens(self, &state, &self.rope)
    }

    #[tracing::instrument(skip(self, state, _client), fields(id=self.id))]
    async fn do_completion(
        &mut self,
        trigger: Option<String>,
        position: &Position,
        state: &CurrentLangState<Self>,
        _client: &C,
    ) -> Vec<super::SimpleCompletion> {
        let parents = &state.parents.last_valid;
        let location = position_to_offset(position.clone(), &self.rope).unwrap();

        let closest_pref = state
            .tokens
            .current
            .iter()
            .filter(|x| x.1.start < location && !x.1.contains(&location))
            .min_by_key(|x| location - x.1.start);

        debug!(%location, ?closest_pref);

        let end = position.clone();
        // end.character += 1;
        let start = Position::new(end.line, end.character - 1);
        let range = lsp_types::Range::new(start, end);

        if trigger == Some("@".to_string()) {
            self.get_ids(&state)
                .into_iter()
                .map(|x| {
                    let w = Some(format!("@{x}"));

                    let edits = vec![lsp_types::TextEdit {
                        new_text: format!("{{\"@id\": \"{x}\"}}"),
                        range: range.clone(),
                    }];

                    SimpleCompletion {
                        documentation: Some(format!("Subject: {x}")),
                        kind: CompletionItemKind::VARIABLE,
                        label: x,
                        sort_text: w.clone(),
                        filter_text: w,
                        edits,
                    }
                })
                .collect()
        } else {
            if !closest_pref
                .map(|x| x.0 == JsonToken::Comma)
                .unwrap_or_default()
            {
                return Vec::new();
            }

            let iri = IriBuf::from_str(&self.id).unwrap();
            let mut x = load_ctx(&self.cache, &self.id).await;

            let context_ids: Vec<_> = parents
                .iter()
                .map(|(_, x)| x)
                .filter_map(|x| x.as_kv())
                .filter(|(x, _)| x.value() == "@context")
                .map(|(_, id)| &parents[*id])
                .flat_map(|x| {
                    if let Some(leaf_st) = x.as_leaf().and_then(|x| x.as_string()) {
                        return vec![leaf_st.clone()];
                    }
                    if let Some(arr) = x.as_array() {
                        return arr
                            .iter()
                            .map(|x| &parents[*x])
                            .flat_map(|x| x.as_leaf())
                            .flat_map(|x| x.as_string())
                            .cloned()
                            .collect();
                    }
                    Vec::new()
                })
                .collect();

            for id in context_ids
                .iter()
                .flat_map(|x| iref::IriRefBuf::from_str(x))
            {
                let resolved = id.resolved(&iri);
                // let fut = self.cache.lock().unwrap().load_context(resolved);
                let y = load_ctx(&self.cache, resolved.as_str()).await;
                x.append(y);
            }

            // doc.expand(loader.deref_mut()).await.unwrap();

            x.traverse()
                .filter_map(filter_definition)
                .map(|v| SimpleCompletion {
                    kind: CompletionItemKind::PROPERTY,
                    label: v.key.to_string(),
                    documentation: Some(format!("{} -> \"{}\"", v.key, v.value)),
                    sort_text: None,
                    filter_text: format!("\"{}", v.key).into(),
                    edits: vec![lsp_types::TextEdit {
                        new_text: format!("\"{}", v.key),
                        range: range.clone(),
                    }],
                })
                .collect()
        }
    }
}

impl JsonLd {
    fn get_ids(&self, state: &CurrentLangState<Self>) -> Vec<String> {
        let parents = &state.parents.current;
        let id_indices = parents
            .iter()
            .map(|(_, x)| x)
            .flat_map(|x| x.as_kv())
            .filter(|(id, _)| id.value() == "@id")
            .map(|(_, idx)| idx)
            .copied();

        let ids = id_indices
            .map(|x| &parents[x].0)
            .flat_map(|x| x.as_leaf())
            .flat_map(|x| x.as_string())
            .cloned();

        ids.collect()
    }
}
