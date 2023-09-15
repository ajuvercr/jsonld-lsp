use core::fmt;
use std::{ops::Range, str::FromStr, sync::Arc};

use chumsky::prelude::Simple;
use futures::lock::Mutex;
use hashbrown::HashMap;
use iref::IriBuf;
use json_ld::{ContextLoader, ExtractContext};
use json_ld_syntax::context::{AnyValueMut, Value};
use locspan::Span;
use lsp_types::{CompletionItemKind, SemanticToken, SemanticTokenType};
use ropey::Rope;

use crate::{
    contexts::filter_definition, model::Spanned, parent::ParentingSystem,
    semantics::semantic_tokens, utils::ReqwestLoader, web,
};

use self::{
    parent::JsonNode,
    parser::Json,
    tokenizer::{tokenize, JsonToken},
};

use super::{Lang, LangState, Node, SimpleCompletion};

pub mod parent;
pub mod parser;
pub mod tokenizer;

type Parents = ParentingSystem<Spanned<<JsonLd as Lang>::Node>>;
pub type Cache = Arc<Mutex<HashMap<String, Value<Span>>>>;
// pub type Loader = Arc<Mutex<ReqwestLoader<IriBuf>>>;
//

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
    parents: Parents,
    rope: Rope,
}
impl fmt::Debug for JsonLd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let bytes = self.parents.to_json_vec().unwrap();
        let json = unsafe { std::str::from_utf8_unchecked(&bytes) };

        write!(f, "JsonLd {{ id: {}, json: {} }}", self.id, json)
    }
}

impl JsonLd {
    fn new(id: String, cache: Cache) -> Self {
        let parents = parent::system(Spanned(Json::Token(JsonToken::Null), 0..0));

        Self {
            id,
            cache,
            parents,
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

    const LANG: &'static str = "jsonld";

    const TRIGGERS: &'static [&'static str] = &["@", "\""];
    const LEGEND_TYPES: &'static [SemanticTokenType] = &[
        SemanticTokenType::VARIABLE,
        SemanticTokenType::STRING,
        SemanticTokenType::NUMBER,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::ENUM_MEMBER,
    ];

    fn tokenize(&mut self, source: &str) -> (Vec<Spanned<Self::Token>>, Vec<Self::TokenError>) {
        self.rope = Rope::from(source.to_owned());
        tokenize(source)
    }

    fn parse(
        &self,
        source: &str,
        tokens: &Vec<Spanned<Self::Token>>,
    ) -> (Spanned<Self::Element>, Vec<Self::ElementError>) {
        parser::parse(source, tokens.clone())
    }

    fn parents(&self, element: &Spanned<Self::Element>) -> ParentingSystem<Spanned<Self::Node>> {
        parent::system(element.clone())
    }

    fn semantic_tokens(
        &self,
        system: &ParentingSystem<Spanned<Self::Node>>,
        mut apply: impl FnMut(Range<usize>, lsp_types::SemanticTokenType) -> (),
    ) {
        log::error!("Semantic tokens functions is called!");
        system
            .iter()
            .flat_map(|(_, x)| x.as_kv())
            .filter(|(ref x, _)| x.value() == "@id")
            .map(|(_, id)| &system[*id])
            .filter(|x| x.is_leaf())
            .map(|x| x.clone().map(|x| x.into_leaf()))
            .filter(|x| x.is_string())
            .for_each(|x| apply(x.span().clone(), SemanticTokenType::VARIABLE));

        system
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

    fn prepare_rename(
        &self,
        pos: usize,
    ) -> Result<(std::ops::Range<usize>, String), Self::PrepareRenameError> {
        self.parents
            .iter()
            .map(|(_, x)| x)
            .filter(|x| x.is_kv())
            .map(|x| x.value().clone().into_kv())
            .filter(|(x, _)| x.value() == "@id")
            .map(|(_, id)| &self.parents[id])
            .filter(|x| x.span().contains(&pos))
            .filter_map(|x| x.as_leaf().map(|y| (x.span().clone(), y)))
            .find_map(|(range, x)| x.as_string().cloned().map(|x| (range, x)))
            .ok_or(())
    }

    fn rename(
        &self,
        pos: usize,
        new_name: String,
    ) -> Result<Vec<(std::ops::Range<usize>, String)>, Self::RenameError> {
        let (_, name) = self.prepare_rename(pos)?;

        Ok(self
            .parents
            .iter()
            .map(|(_, x)| x)
            .filter(|x| x.is_kv())
            .map(|x| x.value().clone().into_kv())
            .filter(|(x, _)| x.value() == "@id")
            .map(|(_, id)| &self.parents[id])
            .filter_map(|x| x.as_leaf().map(|y| (x.span().clone(), y)))
            .filter_map(|(range, x)| x.as_string().cloned().map(|x| (range, x)))
            .filter(|(_, x)| x == &name)
            .map(|(range, _)| (range, new_name.clone()))
            .collect())
    }

    fn new(id: String, cache: Cache) -> Self {
        Self::new(id, cache)
    }
}

#[async_trait::async_trait]
impl LangState for JsonLd {
    async fn update(&mut self, parents: ParentingSystem<Spanned<<JsonLd as Lang>::Node>>) {
        self.parents = parents;
        if let Ok(bytes) = self.parents.to_json_vec() {
            let st = String::from_utf8(bytes).unwrap();
            set_ctx(&self.cache, &self.id, st).await;
            // self.loader
            //     .lock()
            //     .unwrap()
            //     .set_document(IriBuf::from_str(&self.id).unwrap(), bytes);
        }
    }

    async fn do_semantic_tokens(&mut self) -> Vec<SemanticToken> {
        log::error!("Semantic tokens functions is called!");
        semantic_tokens(self, &self.parents, &self.rope)
    }

    async fn do_completion(&mut self, trigger: Option<String>) -> Vec<super::SimpleCompletion> {
        log::error!("Completion functions is called!");
        if trigger == Some("@".to_string()) {
            self.get_ids()
                .into_iter()
                .map(|x| {
                    let w = Some(format!("@{x}"));

                    SimpleCompletion {
                        documentation: Some(format!("Subject: {x}")),
                        edit: format!("{{\"@id\": \"{x}\"}}"),
                        kind: CompletionItemKind::VARIABLE,
                        label: x,
                        sort_text: w.clone(),
                        filter_text: w,
                    }
                })
                .collect()
        } else {
            let iri = IriBuf::from_str(&self.id).unwrap();
            let mut x = load_ctx(&self.cache, &self.id).await;

            let context_ids: Vec<_> = self
                .parents
                .iter()
                .map(|(_, x)| x)
                .filter(|x| x.is_kv())
                .map(|x| x.value().clone().into_kv())
                .filter(|(x, _)| x.value() == "@context")
                .map(|(_, id)| &self.parents[id])
                .flat_map(|x| {
                    if let Some(leaf_st) = x.as_leaf().and_then(|x| x.as_string()) {
                        return vec![leaf_st.clone()];
                    }
                    if let Some(arr) = x.as_array() {
                        return arr
                            .iter()
                            .map(|x| &self.parents[*x])
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
                    edit: format!("\"{}", v.key),
                })
                .collect()
        }
    }
}

impl JsonLd {
    fn get_ids(&self) -> Vec<String> {
        let id_indices = self
            .parents
            .iter()
            .map(|(_, x)| x)
            .flat_map(|x| x.as_kv())
            .filter(|(id, _)| id.value() == "@id")
            .map(|(_, idx)| idx)
            .copied();

        let ids = id_indices
            .map(|x| &self.parents[x].0)
            .flat_map(|x| x.as_leaf())
            .flat_map(|x| x.as_string())
            .cloned();

        ids.collect()
    }
}
