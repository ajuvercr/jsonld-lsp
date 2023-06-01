use core::fmt;
use std::{ops::Range, str::FromStr, sync::Arc};

use chumsky::prelude::Simple;
use futures::lock::Mutex;
use iref::IriBuf;
use json_ld::ContextLoader;
use json_ld_syntax::context::AnyValueMut;
use lsp_types::{CompletionItemKind, SemanticToken, SemanticTokenType};
use ropey::Rope;

use crate::{
    contexts::filter_definition, model::Spanned, parent::ParentingSystem,
    semantics::semantic_tokens, utils::ReqwestLoader,
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
pub type Loader = Arc<Mutex<ReqwestLoader<IriBuf>>>;

pub struct JsonLd {
    id: String,
    loader: Loader,
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
    pub fn new(id: String, loader: Arc<Mutex<ReqwestLoader<IriBuf>>>) -> Self {
        let parents = parent::system(Spanned(Json::Token(JsonToken::Null), 0..0));

        Self {
            id,
            loader,
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
    type Token = JsonToken;

    type TokenError = Simple<char>;

    type Element = Json;

    type ElementError = Simple<JsonToken>;

    type Node = JsonNode;

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
            .filter(|(ref x, _)| x.value().starts_with("@"))
            .for_each(|(x, _)| apply(x.span().clone(), SemanticTokenType::KEYWORD));
    }
}

#[async_trait::async_trait]
impl LangState for JsonLd {
    async fn update(&mut self, parents: ParentingSystem<Spanned<<JsonLd as Lang>::Node>>) {
        self.parents = parents;
        if let Ok(bytes) = self.parents.to_json_vec() {
            self.loader
                .lock()
                .await
                .set_document(IriBuf::from_str(&self.id).unwrap(), bytes);
        }
    }

    async fn do_semantic_tokens(&mut self) -> Vec<SemanticToken> {
        semantic_tokens(self, &self.parents, &self.rope)
    }

    async fn do_completion(&mut self, trigger: Option<String>) -> Vec<super::SimpleCompletion> {
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
            let iri = iref::IriBuf::from_str(&self.id).unwrap();
            let mut loader = self.loader.lock().await;
            let doc = loader.load_context(iri.clone()).await.unwrap();
            let mut x = doc.into_document();

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
                let doc = loader.load_context(resolved).await.unwrap();
                let y = doc.into_document().0;
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
                    filter_text: None,
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
