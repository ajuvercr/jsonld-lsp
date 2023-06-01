use core::fmt;
use std::{
    ops::{DerefMut, Range},
    str::FromStr,
    sync::Arc,
};

use chumsky::prelude::Simple;
use iref::IriBuf;
use json_ld::{ContextLoader, Expand, Loader as _};
use lsp_types::{CompletionItemKind, SemanticToken, SemanticTokenType};
use ropey::Rope;
use tokio::sync::Mutex;

use crate::{
    contexts::filter_definition,
    model::Spanned,
    parent::ParentingSystem,
    semantics::semantic_tokens,
    utils::{log, ReqwestLoader},
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

        log(format!("parents {:?}", parents));

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
            .filter(|(_, x)| x.is_kv())
            .map(|(_, x)| x.to_kv())
            .filter(|(ref x, _)| x.value() == "@id")
            .map(|(_, id)| &system[id])
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
        log(format!("parents {:?}", parents));

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
        use json_ld::ExtractContext;
        if trigger == Some("@".to_string()) {
            self.get_ids()
                .into_iter()
                .map(|x| {
                    let w = Some(format!("@{x}"));

                    SimpleCompletion {
                        documentation: Some(format!("Subject: {x}")),
                        edit: format!("{{\"@id\": \"{x}\" }}"),
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
            let doc = loader.load(iri).await.unwrap();
            doc.expand(loader.deref_mut()).await.unwrap();

            <json_ld_syntax::Value<locspan::Location<IriBuf>>>::extract_context(doc.into_document())
                .map(|x| {
                    x.traverse()
                        .filter_map(filter_definition)
                        .map(|v| SimpleCompletion {
                            kind: CompletionItemKind::PROPERTY,
                            label: v.key.to_string(),
                            documentation: Some(format!("\"{}\"", v.value)),
                            sort_text: None,
                            filter_text: None,
                            edit: format!("\"{}\"", v.value),
                        })
                        .collect()
                })
                .unwrap_or_default()
        }
    }
}

impl JsonLd {
    fn get_ids(&self) -> Vec<String> {
        let id_indices = self
            .parents
            .iter()
            .map(|(_, x)| x)
            .filter(|x| x.is_kv())
            .map(|x| x.to_kv())
            .filter(|(id, _)| id.value() == "@id")
            .map(|(_, idx)| idx);

        let ids = id_indices
            .map(|x| &self.parents[x].0)
            .filter(|x| x.is_leaf())
            .map(|x| x.to_leaf())
            .filter(|x| x.is_string())
            .map(|x| x.into_string());

        ids.collect()
    }
}
