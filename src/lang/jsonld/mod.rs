use std::ops::Range;

use chumsky::prelude::Simple;
use lsp_types::SemanticTokenType;

use crate::{model::Spanned, parent::ParentingSystem};

use self::{
    parent::JsonNode,
    parser::Json,
    tokenizer::{tokenize, JsonToken},
};

use super::{Lang, Node, LangState};

pub mod parent;
pub mod parser;
pub mod tokenizer;

pub struct JsonLd;

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

    fn tokenize(&self, source: &str) -> (Vec<Spanned<Self::Token>>, Vec<Self::TokenError>) {
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



