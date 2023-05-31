use std::{hash::Hash, ops::Range};

use dashmap::DashMap;
use lsp_types::SemanticTokenType;

use crate::{model::Spanned, parent::ParentingSystem};

pub trait Token {
    fn token(&self) -> Option<SemanticTokenType>;
}

pub trait Node<T> {
    fn leaf<'a>(&'a self) -> Option<&'a T>;
}

pub trait Lang {
    /// Type of tokens after tokenization
    type Token: PartialEq + Hash + Token + Clone;
    type TokenError;

    /// Type of Element inside a ParentingSystem
    type Element;
    type ElementError;

    type Node: Node<Self::Token>;

    fn tokenize(&self, source: &str) -> (Vec<Spanned<Self::Token>>, Vec<Self::TokenError>);
    fn parse(
        &self,
        source: &str,
        tokens: &Vec<Spanned<Self::Token>>,
    ) -> (Spanned<Self::Element>, Vec<Self::ElementError>);
    fn parents(&self, element: &Spanned<Self::Element>) -> ParentingSystem<Spanned<Self::Node>>;

    fn semantic_tokens(
        &self,
        system: &ParentingSystem<Spanned<Self::Node>>,
        apply: impl FnMut(Range<usize>, SemanticTokenType) -> (),
    );
}

pub trait LangState<L: Lang>
where
    Self: Sized,
{
    fn lang(&self) -> &L;
    fn update(
        &mut self,
        id: String,
        parents: ParentingSystem<L::Node>,
        others: DashMap<String, Self>,
    );
}
