use std::{hash::Hash, ops::Range};

use lsp_types::{SemanticToken, SemanticTokenType};

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

#[async_trait::async_trait]
pub trait LangState: Lang
where
    Self: Sized,
{
    async fn update(&mut self, parents: ParentingSystem<Spanned<Self::Node>>);

    async fn do_semantic_tokens(&mut self) -> Vec<SemanticToken>;

    async fn update_text(
        &mut self,
        source: &str,
    ) -> Vec<Result<Self::TokenError, Self::ElementError>> {
        let (tokens, errors) = self.tokenize(source);

        if !errors.is_empty() {
            return errors.into_iter().map(Result::Ok).collect();
        }

        let (elements, errors) = self.parse(source, &tokens);

        let parenting = self.parents(&elements);

        self.update(parenting);

        errors.into_iter().map(Result::Err).collect()
    }
}
