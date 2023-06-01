use std::{fmt::Display, hash::Hash, ops::Range};

use chumsky::prelude::Simple;
use lsp_types::{CompletionItemKind, SemanticToken, SemanticTokenType};

use crate::{model::Spanned, parent::ParentingSystem};

pub struct SimpleDiagnostic {
    pub range: Range<usize>,
    pub msg: String,
}

#[derive(Debug)]
pub struct SimpleCompletion {
    pub kind: CompletionItemKind,
    pub label: String,
    pub documentation: Option<String>,
    pub sort_text: Option<String>,
    pub filter_text: Option<String>,
    pub edit: String,
}

impl<T: Display + Eq + Hash> From<Simple<T>> for SimpleDiagnostic {
    fn from(e: Simple<T>) -> Self {
        let msg = if let chumsky::error::SimpleReason::Custom(msg) = e.reason() {
            msg.clone()
        } else {
            format!(
                "{}{}, expected {}",
                if e.found().is_some() {
                    "Unexpected token"
                } else {
                    "Unexpected end of input"
                },
                if let Some(label) = e.label() {
                    format!(" while parsing {}", label)
                } else {
                    String::new()
                },
                if e.expected().len() == 0 {
                    "something else".to_string()
                } else {
                    e.expected()
                        .map(|expected| match expected {
                            Some(expected) => format!("'{}'", expected),
                            None => "end of input".to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(" or ")
                },
            )
        };

        SimpleDiagnostic {
            range: e.span(),
            msg,
        }
    }
}

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

    fn tokenize(&mut self, source: &str) -> (Vec<Spanned<Self::Token>>, Vec<Self::TokenError>);
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
    Spanned<<Self as Lang>::Element>: Send,
    Spanned<<Self as Lang>::Node>: Send,
    Spanned<<Self as Lang>::Token>: Send,
    <Self as Lang>::TokenError: Into<SimpleDiagnostic> + Send,
    <Self as Lang>::ElementError: Into<SimpleDiagnostic> + Send,
{
    async fn update(&mut self, parents: ParentingSystem<Spanned<Self::Node>>);

    async fn do_semantic_tokens(&mut self) -> Vec<SemanticToken>;

    async fn update_text(&mut self, source: &str) -> Vec<SimpleDiagnostic> {
        let (tokens, errors) = self.tokenize(source);

        if !errors.is_empty() {
            return errors.into_iter().map(|x| x.into()).collect();
        }

        let (elements, errors) = self.parse(source, &tokens);

        if !errors.is_empty() {
            return errors.into_iter().map(|x| x.into()).collect();
        }
        let parenting = self.parents(&elements);

        self.update(parenting).await;

        Vec::new()
    }

    async fn do_completion(&mut self, trigger: Option<String>) -> Vec<SimpleCompletion>;
}
