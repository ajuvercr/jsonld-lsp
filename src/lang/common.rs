use std::{fmt::Display, hash::Hash, ops::Range};

use chumsky::prelude::Simple;
use lsp_types::{CompletionItemKind, FormattingOptions, SemanticToken, SemanticTokenType};

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

pub trait Token: Sized {
    fn token(&self) -> Option<SemanticTokenType>;

    fn span_tokens(Spanned(this, span): &Spanned<Self>) -> Vec<(SemanticTokenType, Range<usize>)> {
        if let Some(x) = this.token() {
            vec![(x, span.clone())]
        } else {
            Vec::new()
        }
    }
}

pub trait Node<T> {
    fn leaf<'a>(&'a self) -> Option<&'a T>;
}

impl<T> Node<T> for () {
    fn leaf<'a>(&'a self) -> Option<&'a T> {
        None
    }
}

pub trait Lang {
    type State: Clone;

    /// Type of tokens after tokenization
    type Token: PartialEq + Hash + Clone + Send + Sync;
    type TokenError: Into<SimpleDiagnostic> + Send + Sync;

    /// Type of Element inside a ParentingSystem
    type Element: Send + Sync;
    type ElementError: Into<SimpleDiagnostic> + Send + Sync;

    type RenameError: Send + Sync;
    type PrepareRenameError: Send + Sync;

    type Node: Node<Self::NodeLeaf> + Send + Sync;
    type NodeLeaf: Send + Sync + Token;

    const LANG: &'static str;
    const TRIGGERS: &'static [&'static str];
    const LEGEND_TYPES: &'static [SemanticTokenType];

    fn format(&mut self, options: FormattingOptions) -> Option<String> {
        None
    }

    fn tokenize(&mut self, source: &str) -> (Vec<Spanned<Self::Token>>, Vec<Self::TokenError>);
    fn parse(
        &self,
        source: &str,
        tokens: &Vec<Spanned<Self::Token>>,
    ) -> (Spanned<Self::Element>, Vec<Self::ElementError>);
    fn parents(&self, element: &Spanned<Self::Element>) -> ParentingSystem<Spanned<Self::Node>>;

    fn special_semantic_tokens(&self, apply: impl FnMut(Range<usize>, SemanticTokenType) -> ());

    fn prepare_rename(
        &self,
        pos: usize,
    ) -> Result<(std::ops::Range<usize>, String), Self::PrepareRenameError>;

    fn rename(
        &self,
        pos: usize,
        new_name: String,
    ) -> Result<Vec<(std::ops::Range<usize>, String)>, Self::RenameError>;

    fn new(id: String, state: Self::State) -> Self;
}

#[async_trait::async_trait]
pub trait LangState: Lang
where
    Self: Sized,
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
