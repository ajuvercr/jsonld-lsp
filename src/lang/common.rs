use std::{fmt::Display, hash::Hash, ops::Range, sync::Arc};

use chumsky::prelude::Simple;
use lsp_types::{
    CompletionItemKind, FormattingOptions, Position, SemanticToken, SemanticTokenType,
};
use tracing::debug;

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

#[derive(Clone, Debug)]
pub struct CurrentLangStatePart<E> {
    pub last_valid: Arc<E>,
    pub current: Arc<E>,
}
impl<E: Default> Default for CurrentLangStatePart<E> {
    fn default() -> Self {
        let current: Arc<E> = Default::default();
        Self {
            last_valid: current.clone(),
            current,
        }
    }
}

impl<T> CurrentLangStatePart<T> {
    pub fn last_is_valid(&self) -> bool {
        Arc::ptr_eq(&self.last_valid, &self.current)
    }
}

#[derive(Debug)]
pub struct CurrentLangState<L: Lang> {
    pub tokens: CurrentLangStatePart<Vec<Spanned<L::Token>>>,
    pub element: CurrentLangStatePart<Spanned<L::Element>>,
    pub parents: CurrentLangStatePart<ParentingSystem<Spanned<L::Node>>>,
}

impl<L: Lang> Default for CurrentLangState<L>
where
    L::Element: Default,
{
    fn default() -> Self {
        Self {
            tokens: Default::default(),
            element: Default::default(),
            parents: Default::default(),
        }
    }
}

pub trait Lang: Sized {
    type State: Clone;

    /// Type of tokens after tokenization
    type Token: PartialEq + Hash + Clone + Send + Sync;
    type TokenError: Into<SimpleDiagnostic> + Send + Sync + std::fmt::Debug;

    /// Type of Element inside a ParentingSystem
    type Element: Send + Sync;
    type ElementError: Into<SimpleDiagnostic> + Send + Sync + std::fmt::Debug;

    type RenameError: Send + Sync;
    type PrepareRenameError: Send + Sync;

    type Node: Node<Self::NodeLeaf> + Send + Sync;
    type NodeLeaf: Send + Sync + Token;

    const LANG: &'static str;
    const TRIGGERS: &'static [&'static str];
    const LEGEND_TYPES: &'static [SemanticTokenType];

    fn pattern() -> Option<String> {
        None
    }

    fn format(
        &mut self,
        _state: &CurrentLangState<Self>,
        _options: FormattingOptions,
    ) -> Option<String> {
        None
    }

    fn tokenize(&mut self, source: &str) -> (Vec<Spanned<Self::Token>>, Vec<Self::TokenError>);
    fn parse(
        &mut self,
        source: &str,
        tokens: &Vec<Spanned<Self::Token>>,
    ) -> (Spanned<Self::Element>, Vec<Self::ElementError>);
    fn parents(&self, element: &Spanned<Self::Element>) -> ParentingSystem<Spanned<Self::Node>>;

    fn special_semantic_tokens(
        &self,
        state: &CurrentLangState<Self>,
        apply: impl FnMut(Range<usize>, SemanticTokenType) -> (),
    );

    fn prepare_rename(
        &self,
        state: &CurrentLangState<Self>,
        pos: usize,
    ) -> Result<(std::ops::Range<usize>, String), Self::PrepareRenameError>;

    fn rename(
        &self,
        state: &CurrentLangState<Self>,
        pos: usize,
        new_name: String,
    ) -> Result<Vec<(std::ops::Range<usize>, String)>, Self::RenameError>;

    fn new(id: String, state: Self::State) -> (Self, CurrentLangState<Self>);
}

#[async_trait::async_trait]
pub trait LangState: Lang
where
    Self: Sized,
{
    async fn update(&mut self, parents: &CurrentLangState<Self>);

    async fn do_semantic_tokens(&mut self, state: &CurrentLangState<Self>) -> Vec<SemanticToken>;

    async fn update_text(
        &mut self,
        source: &str,
        state: &mut CurrentLangState<Self>,
    ) -> Vec<SimpleDiagnostic> {
        let (tokens, token_errors) = self.tokenize(source);
        debug!( token_errors = ?token_errors);

        let tokens = Arc::new(tokens);

        state.tokens.current = tokens.clone();
        let (elements, parse_errors) = self.parse(source, &tokens);
        if token_errors.is_empty() {
            state.tokens.last_valid = tokens;
        }

        debug!( parse_errors = ?parse_errors);

        let elements = Arc::new(elements);
        state.element.current = elements.clone();
        let parenting = self.parents(&elements);
        if token_errors.is_empty() && parse_errors.is_empty() {
            state.element.last_valid = elements;
        }

        let parenting = Arc::new(parenting);

        state.parents.current = parenting.clone();
        if token_errors.is_empty() && parse_errors.is_empty() {
            state.parents.last_valid = parenting.clone();
        }

        self.update(&state).await;

        token_errors
            .into_iter()
            .map(|x| x.into())
            .chain(parse_errors.into_iter().map(|x| x.into()))
            .collect()
    }

    async fn do_completion(
        &mut self,
        trigger: Option<String>,
        position: &Position,
        state: &CurrentLangState<Self>,
    ) -> Vec<SimpleCompletion>;
}
