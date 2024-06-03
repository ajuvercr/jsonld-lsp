use std::{fmt::Display, hash::Hash, ops::Range, sync::Arc};

use chumsky::prelude::Simple;
use futures::FutureExt;
use futures::{channel::mpsc, StreamExt};
use lsp_types::{
    CodeActionResponse, CompletionItem, CompletionItemKind, CompletionTextEdit, Diagnostic,
    DiagnosticSeverity, Documentation, FormattingOptions, Hover, InsertTextFormat, Position,
    SemanticToken, SemanticTokenType, TextEdit, Url,
};
use ropey::Rope;
use tracing::debug;

use crate::{backend::Client, model::Spanned, parent::ParentingSystem, utils::offset_to_position};

pub struct SimpleDiagnostic {
    pub range: Range<usize>,
    pub msg: String,
    pub severity: Option<DiagnosticSeverity>,
}

impl SimpleDiagnostic {
    pub fn new(range: Range<usize>, msg: String) -> Self {
        Self {
            range,
            msg,
            severity: None,
        }
    }

    pub fn new_severity(range: Range<usize>, msg: String, severity: DiagnosticSeverity) -> Self {
        Self {
            range,
            msg,
            severity: Some(severity),
        }
    }
}

pub fn head() -> lsp_types::Range {
    let start = lsp_types::Position {
        line: 0,
        character: 0,
    };
    lsp_types::Range {
        end: start.clone(),
        start,
    }
}

#[derive(Debug)]
pub struct SimpleCompletion {
    pub kind: CompletionItemKind,
    pub label: String,
    pub documentation: Option<String>,
    pub sort_text: Option<String>,
    pub filter_text: Option<String>,
    pub edits: Vec<TextEdit>,
}

impl Into<CompletionItem> for SimpleCompletion {
    fn into(self) -> CompletionItem {
        let SimpleCompletion {
            filter_text,
            sort_text,
            label,
            documentation,
            kind,
            edits,
        } = self;

        let text_edit = edits
            .iter()
            .next()
            .map(|x| CompletionTextEdit::Edit(x.clone()));

        let additional_text_edits = edits.into_iter().skip(1).collect();

        CompletionItem {
            label,
            kind: Some(kind),
            sort_text,
            insert_text_format: (kind == CompletionItemKind::SNIPPET)
                .then_some(InsertTextFormat::SNIPPET),
            filter_text,
            documentation: documentation.map(|st| Documentation::String(st)),
            text_edit,
            additional_text_edits: Some(additional_text_edits),
            ..Default::default()
        }
    }
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

        SimpleDiagnostic::new(e.span(), msg)
    }
}

impl<T: Display + Eq + Hash> From<(usize, Simple<T>)> for SimpleDiagnostic {
    fn from(this: (usize, Simple<T>)) -> Self {
        let (len, e) = this;
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

        let range = (len - e.span().end)..(len - e.span().start);
        SimpleDiagnostic::new(range, msg)
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

impl<E> CurrentLangStatePart<E> {
    pub fn new(this: E) -> Self {
        let this = Arc::new(this);
        Self {
            current: this.clone(),
            last_valid: this,
        }
    }
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

    const CODE_ACTION: bool;
    const HOVER: bool;
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

    fn code_action(
        &self,
        _state: &CurrentLangState<Self>,
        _pos: Range<usize>,
        _uri: &lsp_types::Url,
    ) -> Option<CodeActionResponse> {
        None
    }
}

#[async_trait::async_trait]
pub trait LangState<C: Client + Send + Sync + 'static>: Lang
where
    Self: Sized + Send + Sync,
{
    async fn update(&mut self, parents: &CurrentLangState<Self>);

    async fn do_semantic_tokens(&mut self, state: &CurrentLangState<Self>) -> Vec<SemanticToken>;

    async fn post_update_diagnostics(
        &self,
        _state: &CurrentLangState<Self>,
        _sender: DiagnosticSender,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send>> {
        async {}.boxed()
    }

    async fn hover(&mut self, state: &CurrentLangState<Self>, pos: usize) -> Option<Hover> {
        if let Some(el) = state
            .parents
            .current
            .iter()
            .filter(|x| x.1.span().contains(&pos))
            .min_by_key(|x| x.1.span().len())
        {
            match el.1.value() {
                _ => {}
            }
        }

        None
    }

    async fn update_text(
        &mut self,
        source: &str,
        state: &mut CurrentLangState<Self>,
        sender: DiagnosticSender,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send>> {
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

        sender.push_all(
            token_errors
                .into_iter()
                .map(|x| Into::<SimpleDiagnostic>::into(x))
                .chain(parse_errors.into_iter().map(|x| x.into()))
                .collect(),
        );

        self.post_update_diagnostics(state, sender).await
    }

    async fn do_completion(
        &mut self,
        trigger: Option<String>,
        position: &Position,
        state: &CurrentLangState<Self>,
        client: &C,
    ) -> Vec<SimpleCompletion>;
}

#[derive(Clone)]
pub struct DiagnosticSender {
    tx: mpsc::UnboundedSender<Vec<SimpleDiagnostic>>,
}
impl DiagnosticSender {
    pub fn push(&self, diagnostic: SimpleDiagnostic) -> Option<()> {
        let out = self.tx.unbounded_send(vec![diagnostic]).ok();
        out
    }

    pub fn push_all(&self, diagnostics: Vec<SimpleDiagnostic>) -> Option<()> {
        self.tx.unbounded_send(diagnostics).ok()
    }
}

pub struct Publisher<C: Client + Send + Sync + 'static> {
    version: i32,
    uri: Url,
    client: C,
    diagnostics: Vec<Diagnostic>,
    rope: Rope,
    rx: mpsc::UnboundedReceiver<Vec<SimpleDiagnostic>>,
}

impl<C: Client + Send + Sync + 'static> Publisher<C> {
    pub fn new(uri: Url, version: i32, client: C, rope: Rope) -> (Self, DiagnosticSender) {
        let (tx, rx) = mpsc::unbounded();
        (
            Self {
                version,
                uri,
                client,
                diagnostics: Vec::new(),
                rx,
                rope,
            },
            DiagnosticSender { tx },
        )
    }

    pub async fn spawn(mut self) {
        loop {
            if let Some(x) = self.rx.next().await {
                self.diagnostics.extend(x.into_iter().flat_map(|item| {
                    let (span, message) = (item.range, item.msg);
                    let start_position = offset_to_position(span.start, &self.rope)?;
                    let end_position = offset_to_position(span.end, &self.rope)?;
                    Some(Diagnostic {
                        range: lsp_types::Range::new(start_position, end_position),
                        message,
                        severity: item.severity,
                        ..Default::default()
                    })
                }));

                self.client
                    .publish_diagnostics(
                        self.uri.clone(),
                        self.diagnostics.clone(),
                        Some(self.version),
                    )
                    .await;
            } else {
                return;
            }
        }
    }
}
