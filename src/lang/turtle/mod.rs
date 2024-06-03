mod completion;
mod formatter;
mod green;
mod model;
mod node;
// mod parser;
mod parser2;
pub use parser2::parse_turtle;
pub mod shacl;
mod token;
pub mod tokenizer;
use futures::FutureExt;
use lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionResponse, CompletionItemKind,
    DiagnosticSeverity, FormattingOptions, Hover, MessageType, Position, SemanticTokenType,
};
use std::{collections::HashSet, ops::Range};
use tracing::info;

use chumsky::{prelude::Simple, Parser};
pub use model::*;

pub use parser2::*;
use ropey::Rope;

use crate::{
    backend::Client,
    lang::{
        self, head,
        turtle::completion::{NextTokenCompletionCtx, NsCompletionCtx},
    },
    model::{spanned, Spanned},
    parent::ParentingSystem,
    prefix::Prefixes,
    semantics::semantic_tokens,
    utils::{position_to_offset, range_to_range},
};

use self::{
    completion::{ArcedNamespaceCompletionProvider, CompletionProvider},
    formatter::format_turtle,
    node::{Leaf, Node},
    token::Token,
};

use super::{
    CurrentLangState, CurrentLangStatePart, DiagnosticSender, Lang, LangState, SimpleCompletion,
    SimpleDiagnostic,
};

pub mod semantic_token {
    use lsp_types::SemanticTokenType as STT;
    pub const BOOLEAN: STT = STT::new("boolean");
    pub const LANG_TAG: STT = STT::new("langTag");
}

#[allow(unused)]
pub struct TurtleLang {
    id: lsp_types::Url,
    rope: Rope,
    comments: Vec<Spanned<String>>,
    defined_prefixes: HashSet<String>,
    prefixes: Prefixes,
    namespace_completion_provider: ArcedNamespaceCompletionProvider,
}

impl Lang for TurtleLang {
    type State = (Prefixes, ArcedNamespaceCompletionProvider);

    type Token = token::Token;

    type TokenError = Simple<char>;

    type Element = model::Turtle;

    type ElementError = (usize, Simple<token::Token>);

    type RenameError = ();

    type PrepareRenameError = ();

    type Node = Node;
    type NodeLeaf = Leaf;

    const LANG: &'static str = "turtle";

    const TRIGGERS: &'static [&'static str] = &[":"];
    const CODE_ACTION: bool = true;
    const HOVER: bool = true;

    const LEGEND_TYPES: &'static [lsp_types::SemanticTokenType] = &[
        semantic_token::BOOLEAN,
        semantic_token::LANG_TAG,
        SemanticTokenType::COMMENT,
        SemanticTokenType::ENUM_MEMBER,
        SemanticTokenType::ENUM,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::NAMESPACE,
        SemanticTokenType::NUMBER,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::STRING,
        SemanticTokenType::VARIABLE,
    ];

    fn pattern() -> Option<String> {
        None
    }

    fn format(
        &mut self,
        state: &CurrentLangState<Self>,
        options: FormattingOptions,
    ) -> Option<String> {
        if state.element.last_is_valid() {
            format_turtle(
                &state.element.last_valid,
                options,
                &self.comments,
                &self.rope,
            )
        } else {
            None
        }
    }

    fn tokenize(&mut self, source: &str) -> (Vec<Spanned<Self::Token>>, Vec<Self::TokenError>) {
        self.rope = Rope::from(source.to_owned());
        let parser = tokenizer::parse_tokens();

        let (json, errs) = parser.parse_recovery(source);
        let tokens: Vec<_> = json.unwrap_or_default().into_iter().collect();

        (tokens, errs)
    }

    fn parse(
        &mut self,
        source: &str,
        tokens: &Vec<Spanned<Self::Token>>,
    ) -> (Spanned<Self::Element>, Vec<Self::ElementError>) {
        let tokens = tokens.clone();
        let mut comments: Vec<_> = tokens
            .iter()
            .filter(|x| x.is_comment())
            .cloned()
            .map(|x| x.map(|x| x.to_comment()))
            .collect();
        comments.sort_by_key(|x| x.1.start);
        self.comments = comments;

        parse_turtle(&self.id, tokens, source.len())
    }

    fn code_action(
        &self,
        state: &CurrentLangState<Self>,
        pos: Range<usize>,
        uri: &lsp_types::Url,
    ) -> Option<CodeActionResponse> {
        let mut seen = HashSet::new();
        let mut actions: Vec<(String, Vec<lsp_types::TextEdit>)> = self
            .get_undefined_prefixes(
                state,
                move |span, prefix| {
                    if pos.contains(&span.start)
                        || pos.contains(&span.end)
                        || (pos.start > span.start && pos.end < span.end)
                    {
                        if !seen.contains(prefix) {
                            seen.insert(prefix.to_string());
                            return true;
                        }
                    }
                    false
                },
                |x| x,
            )
            .into_iter()
            .filter_map(|x| {
                let target = self.prefixes.get(&x.prefix)?;

                let edits = vec![lsp_types::TextEdit {
                    new_text: format!("@prefix {}: <{}>.\n", x.prefix, target),
                    range: head(),
                }];
                Some((format!("Add '{}' prefix", x.prefix), edits))
            })
            .collect();

        if actions.len() > 1 {
            actions.insert(
                0,
                (
                    "Fix all prefixes".into(),
                    actions.iter().map(|(_, e)| e.clone()).flatten().collect(),
                ),
            );
        }

        let actions = actions
            .into_iter()
            .map(|(title, edits)| CodeAction {
                title,
                kind: Some(CodeActionKind::QUICKFIX),
                edit: Some(lsp_types::WorkspaceEdit {
                    changes: Some([(uri.clone(), edits)].into()),
                    document_changes: None,
                    change_annotations: None,
                }),
                ..Default::default()
            })
            .map(CodeActionOrCommand::CodeAction)
            .collect();

        Some(actions)
    }

    fn parents(&self, element: &Spanned<Self::Element>) -> ParentingSystem<Spanned<Self::Node>> {
        ParentingSystem::new_turtle(element.clone())
    }

    fn special_semantic_tokens(
        &self,
        state: &CurrentLangState<Self>,
        mut apply: impl FnMut(Range<usize>, lsp_types::SemanticTokenType) -> (),
    ) {
        use lang::Token;
        state.tokens.current.iter().for_each(|token| {
            Token::span_tokens(token)
                .into_iter()
                .for_each(|(token, span)| apply(span, token))
        });
    }

    fn prepare_rename(
        &self,
        _state: &CurrentLangState<Self>,
        _pos: usize,
    ) -> Result<(Range<usize>, String), Self::PrepareRenameError> {
        Err(())
    }

    fn rename(
        &self,
        _state: &CurrentLangState<Self>,
        _pos: usize,
        _new_name: String,
    ) -> Result<Vec<(Range<usize>, String)>, Self::RenameError> {
        Err(())
    }

    fn new(id: String, state: Self::State) -> (Self, CurrentLangState<Self>) {
        let url = lsp_types::Url::parse(&id).unwrap();
        (
            TurtleLang {
                id: url.clone(),
                rope: Rope::new(),
                comments: Vec::new(),
                defined_prefixes: HashSet::new(),
                prefixes: state.0.clone(),
                namespace_completion_provider: state.1.clone(),
            },
            CurrentLangState {
                element: CurrentLangStatePart::new(spanned(Turtle::empty(&url), 0..1)),
                tokens: CurrentLangStatePart::new(Default::default()),
                parents: CurrentLangStatePart::new(Default::default()),
            },
        )
    }
}

impl TurtleLang {
    fn prefix_to_completion(
        &self,
        key: &str,
        state: &CurrentLangState<Self>,
        range: lsp_types::Range,
    ) -> SimpleCompletion {
        let url = self
            .prefixes
            .get(key)
            .map(|x| x.to_string())
            .unwrap_or_default();

        let mut edits = vec![lsp_types::TextEdit {
            new_text: format!("{}:", key),
            range,
        }];

        if state
            .element
            .last_valid
            .prefixes
            .iter()
            .find(|x| x.prefix.as_str() == key)
            .is_none()
        {
            edits.push(lsp_types::TextEdit {
                new_text: format!("@prefix {}: <{}>.\n", key, url),
                range: head(),
            });
        }

        SimpleCompletion {
            kind: CompletionItemKind::MODULE,
            label: format!("{}", key),
            documentation: Some(url.clone()),
            sort_text: None,
            filter_text: None,
            edits,
        }
    }

    fn get_undefined_prefixes<O, F: Fn(UndefinedPrefix) -> O>(
        &self,
        state: &CurrentLangState<Self>,
        mut contains: impl FnMut(&Range<usize>, &str) -> bool,
        f: F,
    ) -> Vec<O> {
        state
            .tokens
            .current
            .iter()
            .filter_map(|x| match &x.0 {
                Token::PNameLN(s, _) => {
                    let prefix = s.as_deref().unwrap_or("");
                    if contains(x.span(), prefix)
                        && !self.defined_prefixes.contains(s.as_deref().unwrap_or(""))
                    {
                        let prefix = prefix.to_string();
                        Some(UndefinedPrefix {
                            prefix,
                            range: x.span().clone(),
                        })
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .map(f)
            .collect()
    }
}

struct UndefinedPrefix {
    prefix: String,
    range: Range<usize>,
}

#[async_trait::async_trait]
impl<C: Client + Send + Sync + 'static> LangState<C> for TurtleLang {
    async fn hover(&mut self, _state: &CurrentLangState<Self>, _pos: usize) -> Option<Hover> {
        None
    }

    async fn post_update_diagnostics(
        &self,
        state: &CurrentLangState<Self>,
        sender: DiagnosticSender,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send>> {
        let undefined_prefixes = self.get_undefined_prefixes(
            state,
            |_, _| true,
            |UndefinedPrefix { prefix, range }| {
                SimpleDiagnostic::new_severity(
                    range,
                    format!("Prefix {} not defined", prefix),
                    DiagnosticSeverity::HINT,
                )
            },
        );

        sender.push_all(undefined_prefixes);

        let turtle = &state.element.current;
        info!("Updating prefixes for {}", self.id);
        let fut1 = self.namespace_completion_provider.update(turtle).await;

        fut1.boxed()
    }

    async fn update(&mut self, state: &CurrentLangState<Self>, client: &C) {
        let last_valid_triples = state
            .element
            .last_valid
            .get_simple_triples()
            .map(|x| format!("last valid {} triples", x.triples.len()))
            .unwrap_or_else(|e| String::from(format!("Last valid does not have triples {:?}", e)));

        let current_triples = state
            .element
            .current
            .get_simple_triples()
            .map(|x| format!("current {} triples", x.triples.len()))
            .unwrap_or_else(|e| String::from(format!("current does not have triples {:?}", e)));

        client
            .log_message(
                MessageType::INFO,
                format!("{} / {}", current_triples, last_valid_triples),
            )
            .await;

        info!("{} / {}", current_triples, last_valid_triples);

        if state.element.last_is_valid() {
            self.defined_prefixes = HashSet::new();
            for pref in &state.element.current.prefixes {
                self.defined_prefixes.insert(pref.prefix.0.clone());
            }
        }
    }

    async fn do_semantic_tokens(
        &mut self,
        state: &CurrentLangState<Self>,
    ) -> Vec<lsp_types::SemanticToken> {
        semantic_tokens(self, state, &self.rope)
    }

    async fn do_completion(
        &mut self,
        _trigger: Option<String>,
        position: &Position,
        state: &CurrentLangState<Self>,
        _client: &C,
    ) -> Vec<super::SimpleCompletion> {
        let mut completions = Vec::new();

        let location =
            position_to_offset(position.clone(), &self.rope).unwrap_or(self.rope.len_chars()) - 1;

        info!("Completion on location {}", location);

        let current_token_idx = state
            .tokens
            .current
            .iter()
            .enumerate()
            .filter(|(_, x)| x.span().end > location)
            .min_by_key(|(_, x)| x.span().end)
            .map(|x| x.0)
            .or((!state.tokens.current.is_empty()).then_some(state.tokens.current.len() - 1));

        if let Some(idx) = current_token_idx {
            info!("Current token {}", state.tokens.current[idx].value());
            if idx > 0 {
                info!("Previous token {}", state.tokens.current[idx - 1].value());
            }
        }

        let range = current_token_idx
            .iter()
            .filter(|&x| state.tokens.current[*x].span().contains(&location))
            .map(|x| range_to_range(&state.tokens.current[*x].1, &self.rope).unwrap_or_default())
            .next()
            .unwrap_or(lsp_types::Range::new(*position, *position));

        let triples = state
            .element
            .last_valid
            .get_simple_triples()
            .unwrap_or_default();

        completions.extend(
            self.namespace_completion_provider
                .find_completions(
                    &NsCompletionCtx {
                        turtle: &state.element.current,
                        triples: &triples,
                        location,
                    },
                    range,
                )
                .await,
        );

        info!(
            "Current token {:?}",
            current_token_idx
                .as_ref()
                .map(|idx| &state.tokens.current[*idx])
        );

        if let Some(token_idx) = current_token_idx {
            if token_idx > 0 {
                let ctx = NextTokenCompletionCtx {
                    turtle: &state.element.last_valid,
                    triples: &triples,
                    location,
                    prev_token: &state.tokens.current[token_idx - 1],
                    current_token: &state.tokens.current[token_idx],
                };

                completions.extend(
                    self.namespace_completion_provider
                        .find_completions(&ctx, range)
                        .await,
                );
            }

            let token = &state.tokens.current[token_idx];
            info!("For token {:?}", token);
            // let range = range_to_range(token.span(), &self.rope).unwrap_or_default();

            if let Spanned(Token::Invalid(_), _) = &token {
                completions.extend(
                    self.prefixes
                        .get_all()
                        .map(|key| self.prefix_to_completion(key, state, range)),
                );
            }
        }

        info!("Returning {} suggestions", completions.len());
        completions
    }
}
