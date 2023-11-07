mod formatter;
mod model;
mod node;
mod parser;
mod token;
pub mod tokenizer;
use futures::{stream::FuturesUnordered, FutureExt, StreamExt};
use lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionResponse, CompletionItemKind,
    DiagnosticSeverity, FormattingOptions, MessageType, Position, SemanticTokenType,
};
use std::{collections::HashSet, ops::Range};
use tracing::info;

use chumsky::{prelude::Simple, primitive::end, recovery::skip_then_retry_until, Parser};
pub use model::*;

pub use parser::*;
use ropey::Rope;

use crate::{
    backend::Client,
    lang::{self, head},
    model::{spanned, Spanned},
    parent::ParentingSystem,
    prefix::{Prefixes, Property},
    semantics::semantic_tokens,
    utils::{position_to_offset, range_to_range},
};

use self::{
    formatter::format_turtle,
    node::{Leaf, Node},
    token::Token,
};

use super::{
    CurrentLangState, DiagnosticSender, Lang, LangState, SimpleCompletion, SimpleDiagnostic,
};

pub mod semantic_token {
    use lsp_types::SemanticTokenType as STT;
    pub const BOOLEAN: STT = STT::new("boolean");
    pub const LANG_TAG: STT = STT::new("langTag");
}

#[allow(unused)]
pub struct TurtleLang {
    id: String,
    rope: Rope,
    comments: Vec<Spanned<String>>,
    defined_prefixes: HashSet<String>,
    prefixes: Prefixes,
}

impl Lang for TurtleLang {
    type State = Prefixes;

    type Token = token::Token;

    type TokenError = Simple<char>;

    type Element = model::Turtle;

    type ElementError = Simple<token::Token>;

    type RenameError = ();

    type PrepareRenameError = ();

    type Node = Node;
    type NodeLeaf = Leaf;

    const LANG: &'static str = "turtle";

    const TRIGGERS: &'static [&'static str] = &[":"];

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
        let stream = chumsky::Stream::from_iter(
            0..self.rope.len_chars() + 1,
            state
                .tokens
                .current
                .iter()
                .filter(|x| !x.is_comment())
                .map(|Spanned(x, s)| (x.clone(), s.clone())),
        );

        let parser = turtle()
            .map_with_span(spanned)
            .then_ignore(end().recover_with(skip_then_retry_until([])));

        let turtle = parser.parse(stream).ok()?;
        format_turtle(&turtle.0, options, &self.comments, &self.rope)
    }

    fn tokenize(&mut self, source: &str) -> (Vec<Spanned<Self::Token>>, Vec<Self::TokenError>) {
        self.rope = Rope::from(source.to_owned());
        let parser = tokenizer::parse_tokens();

        let (json, errs) = parser.parse_recovery(source);

        let tokens: Vec<_> = json
            .unwrap_or_default()
            .into_iter()
            .map(|(x, y)| spanned(x, y))
            .collect();

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

        let stream = chumsky::Stream::from_iter(
            0..source.len() + 1,
            tokens
                .into_iter()
                .filter(|x| !x.is_comment())
                .map(|Spanned(x, s)| (x, s)),
        );

        let parser = turtle()
            .map_with_span(spanned)
            .then_ignore(end().recover_with(skip_then_retry_until([])));

        let (mut json, json_errors) = parser.parse_recovery(stream);
        if let Some(ref mut ttl) = &mut json {
            info!("triples {}", ttl.triples.len());
            ttl.0.set_base(&self.id);
        }

        (
            json.unwrap_or(Spanned(Default::default(), 0..source.len())),
            json_errors,
        )
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

    fn parents(&self, _element: &Spanned<Self::Element>) -> ParentingSystem<Spanned<Self::Node>> {
        ParentingSystem::new()
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
        (
            TurtleLang {
                id: format!("{}", id),
                rope: Rope::new(),
                comments: Vec::new(),
                defined_prefixes: HashSet::new(),
                prefixes: state.clone(),
            },
            Default::default(),
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

    fn property_to_completion(
        &self,
        prop: &Property,
        prefix: &str,
        prefix_url: &str,
        range: lsp_types::Range,
    ) -> SimpleCompletion {
        let short = prop.short(prefix_url);
        let new_text = format!("{}:{}", prefix, short);
        let edits = vec![lsp_types::TextEdit {
            new_text,
            range: range.clone(),
        }];

        let label = match (&prop.comment, &prop.label) {
            (Some(comment), Some(label)) => {
                format!("{}: {}", label, comment)
            }
            (None, Some(label)) => label.clone(),
            (Some(comment), None) => format!("{}: {}", short, comment),
            (None, None) => short.clone(),
        };

        let documentation = Some(prop.id.clone());

        SimpleCompletion {
            kind: prop.ty.into(),
            label,
            documentation,
            sort_text: Some(format!("{}:{}", prefix, short)),
            filter_text: Some(format!("{}:{}", prefix, short)),
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
                    DiagnosticSeverity::ERROR,
                )
            },
        );

        sender.push_all(undefined_prefixes);

        let prefixes = self.prefixes.clone();
        let turtle = &state.element.current;
        let prefixes_ids: Vec<_> = turtle
            .prefixes
            .iter()
            .flat_map(|Spanned(ref prefix, ref span)| {
                prefix
                    .value
                    .expand(turtle, &self.id)
                    .map(|pref| Spanned(pref, span.clone()))
            })
            .collect();

        if state.element.last_is_valid() {
            let base = turtle.get_base(&self.id);
            info!("Updating prefixes for {} and {}", base, self.id);
            prefixes
                .update(&self.id, turtle, Some(&format!("{}#", base)), |_| {})
                .await;
        }

        async move {
            // Rust please
            let sender = &sender;
            let prefixes = &prefixes;

            prefixes_ids
                .into_iter()
                .map(|prefix| async move {
                    if let Err(e) = prefixes
                        .fetch(&prefix, |_| {}, |_msg| async {}.boxed())
                        .await
                    {
                        info!("sending diagnostic, {} ", prefix.value());
                        sender.push(SimpleDiagnostic::new_severity(
                            prefix.span().clone(),
                            format!("Failed to fetch prefix: {}", e),
                            DiagnosticSeverity::WARNING,
                        ));
                    }
                })
                .collect::<FuturesUnordered<_>>()
                .count()
                .await;
        }
        .boxed()
    }

    async fn update(&mut self, state: &CurrentLangState<Self>) {
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
        client: &C,
    ) -> Vec<super::SimpleCompletion> {
        let location =
            position_to_offset(position.clone(), &self.rope).unwrap_or(self.rope.len_chars()) - 1;

        info!("Completion on location {}", location);
        if let Some(token) = state
            .tokens
            .current
            .iter()
            .find(|x| x.1.contains(&location))
        {
            info!("For token {:?}", token);
            if let Spanned(Token::Invalid(_), range) = &token {
                let range = range_to_range(range, &self.rope).unwrap_or_default();

                return self
                    .prefixes
                    .get_all()
                    .map(|key| self.prefix_to_completion(key, state, range))
                    .collect();
            }

            if let Spanned(Token::PNameLN(prefix, _), range) = &token {
                // Lets find the corresponding prefix thanks
                let prefix_str = prefix.as_ref().map(|x| x.as_str()).unwrap_or("");
                let turtle = &state.element.last_valid;

                let m_expanended = if let Some(prefix) = turtle
                    .prefixes
                    .iter()
                    .find(|x| x.prefix.as_str() == prefix_str)
                {
                    prefix.value.expand(turtle, &self.id)
                } else {
                    self.prefixes.get(prefix_str).map(String::from)
                };

                info!("m_expaned {:?}", m_expanended);

                let mut out = Vec::new();
                if let Some(expaned) = m_expanended {
                    let range = range_to_range(range, &self.rope).unwrap_or_default();

                    info!("Range {:?}", range);
                    let _ = self
                        .prefixes
                        .fetch(
                            &expaned,
                            |props| {
                                info!("Properties {}", props.len());

                                out.extend(props.iter().map(|prop| {
                                    self.property_to_completion(prop, prefix_str, &expaned, range)
                                }));
                            },
                            |msg| client.log_message(MessageType::INFO, msg),
                        )
                        .await;

                    info!("Returning {} suggestions", out.len());
                }
                return out;
            }
        }

        Vec::new()
    }
}
