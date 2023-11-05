use crate::lang::{CurrentLangState, Lang, LangState, Publisher};
use crate::lsp_types::*;

use crate::utils::{offsets_to_range, position_to_offset};

use futures::future::join;
use futures::lock::Mutex;
use ropey::Rope;
use tracing::info;

use std::collections::HashMap;
use std::fmt::Display;
use std::sync::Arc;
use tower_lsp::jsonrpc::{Error, ErrorCode, Result};
use tower_lsp::LanguageServer;

#[tower_lsp::async_trait]
pub trait Client: Clone + ClientSync {
    async fn log_message<M: Display + Sync + Send + 'static>(&self, ty: MessageType, msg: M) -> ();
    async fn publish_diagnostics(
        &self,
        uri: Url,
        diags: Vec<Diagnostic>,
        version: Option<i32>,
    ) -> ();
}

pub trait ClientSync {
    fn spawn<O: Send + 'static, F: std::future::Future<Output = O> + Send + 'static>(&self, fut: F);
}

#[derive(Debug)]
pub struct Backend<C: Client + Send + Sync + 'static, L: LangState<C>> {
    pub client: C,

    cache: L::State,
    langs: Arc<Mutex<HashMap<String, (L, CurrentLangState<L>)>>>,
    ropes: Mutex<HashMap<String, Rope>>,
}

#[tower_lsp::async_trait]
impl<C: Client + Send + Sync + 'static, L: LangState<C> + Send + Sync + 'static> LanguageServer
    for Backend<C, L>
where
    <L as Lang>::State: Send + Sync + 'static,
    <L as Lang>::Token: Send + Sync + 'static,
    <L as Lang>::TokenError: Send + Sync + 'static,
    <L as Lang>::Node: Send + Sync + 'static,
    <L as Lang>::Element: Send + Sync + 'static,
    <L as Lang>::ElementError: Send + Sync + 'static,
{
    #[tracing::instrument(skip(self, _init))]
    async fn initialize(&self, _init: InitializeParams) -> Result<InitializeResult> {
        info!("Initialize");
        let triggers = L::TRIGGERS.iter().copied().map(String::from).collect();
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(triggers),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                document_formatting_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some(L::LANG.to_string()),
                                        scheme: Some("file".to_string()),
                                        pattern: L::pattern(),
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: L::LEGEND_TYPES.into(),
                                    token_modifiers: vec![],
                                },
                                range: Some(false),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),
                ..ServerCapabilities::default()
            },
        })
    }

    #[tracing::instrument(skip(self))]
    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let range = params.range;
        let uri = params.text_document.uri;

        let rope = {
            let ropes = self.ropes.lock().await;
            if let Some(rope) = ropes.get(uri.as_str()) {
                rope.clone()
            } else {
                return Ok(None);
            }
        };
        let mut langs = self.langs.lock().await;

        if let Some((ref mut lang, ref mut state)) = langs.get_mut(uri.as_str()) {
            let start = position_to_offset(range.start, &rope)
                .ok_or(Error::new(ErrorCode::InvalidRequest))?;
            let end = position_to_offset(range.end, &rope)
                .ok_or(Error::new(ErrorCode::InvalidRequest))?;
            if let Some(t) = lang.code_action(state, start..end, &uri) {
                return Ok(Some(t));
            }
        }

        Ok(None)
    }

    #[tracing::instrument(skip(self))]
    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        info!("formatting");
        let uri = params.text_document.uri;
        let mut langs = self.langs.lock().await;

        let rope = {
            let ropes = self.ropes.lock().await;
            if let Some(rope) = ropes.get(uri.as_str()) {
                rope.clone()
            } else {
                return Ok(None);
            }
        };

        let line_count = rope.len_lines();
        if line_count == 0 {
            return Ok(None);
        }

        if let Some((ref mut lang, ref mut state)) = langs.get_mut(uri.as_str()) {
            if let Some(t) = lang.format(state, params.options) {
                let end_line = rope.char_to_line(rope.len_chars());
                // if let Some(r) = offsets_to_range(0, rope.len_chars() - 1, &rope) {
                return Ok(Some(vec![
                    TextEdit::new(
                        Range::new(Position::new(0, 0), Position::new(end_line as u32 + 1, 0)),
                        t,
                    ),
                    // TextEdit::new(Range::new(Position::new(0, 0), Position::new(0, 0)), t),
                ]));
                // }
            }
        }

        Ok(None)
    }

    #[tracing::instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        info!("prepare rename");
        let loc = params.position;
        let uri = params.text_document.uri;

        let rope = {
            let ropes = self.ropes.lock().await;
            if let Some(rope) = ropes.get(uri.as_str()) {
                rope.clone()
            } else {
                return Ok(None);
            }
        };

        let mut langs = self.langs.lock().await;
        if let Some((ref mut lang, ref state)) = langs.get_mut(uri.as_str()) {
            let pos =
                position_to_offset(loc, &rope).ok_or(Error::new(ErrorCode::InvalidRequest))?;

            let (span, st) = if let Ok(x) = lang.prepare_rename(state, pos) {
                x
            } else {
                return Ok(None);
            };

            if let Some(range) = offsets_to_range(span.start + 1, span.end - 1, &rope) {
                return Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
                    range,
                    placeholder: st,
                }));
            }
        }

        Ok(None)
    }

    #[tracing::instrument(skip(self, params), fields(uri = %params.text_document_position.text_document.uri.as_str(), position = ?params.text_document_position.position))]
    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        info!("rename");
        let new_id = params.new_name;

        let loc = params.text_document_position.position;
        let uri = params.text_document_position.text_document.uri;

        let rope = {
            let ropes = self.ropes.lock().await;
            if let Some(rope) = ropes.get(uri.as_str()) {
                rope.clone()
            } else {
                return Ok(None);
            }
        };

        let mut langs = self.langs.lock().await;
        if let Some((ref mut lang, ref state)) = langs.get_mut(uri.as_str()) {
            let pos =
                position_to_offset(loc, &rope).ok_or(Error::new(ErrorCode::InvalidRequest))?;

            let changes = if let Ok(x) = lang.rename(state, pos, new_id) {
                x
            } else {
                return Ok(None);
            };

            let edits: Vec<_> = changes
                .into_iter()
                .filter_map(|(range, x)| {
                    let range = offsets_to_range(range.start + 1, range.end - 1, &rope)?;
                    Some(TextEdit::new(range, x))
                })
                .collect();

            let mut map = HashMap::new();
            map.insert(uri, edits);

            return Ok(Some(WorkspaceEdit {
                changes: Some(map),
                document_changes: None,
                change_annotations: None,
            }));
        }

        Ok(None)
    }

    #[tracing::instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        info!("semantic tokens full");
        let uri = params.text_document.uri.as_str();
        let mut langs = self.langs.lock().await;
        if let Some((ref mut lang, ref state)) = langs.get_mut(uri) {
            let tokens = lang.do_semantic_tokens(state).await;
            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens,
            })));
        }
        Ok(None)
    }

    #[tracing::instrument(skip(self))]
    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    #[tracing::instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
        })
        .await;
    }

    #[tracing::instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let text: String = params.content_changes[0].text.clone();
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            version: params.text_document.version,
            text,
        })
        .await;
    }

    #[tracing::instrument(skip(self, params), fields(uri = %params.text_document_position.text_document.uri.as_str()))]
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let id = params.text_document_position.text_document.uri.to_string();

        let mut langs = self.langs.lock().await;

        if !langs.contains_key(&id) {
            let lang = L::new(id.clone(), self.cache.clone());
            langs.insert(id.clone(), lang);
        }
        let (ref mut lang, ref state) = langs.get_mut(&id).unwrap();

        info!(
            "Trigger character {:?}",
            params
                .context
                .as_ref()
                .and_then(|x| x.trigger_character.clone())
        );

        let ctx = params
            .context
            .as_ref()
            .and_then(|x| x.trigger_character.clone());

        let simples = lang
            .do_completion(
                ctx,
                &params.text_document_position.position,
                state,
                &self.client,
            )
            .await;

        self.client
            .log_message(MessageType::INFO, "Got simples!")
            .await;

        let completions = simples.into_iter().map(|x| x.into()).collect();
        Ok(Some(CompletionResponse::Array(completions)))
    }
}

struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

impl<C: Client + Send + Sync + 'static, L: LangState<C> + Send + Sync> Backend<C, L> {
    pub fn new(client: C, cache: L::State) -> Self {
        Backend {
            cache,
            client,
            langs: Default::default(),
            ropes: Default::default(),
        }
    }

    async fn on_change(&self, params: TextDocumentItem) -> Option<()> {
        let id = params.uri.to_string();

        let mut langs = self.langs.lock().await;

        if !langs.contains_key(&id) {
            let lang = L::new(id.clone(), self.cache.clone());
            langs.insert(id.clone(), lang);
        }

        let (ref mut lang, ref mut state) = langs.get_mut(&id)?;

        let rope = Rope::from_str(&params.text);
        let (publisher, sender) = Publisher::new(
            params.uri.clone(),
            params.version,
            self.client.clone(),
            rope.clone(),
        );

        let fut = lang.update_text(&params.text, state, sender).await;
        self.client.spawn(join(fut, publisher.spawn()));

        {
            let mut ropes = self.ropes.lock().await;
            ropes.insert(id, rope);
        }

        Some(())
    }
}
