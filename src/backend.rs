use crate::lang::{Lang, LangState, SimpleCompletion};
use crate::lsp_types::*;

use crate::utils::{offset_to_position, offsets_to_range, position_to_offset};

use futures::lock::Mutex;
use ropey::Rope;

use std::collections::HashMap;
use std::fmt::Display;
use std::sync::Arc;
use tower_lsp::jsonrpc::{Error, ErrorCode, Result};
use tower_lsp::LanguageServer;

#[tower_lsp::async_trait]
pub trait Client {
    async fn log_message<M: Display + Sync + Send + 'static>(&self, ty: MessageType, msg: M) -> ();
    async fn publish_diagnostics(
        &self,
        uri: Url,
        diags: Vec<Diagnostic>,
        version: Option<i32>,
    ) -> ();
}

#[tower_lsp::async_trait]
impl Client for tower_lsp::Client {
    async fn log_message<M: Display + Sync + Send + 'static>(&self, ty: MessageType, msg: M) -> () {
        self.log_message(ty, msg).await;
    }

    async fn publish_diagnostics(
        &self,
        uri: Url,
        diags: Vec<Diagnostic>,
        version: Option<i32>,
    ) -> () {
        self.publish_diagnostics(uri, diags, version).await;
    }
}

#[derive(Debug)]
pub struct Backend<C: Client, L: LangState> {
    pub client: C,

    cache: L::State,
    langs: Arc<Mutex<HashMap<String, L>>>,
    ropes: Mutex<HashMap<String, Rope>>,
}

#[tower_lsp::async_trait]
impl<C: Client + Send + Sync + 'static, L: LangState + Send + Sync + 'static> LanguageServer
    for Backend<C, L>
where
    <L as Lang>::State: Send + Sync + 'static,
    <L as Lang>::Token: Send + Sync + 'static,
    <L as Lang>::TokenError: Send + Sync + 'static,
    <L as Lang>::Node: Send + Sync + 'static,
    <L as Lang>::Element: Send + Sync + 'static,
    <L as Lang>::ElementError: Send + Sync + 'static,
{
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        let triggers = L::TRIGGERS.iter().copied().map(String::from).collect();
        log::error!("initialize");
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
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
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: L::LEGEND_TYPES.clone().into(),
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

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
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

        if let Some(lang) = langs.get_mut(uri.as_str()) {
            if let Some(t) = lang.format(params.options) {
                let end_line = rope.char_to_line(rope.len_chars());
                // if let Some(r) = offsets_to_range(0, rope.len_chars() - 1, &rope) {
                    return Ok(Some(vec![
                        TextEdit::new(
                            Range::new(Position::new(0, 0), Position::new(end_line as u32, 0)),
                            t,
                        ),
                        // TextEdit::new(Range::new(Position::new(0, 0), Position::new(0, 0)), t),
                    ]));
                // }
            }
        }

        Ok(None)
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
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
        if let Some(lang) = langs.get_mut(uri.as_str()) {
            let pos =
                position_to_offset(loc, &rope).ok_or(Error::new(ErrorCode::InvalidRequest))?;

            let (span, st) = if let Ok(x) = lang.prepare_rename(pos) {
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

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
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
        if let Some(lang) = langs.get_mut(uri.as_str()) {
            let pos =
                position_to_offset(loc, &rope).ok_or(Error::new(ErrorCode::InvalidRequest))?;

            let changes = if let Ok(x) = lang.rename(pos, new_id) {
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

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        log::error!("semantic tokens full");
        let uri = params.text_document.uri.as_str();
        let mut langs = self.langs.lock().await;
        if let Some(lang) = langs.get_mut(uri) {
            let tokens = lang.do_semantic_tokens().await;
            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens,
            })));
        }
        Ok(None)
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        log::error!("did open");
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
        })
        .await;
    }

    async fn did_change(
        &self,
        DidChangeTextDocumentParams {
            text_document,
            content_changes,
        }: DidChangeTextDocumentParams,
    ) {
        log::error!("did change");
        let text: String = content_changes[0].text.clone();
        self.on_change(TextDocumentItem {
            uri: text_document.uri,
            version: text_document.version,
            text,
        })
        .await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        log::error!("completion");
        let id = params.text_document_position.text_document.uri.to_string();

        let mut langs = self.langs.lock().await;

        if !langs.contains_key(&id) {
            let lang = L::new(id.clone(), self.cache.clone());
            langs.insert(id.clone(), lang);
        }
        let lang = langs.get_mut(&id).unwrap();

        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "Trigger character {:?}",
                    params
                        .context
                        .as_ref()
                        .and_then(|x| x.trigger_character.clone())
                ),
            )
            .await;

        let ctx = params
            .context
            .as_ref()
            .and_then(|x| x.trigger_character.clone());

        let simples = lang.do_completion(ctx).await;

        self.client
            .log_message(MessageType::INFO, "Got simples!")
            .await;

        let end = params.text_document_position.position;
        // end.character += 1;
        let start = Position::new(end.line, end.character - 1);
        let range = Range::new(start, end);

        let completions = simples
            .into_iter()
            .map(
                |SimpleCompletion {
                     filter_text,
                     sort_text,
                     label,
                     documentation,
                     kind,
                     edit,
                 }| {
                    CompletionItem {
                        label,
                        kind: Some(kind),
                        sort_text,
                        filter_text,
                        documentation: documentation.map(|st| Documentation::String(st)),
                        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                            range,
                            new_text: edit,
                        })),
                        ..Default::default()
                    }
                },
            )
            .collect();

        Ok(Some(CompletionResponse::Array(completions)))
    }
}

struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

impl<C: Client, L: LangState + Send + Sync> Backend<C, L> {
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

        let lang = langs.get_mut(&id)?;

        let errors = lang.update_text(&params.text).await;
        let rope = Rope::from_str(&params.text);

        let diagnostics = errors
            .into_iter()
            .filter_map(|item| {
                let (span, message) = (item.range, item.msg);
                let start_position = offset_to_position(span.start, &rope)?;
                let end_position = offset_to_position(span.end, &rope)?;
                Some(Diagnostic::new_simple(
                    Range::new(start_position, end_position),
                    message,
                ))
            })
            .collect::<Vec<_>>();

        {
            let mut ropes = self.ropes.lock().await;
            ropes.insert(id, rope);
        }

        self.client
            .publish_diagnostics(params.uri, diagnostics, Some(params.version))
            .await;

        Some(())
    }
}
