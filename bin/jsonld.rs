use std::collections::{HashMap, HashSet};

use dashmap::DashMap;
use jsonld_language_server::contexts::{Context, ContextResolver};
use jsonld_language_server::model::{JsonToken, ParentingSystem};
use jsonld_language_server::parser::parse;
use jsonld_language_server::semantics::LEGEND_TYPE;
use ropey::Rope;
use tower_lsp::jsonrpc::{Error, ErrorCode, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    resolver: ContextResolver,
    contexts: DashMap<String, Context>,
    ids: DashMap<String, HashSet<String>>,
    document: DashMap<String, (ParentingSystem, Rope)>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec!["@".to_string(), "\"".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                }),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["dummy.do_something".to_string()],
                    work_done_progress_options: Default::default(),
                }),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("jsonld".to_string()),
                                        scheme: Some("file".to_string()),
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPE.clone().into(),
                                    token_modifiers: vec![],
                                },
                                range: Some(false),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                // definition: Some(GotoCapability::default()),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let loc = params.position;
        let uri = params.text_document.uri;
        if let Some(x) = self.document.get(uri.as_str()) {
            let (parents, rope) = (&x.0, &x.1);
            let pos =
                position_to_offset(loc, &rope).ok_or(Error::new(ErrorCode::InvalidRequest))?;
            if let Some((_, item)) = parents // find me the most matching token
                .iter()
                .filter(|(_, x)| x.span().contains(&pos))
                .min_by_key(|(_, x)| x.span().len())
            {
                let st = match item.value() {
                    JsonToken::KV(s, _) => s.as_str(),
                    JsonToken::Str(s) => s.as_str(),
                    _ => return Ok(None),
                };
                let span = item.span();
                if let Some(range) = offsets_to_range(span.start + 1, span.end - 1, &rope) {
                    return Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
                        range,
                        placeholder: String::from(st),
                    }));
                }
            }
        }
        Ok(None)
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let new_id = params.new_name;

        let loc = params.text_document_position.position;
        let uri = params.text_document_position.text_document.uri;

        if let Some(x) = self.document.get(uri.as_str()) {
            let (parents, rope) = (&x.0, &x.1);

            let pos =
                position_to_offset(loc, &rope).ok_or(Error::new(ErrorCode::InvalidRequest))?;

            if let Some((idx, item)) = parents // find me the most matching token
                .iter()
                .filter(|(_, x)| x.span().contains(&pos))
                .min_by_key(|(_, x)| x.span().len())
            {
                // is it's parent a key value? and am I a string?
                if let (Some(kv), Some(old_id)) = (
                    parents.parent(idx).and_then(|(_i, x)| x.as_kv()),
                    item.as_str(),
                ) {
                    // Check if we can actually rename this, qq
                    if kv.0.as_str() == "@id" {
                        let changes: Vec<_> = parents
                            .iter()
                            .map(|(_, x)| x) // I only care about values
                            .filter_map(|x| x.as_kv()) // give me key value pairs
                            .filter(|x| x.0.as_str() == "@id") // that are @id as key
                            .map(|x| &parents[x.1]) // now give me the value
                            .filter_map(|x| x.map_ref(|x| x.as_str()).transpose()) // that is a string
                            .filter(|x| x.value() == &old_id) // and matches the old value
                            // This should change to the new value
                            // (take care of quotes)
                            .filter_map(|x| {
                                offsets_to_range(x.span().start + 1, x.span().end - 1, rope)
                            })
                            .map(|x| TextEdit::new(x, new_id.clone()))
                            .collect();

                        let mut map = HashMap::new();
                        map.insert(uri, changes);

                        return Ok(Some(WorkspaceEdit {
                            changes: Some(map),
                            document_changes: None,
                            change_annotations: None,
                        }));
                    }
                }

                self.client
                    .show_message(
                        MessageType::INFO,
                        format!("Found (pos {} ) {:?}", pos, item),
                    )
                    .await;
            }
        }

        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let _uri = params.text_document.uri.to_string();
        self.client
            .log_message(MessageType::LOG, "semantic_token_full")
            .await;
        Ok(None)
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::INFO, "workspace folders changed!")
            .await;
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::INFO, "configuration changed!")
            .await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::INFO, "watched files have changed!")
            .await;
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<serde_json::Value>> {
        self.client
            .log_message(MessageType::INFO, "command executed!")
            .await;

        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
        })
        .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let ev: Vec<_> = params
            .content_changes
            .iter()
            .map(|x| x.text.len())
            .collect();
        self.client
            .log_message(MessageType::INFO, format!("did change! {:?}", ev))
            .await;

        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.content_changes[0].text.clone(),
            version: params.text_document.version,
        })
        .await;
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let ctx = params
            .context
            .as_ref()
            .and_then(|x| x.trigger_character.clone());

        let uri = params.text_document_position.text_document.uri;

        let mut completions = if let (Some("@"), Some(ids)) =
            (ctx.as_ref().map(|x| x.as_str()), self.ids.get(uri.as_str()))
        {
            let end = params.text_document_position.position;
            let start = Position::new(end.line, end.character - 1);
            let range = Range::new(start, end);
            let out: Vec<_> = ids
                .iter()
                .map(|s| {
                    let w = format!("@{}", s);
                    CompletionItem {
                        label: s.clone(),
                        kind: Some(CompletionItemKind::VARIABLE),
                        sort_text: Some(w.clone()),
                        filter_text: Some(w.clone()),
                        documentation: Some(Documentation::String(format!("Subject: '{}'", s))),
                        // insert_text: Some(format!("{{\"@id\": \"{}\"}}", s)),
                        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                            range,
                            new_text: format!("{{\"@id\": \"{}\"}}", s),
                        })),
                        ..Default::default()
                    }
                })
                .collect();

            let filters: Vec<_> = out.iter().flat_map(|x| &x.filter_text).collect();
            self.client
                .show_message(
                    MessageType::INFO,
                    format!("filter {:?} ids {:?}", filters, ids),
                )
                .await;
            out
        } else {
            Vec::new()
        };

        if let Some(ctx) = self.contexts.get(uri.as_str()) {
            let extra = ctx.values().map(|v| CompletionItem {
                label: v.key.to_string(),
                kind: Some(CompletionItemKind::PROPERTY),
                documentation: Some(Documentation::String(format!("\"{}\"", v.value))),
                insert_text: Some(v.key.to_string()),
                ..Default::default()
            });
            completions.extend(extra);
        }

        Ok(Some(CompletionResponse::Array(completions)))
    }
}

struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

impl Backend {
    fn find_ids(&self, obj: &ParentingSystem) -> HashSet<String> {
        obj.iter()
            .map(|(_, x)| x)
            .filter_map(|x| x.as_kv())
            .filter(|x| x.0.as_str() == "@id")
            .map(|x| &obj[x.1])
            .filter_map(|x| x.as_str())
            .map(String::from)
            .collect()
    }

    async fn on_change(&self, params: TextDocumentItem) -> Option<()> {
        self.client
            .log_message(MessageType::INFO, "Start on_change")
            .await;

        let (json, errors) = parse(&params.text);
        let parenting = ParentingSystem::from_json(json);

        if let Some(ctx) = self.resolver.resolve(&parenting).await {
            self.contexts.insert(params.uri.to_string(), ctx);
            self.client
                .log_message(MessageType::INFO, "Found jsonld context")
                .await;
        }

        let uri_str = params.uri.as_str();
        if !self.ids.contains_key(uri_str) {
            self.ids.insert(uri_str.to_string(), HashSet::default());
        }

        let ids = self.find_ids(&parenting);

        if !errors.is_empty() {
            if let Some(mut p) = self.ids.get_mut(uri_str) {
                p.value_mut().extend(ids);
            }
        } else {
            self.ids.insert(params.uri.to_string(), ids);
        }

        let rope = Rope::from_str(&params.text);
        let diagnostics = errors
            .into_iter()
            .filter_map(|item| {
                let (span, message) = (item.span, item.msg);
                let start_position = offset_to_position(span.start, &rope)?;
                let end_position = offset_to_position(span.end, &rope)?;
                Some(Diagnostic::new_simple(
                    Range::new(start_position, end_position),
                    message,
                ))
            })
            .collect::<Vec<_>>();

        self.document.insert(uri_str.to_string(), (parenting, rope));
        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
            .await;
        self.client
            .log_message(MessageType::INFO, "finished on_change")
            .await;
        Some(())
    }
}
#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        contexts: DashMap::new(),
        resolver: ContextResolver::new(),
        ids: DashMap::new(),
        document: DashMap::new(),
    })
    .finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}

fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char;
    Some(Position::new(line as u32, column as u32))
}
fn position_to_offset(position: Position, rope: &Rope) -> Option<usize> {
    let line_offset = rope.try_line_to_char(position.line as usize).ok()?;
    let line_length = rope.line(position.line as usize).len_chars();

    if (position.character as usize) < line_length {
        Some(line_offset + position.character as usize)
    } else {
        None
    }
}
fn offsets_to_range(start: usize, end: usize, rope: &Rope) -> Option<Range> {
    let start = offset_to_position(start, rope)?;
    let end = offset_to_position(end, rope)?;
    Some(Range { start, end })
}
