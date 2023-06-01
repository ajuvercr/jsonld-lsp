use crate::lang::jsonld::{JsonLd, Loader};
use crate::lang::{LangState, SimpleCompletion};
use crate::lsp_types::*;

use crate::semantics::LEGEND_TYPE;
use crate::utils::{offset_to_position};
use dashmap::DashMap;

use ropey::Rope;

use std::fmt::Display;
use tower_lsp::jsonrpc::Result;
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
pub struct Backend<C: Client> {
    pub client: C,

    loader: Loader,
    langs: DashMap<String, JsonLd>,
}

#[tower_lsp::async_trait]
impl<C: Client + Send + Sync + 'static> LanguageServer for Backend<C> {
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
                    completion_item: None,
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
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),
                ..ServerCapabilities::default()
            },
        })
    }

    // async fn prepare_rename(
    //     &self,
    //     params: TextDocumentPositionParams,
    // ) -> Result<Option<PrepareRenameResponse>> {
    //     let loc = params.position;
    //     let uri = params.text_document.uri;
    //     if let Some(x) = self.document.get(uri.as_str()) {
    //         let (parents, rope) = (&x.0, &x.1);
    //         let pos =
    //             position_to_offset(loc, &rope).ok_or(Error::new(ErrorCode::InvalidRequest))?;
    //         if let Some((_, item)) = parents // find me the most matching token
    //             .iter()
    //             .filter(|(_, x)| x.span().contains(&pos))
    //             .min_by_key(|(_, x)| x.span().len())
    //         {
    //             let st = match item.value() {
    //                 JsonToken::KV(s, _) => s.as_str(),
    //                 JsonToken::Str(s) => s.as_str(),
    //                 _ => return Ok(None),
    //             };
    //             let span = item.span();
    //             if let Some(range) = offsets_to_range(span.start + 1, span.end - 1, &rope) {
    //                 return Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
    //                     range,
    //                     placeholder: String::from(st),
    //                 }));
    //             }
    //         }
    //     }
    //     Ok(None)
    // }
    //
    // async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
    //     let new_id = params.new_name;
    //
    //     let loc = params.text_document_position.position;
    //     let uri = params.text_document_position.text_document.uri;
    //
    //     if let Some(x) = self.document.get(uri.as_str()) {
    //         let (parents, rope) = (&x.0, &x.1);
    //
    //         let pos =
    //             position_to_offset(loc, &rope).ok_or(Error::new(ErrorCode::InvalidRequest))?;
    //
    //         if let Some((idx, item)) = parents // find me the most matching token
    //             .iter()
    //             .filter(|(_, x)| x.span().contains(&pos))
    //             .min_by_key(|(_, x)| x.span().len())
    //         {
    //             // is it's parent a key value? and am I a string?
    //             if let (Some(kv), Some(old_id)) = (
    //                 parents.parent(idx).and_then(|(_i, x)| x.as_kv()),
    //                 item.as_str(),
    //             ) {
    //                 // Check if we can actually rename this, qq
    //                 if kv.0.as_str() == "@id" {
    //                     let changes: Vec<_> = parents
    //                         .iter()
    //                         .map(|(_, x)| x) // I only care about values
    //                         .filter_map(|x| x.as_kv()) // give me key value pairs
    //                         .filter(|x| x.0.as_str() == "@id") // that are @id as key
    //                         .map(|x| &parents[x.1]) // now give me the value
    //                         .filter_map(|x| x.map_ref(|x| x.as_str()).transpose()) // that is a string
    //                         .filter(|x| x.value() == &old_id) // and matches the old value
    //                         // This should change to the new value
    //                         // (take care of quotes)
    //                         .filter_map(|x| {
    //                             offsets_to_range(x.span().start + 1, x.span().end - 1, rope)
    //                         })
    //                         .map(|x| TextEdit::new(x, new_id.clone()))
    //                         .collect();
    //
    //                     let mut map = HashMap::new();
    //                     map.insert(uri, changes);
    //
    //                     return Ok(Some(WorkspaceEdit {
    //                         changes: Some(map),
    //                         document_changes: None,
    //                         change_annotations: None,
    //                     }));
    //                 }
    //             }
    //
    //             debug!("Found (pos {} ) {:?}", pos, item);
    //         }
    //     }
    //
    //     Ok(None)
    // }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.as_str();
        if let Some(mut lang) = self.langs.get_mut(uri) {
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
        let text: String = content_changes[0].text.clone();
        self.on_change(TextDocumentItem {
            uri: text_document.uri,
            version: text_document.version,
            text,
        })
        .await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let id = params.text_document_position.text_document.uri.to_string();

        if !self.langs.contains_key(&id) {
            let lang = JsonLd::new(id.clone(), self.loader.clone());
            self.langs.insert(id.clone(), lang);
        }

        let mut lang = self.langs.get_mut(&id).unwrap();

        let ctx = params
            .context
            .as_ref()
            .and_then(|x| x.trigger_character.clone());

        let simples = lang.do_completion(ctx).await;

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

impl<C: Client> Backend<C> {
    pub fn new(client: C) -> Self {
        Backend {
            client,
            langs: DashMap::new(),
            loader: Default::default(),
        }
    }

    async fn on_change(&self, params: TextDocumentItem) -> Option<()> {
        let id = params.uri.to_string();

        if !self.langs.contains_key(&id) {
            let lang = JsonLd::new(id.clone(), self.loader.clone());
            self.langs.insert(id.clone(), lang);
        }

        let mut lang = self.langs.get_mut(&id)?;

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

        self.client
            .publish_diagnostics(params.uri, diagnostics, Some(params.version))
            .await;

        Some(())
    }
}
