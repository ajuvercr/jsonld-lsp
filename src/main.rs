use std::collections::HashMap;

use dashmap::mapref::one::Ref;
use dashmap::DashMap;
use iref::IriBuf;
use json_ld::syntax::context::{definition, FragmentRef, Value};
use json_ld::{ContextLoader, ReqwestLoader};
use jsonld_language_server::parser::{Obj, parse, Json};
use jsonld_language_server::semantics::LEGEND_TYPE;
use locspan::Location;
use ropey::Rope;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
pub struct ContextResolver {
    cache: DashMap<String, Context>,
}

fn get_definition_ref(
    re: &definition::FragmentRef<Location<IriBuf>, Value<Location<IriBuf>>>,
) -> Option<Definition> {
    match re {
        definition::FragmentRef::Entry(definition::EntryRef::Definition(key, value)) => {
            let binding = match value.definition.value().as_ref().unwrap() {
                json_ld::syntax::context::TermDefinitionRef::Simple(s) => s.as_str().to_string(),
                json_ld::syntax::context::TermDefinitionRef::Expanded(e) => {
                    e.id.as_ref()
                        .unwrap()
                        .as_ref()
                        .unwrap()
                        .as_str()
                        .to_string()
                }
            };
            // println!("{:?}", value.definition.metadata());
            Some(Definition {
                key: key.as_str().to_string(),
                value: binding,
            })
        }
        _ => None,
    }
}
impl ContextResolver {
    pub fn new() -> Self {
        Self {
            cache: DashMap::new(),
        }
    }

    fn filter_definition(
        frag: FragmentRef<Location<IriBuf>, Value<Location<IriBuf>>>,
    ) -> Option<Definition> {
        match frag {
            // FragmentRef::Context(x) => print_context_ref(&x),
            FragmentRef::DefinitionFragment(x) => get_definition_ref(&x),
            // FragmentRef::ContextArray(_) => println!("context array"),
            _ => None,
        }
    }

    pub async fn resolve<'a>(&'a self, uri: &str) -> Option<Ref<String, Context>> {
        if let Some(x) = self.cache.get(uri) {
            return Some(x);
        }

        let mut loader = ReqwestLoader::default();
        let iri = IriBuf::new(uri).ok()?;
        let context = loader.load_context(iri).await.ok()?;

        let definitions: HashMap<String, Definition> = context
            .document()
            .value()
            .traverse()
            .filter_map(Self::filter_definition)
            .map(|x| (x.key.clone(), x))
            .collect();

        self.cache.insert(uri.to_string(), Context { definitions });

        self.cache.get(uri)
    }
}

#[derive(Debug, Clone)]
pub struct Context {
    definitions: HashMap<String, Definition>,
}

impl Context {
    pub fn merge(&mut self, other: &Self) {
        self.definitions.extend(other.definitions.clone());
    }
}

#[derive(Debug, Clone)]
pub struct Definition {
    key: String,
    value: String,
}

#[derive(Debug)]
struct Backend {
    client: Client,
    resolver: ContextResolver,
    contexts: DashMap<String, Context>,
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
                    trigger_characters: Some(vec![".".to_string()]),
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
                rename_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
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

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
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
        let uri = params.text_document_position.text_document.uri;

        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "Found context {} ({})",
                    self.contexts.get(uri.as_str()).is_some(),
                    uri.as_str()
                ),
            )
            .await;

        let completions = (|| {
            let ctx = self.contexts.get(uri.as_str())?;

            let completions: Vec<_> = ctx
                .definitions
                .values()
                .map(|v| CompletionItem {
                    label: v.key.to_string(),
                    kind: Some(CompletionItemKind::PROPERTY),
                    detail: Some(format!("{} -> {}", v.key, v.value)),
                    insert_text: Some(v.key.to_string()),
                    ..Default::default()
                })
                .collect();
            Some(completions)
        })();

        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "Found {} completions",
                    completions.as_ref().map(|x| x.len()).unwrap_or(0)
                ),
            )
            .await;

        Ok(completions.map(CompletionResponse::Array))
    }
}

struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

impl Backend {
    async fn find_ctx(&self, obj: &Json) -> Option<Context> {
        let obj = obj.as_obj()?;
        let context = obj.get("@context")?;

        if let Some(x) = context.as_str() {
            self.client
                .log_message(MessageType::INFO, format!("Resolving context {}", x))
                .await;
            return self.resolver.resolve(&x).await?.value().clone().into();
        }

        if let Some(arr) = context.as_arr() {
            let mut base: Option<Context> = None;
            for sol in arr
                .iter()
                .filter_map(|x| x.as_str())
            {
                if let Some(con) = self.resolver.resolve(sol).await {
                    if let Some(x) = base.as_mut() {
                        x.merge(con.value());
                    } else {
                        base = Some(con.value().clone());
                    }
                }
            }
        }

        None
    }

    async fn on_change(&self, params: TextDocumentItem) -> Option<()> {
        let (json, errors) = parse(&params.text);

        if let Some(ctx) = self.find_ctx(&json).await {
            self.client
                .log_message(
                    MessageType::INFO,
                    format!(
                        "Succesfully found jsonld context ({})",
                        params.uri.to_string()
                    ),
                )
                .await;
            self.contexts.insert(params.uri.to_string(), ctx);
        }

        self.client
            .log_message(MessageType::INFO, "Succesfully parsed jsonld")
            .await;
        // Try to get the context from the file
        //
        // let mut diagnostics = errors
        //     .into_iter()
        //     .filter_map(|item| {
        //         let (message, span) = match item.reason() {
        //             chumsky::error::SimpleReason::Unclosed { span, delimiter } => {
        //                 (format!("Unclosed delimiter {}", delimiter), span.clone())
        //             }
        //             chumsky::error::SimpleReason::Unexpected => (
        //                 format!(
        //                     "{}, expected {}",
        //                     if item.found().is_some() {
        //                         "Unexpected token in input"
        //                     } else {
        //                         "Unexpected end of input"
        //                     },
        //                     if item.expected().len() == 0 {
        //                         "something else".to_string()
        //                     } else {
        //                         item.expected()
        //                             .map(|expected| match expected {
        //                                 Some(expected) => expected.to_string(),
        //                                 None => "end of input".to_string(),
        //                             })
        //                             .collect::<Vec<_>>()
        //                             .join(", ")
        //                     }
        //                 ),
        //                 item.span(),
        //             ),
        //             chumsky::error::SimpleReason::Custom(msg) => (msg.to_string(), item.span()),
        //         };
        //
        //         // let start_line = rope.try_char_to_line(span.start)?;
        //         // let first_char = rope.try_line_to_char(start_line)?;
        //         // let start_column = span.start - first_char;
        //         let start_position = offset_to_position(span.start, &rope)?;
        //         let end_position = offset_to_position(span.end, &rope)?;
        //         // let end_line = rope.try_char_to_line(span.end)?;
        //         // let first_char = rope.try_line_to_char(end_line)?;
        //         // let end_column = span.end - first_char;
        //         Some(Diagnostic::new_simple(
        //             Range::new(start_position, end_position),
        //             message,
        //         ))
        //     })
        //     .collect::<Vec<_>>();
        //
        // if let (Some(start), Some(end)) =
        //     (offset_to_position(0, &rope), offset_to_position(2, &rope))
        // {
        //     diagnostics.push(Diagnostic::new_simple(
        //         Range::new(start, end),
        //         String::from("tetten zijn leuk"),
        //     ));
        // }

        // self.client
        //     .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
        //     .await;
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
