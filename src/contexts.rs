use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::ops::Deref;

use dashmap::DashMap;
use json_ld::syntax::context::{definition, FragmentRef, Value};
use json_ld::{syntax, ExtractContext};
use locspan::{Meta, Span};
use log::{debug, error};
use lsp_types::{MessageType};
use ropey::Rope;

use crate::backend::Client;
use crate::lsp_types::Url;

use crate::model::{JsonToken, ParentingSystem};
use crate::utils::ReqwestLoader;
use crate::Documents;

#[derive(Default)]
pub struct CtxResolver {
    remote_contexts: DashMap<String, Result<Context, RemoteError>>,
}

impl std::fmt::Debug for CtxResolver {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ctx: Vec<_> = self
            .remote_contexts
            .iter()
            .map(|x| {
                let v = match x.value() {
                    Ok(_) => Cow::Borrowed("Loaded"),
                    Err(RemoteError::LoaderFail(i)) => Cow::Owned(format!("LoadFailed({})", i)),
                    Err(RemoteError::InvalidIri) => Cow::Borrowed("Loaded"),
                    Err(RemoteError::Offline) => Cow::Borrowed("Offline"),
                };
                format!("{}: {}", x.key(), v)
            })
            .collect();

        write!(f, "CtxResolver ( {:?} )", ctx)
    }
}

impl CtxResolver {
    pub async fn resolve<C: Client + Sync>(
        &self,
        uri: &Url,
        documents: &Documents,
        c: &C,
    ) -> Context {
        let mut res = Resolver {
            remote_contexts: &self.remote_contexts,
            documents,
            done: HashSet::new(),
        };

        res.get_context(uri, c).await
    }
}

#[allow(unused)]
enum RemoteError {
    Offline,
    InvalidIri,
    LoaderFail(u16),
}

struct Resolver<'a> {
    remote_contexts: &'a DashMap<String, Result<Context, RemoteError>>,
    documents: &'a Documents,
    done: HashSet<String>,
}

async fn try_load_remote<C: Client + Sync>(uri: &str, c: &C) -> Result<Context, RemoteError> {
    use json_ld::ContextLoader;
    let uri = uri;
    let mut loader = ReqwestLoader::default();
    let iri = iref::Iri::from_str(uri)
        .map_err(|_| RemoteError::InvalidIri)?
        .to_owned();

    c.log_message(MessageType::INFO, format!("Correct iri {}", uri))
        .await;

    let doc = loader
        .load_context(iri)
        .await
        .map_err(|_| RemoteError::LoaderFail(1))?;

    let definitions: HashMap<String, Definition> = doc
        .into_document()
        .into_value()
        .traverse()
        .filter_map(filter_definition)
        .map(|x| (x.key.clone(), x))
        .collect();

    Ok(Context {
        definitions,
        ..Default::default()
    })
}

async fn load_file(_uri: &Url) -> Option<(ParentingSystem, Rope)> {
    cfg_if::cfg_if! {
        if #[cfg(req)] {
            let uri = _uri;
            debug!("loading file {}", uri);
            if uri.scheme() != "file" {
                return None;
            }
            let path = uri.to_file_path().ok()?;
            let st = read_to_string(path).await.ok()?;

            let (json, _) = parse(&st);
            let parenting = ParentingSystem::from_json(json);

            debug!("succeeded");
            Some((parenting, Rope::from_str(&st)))
        } else {
            None
        }
    }
}

impl<'a> Resolver<'a> {
    async fn get_context<C: Client + Sync>(&mut self, uri: &Url, c: &C) -> Context {
        let mut out = Context::default();
        self._get_context(uri, &mut out, c).await;
        return out;
    }

    #[async_recursion::async_recursion]
    async fn _get_context_from_value<C: Client + Sync>(
        &mut self,
        uri: &Url,
        context: &mut Context,
        token: usize,
        parents: &ParentingSystem,
        c: &C,
    ) {
        match parents[token].value() {
            JsonToken::Str(s) => {
                if let Ok(joined) = uri.join(s.as_str()) {
                    debug!("Recursive! looking for uri {}", joined);
                    self._get_context(&joined, context, c).await;
                } else {
                    error!("Failed to join! ({}.join({}))", uri, s);
                }
            }
            JsonToken::Array(arr) => {
                debug!("Found array (len {})", arr.len());
                for i in arr {
                    self._get_context_from_value(uri, context, *i, parents, c)
                        .await;
                }
            }
            JsonToken::Obj(arr) => {
                debug!("Found obj (len {})", arr.len());
                if let Some(Meta(v, span)) = parents.jsonld_value(token) {
                    let mut object = syntax::Object::new();
                    object.insert(Meta("@context".into(), Span::new(0, 1)), Meta(v, span));
                    let root = Meta(syntax::Value::Object(object), span);

                    if let Ok(v) = ExtractContext::extract_context(root).map_err(|e| e.to_string())
                    {
                        let new_context = v.into_value();

                        new_context
                            .traverse()
                            .filter_map(filter_definition)
                            .for_each(|x| {
                                context.definitions.insert(x.key.clone(), x);
                            });
                    }
                }
            }
            _ => {}
        }
    }

    #[async_recursion::async_recursion]
    async fn _get_context<C: Client + Sync>(&mut self, uri: &Url, context: &mut Context, c: &C) {
        if self.done.contains(uri.as_str()) {
            c.log_message(MessageType::INFO, format!("infinite loop averted {}", uri))
                .await;
            return;
        }

        self.done.insert(uri.to_string());
        c.log_message(MessageType::INFO, format!("Get context for {}", uri))
            .await;

        if uri.scheme() == "http" || uri.scheme() == "https" {
            c.log_message(MessageType::INFO, "Remote context".to_string())
                .await;
            debug!("which is a remote");
            if let Some(mut remote) = self.remote_contexts.get_mut(uri.as_str()) {
                debug!("that we already found once (ok {})", remote.is_ok());
                match remote.value_mut() {
                    Ok(x) => context.merge(&x),
                    Err(RemoteError::LoaderFail(tr)) => {
                        if *tr < 5 {
                            let new_context = try_load_remote(uri.as_str(), c)
                                .await
                                .map_err(|_| RemoteError::LoaderFail(*tr + 1));
                            if let Ok(x) = &new_context {
                                context.merge(&x);
                            }

                            *remote = new_context;
                        }
                    }
                    Err(RemoteError::InvalidIri) => {}
                    Err(RemoteError::Offline) => {}
                }
            } else {
                let new_context = try_load_remote(uri.as_str(), c).await;
                c.log_message(
                    MessageType::INFO,
                    format!("Found new context {}", new_context.is_ok()),
                )
                .await;
                if let Ok(x) = &new_context {
                    context.merge(&x);
                }
                debug!("looked it up (ok {})", new_context.is_ok());
                self.remote_contexts.insert(uri.to_string(), new_context);
            }
        } else {
            debug!("which is a file");
            // Get that thing!
            if !self.documents.contains_key(uri.as_str()) {
                c.log_message(
                    MessageType::INFO,
                    format!("Got to load local file {}", uri.as_str()),
                )
                .await;

                if let Some(item) = load_file(uri).await {
                    self.documents.insert(uri.as_str().to_string(), item);
                }
            }
            if let Some(d) = self.documents.get(uri.as_str()) {
                let doc = &d.0;
                if let Some(obj) = doc[0].as_obj() {
                    let find = |x: &str| {
                        obj.iter()
                            .filter_map(|i| doc[*i].as_kv())
                            .find(|i| i.0.value() == x)
                            .map(|x| x.1)
                    };

                    let base = find("@base")
                        .and_then(|x| doc[x].as_str())
                        .and_then(|st| uri.join(st).ok())
                        .unwrap_or(uri.clone());

                    debug!("Using base {}", base);

                    if let Some(inner_ctx) = find("@context") {
                        c.log_message(MessageType::INFO, format!("Found context {}", inner_ctx))
                            .await;
                        self._get_context_from_value(&base, context, inner_ctx, doc, c)
                            .await;
                    } else {
                        c.log_message(MessageType::INFO, format!("Didn't find context"))
                            .await;
                    }
                }
            }
        }
    }
}

fn get_definition_ref<M: Send + Sync + Clone>(
    re: &definition::FragmentRef<M, Value<M>>,
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
            Some(Definition {
                key: key.as_str().to_string(),
                value: binding,
            })
        }
        _ => None,
    }
}

pub fn filter_definition<M: Sync + Send + Clone>(frag: FragmentRef<M, Value<M>>) -> Option<Definition> {
    match frag {
        FragmentRef::DefinitionFragment(x) => get_definition_ref(&x),
        _ => None,
    }
}

#[derive(Debug, Clone, Default)]
pub struct Context {
    definitions: HashMap<String, Definition>,
}

impl Deref for Context {
    type Target = HashMap<String, Definition>;

    fn deref(&self) -> &Self::Target {
        &self.definitions
    }
}

impl Context {
    pub fn merge(&mut self, other: &Self) {
        self.definitions.extend(other.definitions.clone());
    }
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub key: String,
    pub value: String,
}
