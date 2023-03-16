use std::collections::HashMap;
use std::ops::{Deref, Range};

use dashmap::DashMap;
use iref::{IriBuf, PathBuf};
use json_ld::syntax::context::{definition, FragmentRef, Value};
use json_ld::ReqwestLoader;
use json_ld::{syntax, ExtractContext};
use locspan::{Meta, Span};
use rdf_types::vocabulary::IriVocabularyMut;
use rdf_types::IriVocabulary;
use tower_lsp::lsp_types::MessageType;
use tower_lsp::Client;

use crate::loader::LocalCtx;
use crate::model::{JsonToken, ParentingSystem, Spanned};
use crate::Documents;
/// Dynamic parser type.

fn range_to_span(range: &Range<usize>) -> Span {
    Span::new(range.start, range.end)
}

fn into_jsonld_value(
    obj: usize,
    system: &ParentingSystem,
) -> Option<Meta<syntax::Value<Span>, Span>> {
    type V = syntax::Value<Span>;
    let Spanned(ref t, ref range) = system[obj];
    let span = Span::new(range.start, range.end);

    let v = match t {
        JsonToken::Invalid => return None,
        JsonToken::KV(_, _) => return None,
        JsonToken::Null => V::Null,
        JsonToken::Bool(x) => V::Boolean(*x),
        JsonToken::Str(x) => V::String(smallstr::SmallString::from_str(x)),
        JsonToken::Num(_) => return None, // FIXME: Who needs number in JSON-LD context anyway?
        JsonToken::Array(arr) => V::Array(
            arr.iter()
                .flat_map(|id| into_jsonld_value(*id, system))
                .collect(),
        ),
        JsonToken::Obj(arr) => {
            let mut object = syntax::Object::new();
            arr.iter()
                .flat_map(|&id| {
                    let (k, v) = system[id].as_kv()?;
                    let key = Meta(
                        smallstr::SmallString::<[u8; 16]>::from_str(k.value().as_str()),
                        range_to_span(k.span()),
                    );

                    let value = into_jsonld_value(v, system)?;

                    Some(syntax::object::Entry { key, value })
                })
                .for_each(|x| {
                    object.push_entry(x);
                });

            V::Object(object)
        }
    };

    Some(Meta(v, span))
}

#[derive(Debug)]
pub struct ContextResolver {
    cache: DashMap<String, Context>,
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

impl ContextResolver {
    pub fn new() -> Self {
        Self {
            cache: DashMap::new(),
        }
    }

    fn filter_definition<M: Sync + Send + Clone>(
        frag: FragmentRef<M, Value<M>>,
    ) -> Option<Definition> {
        match frag {
            FragmentRef::DefinitionFragment(x) => get_definition_ref(&x),
            _ => None,
        }
    }

    async fn resolve_remote<'a>(
        &'a self,
        uri: &str,
        context: &mut Context,
        ctx: &'a LocalCtx<'a>,
        client: Client,
    ) -> Option<()> {
        if let Some(x) = self.cache.get(uri) {
            client.log_message(MessageType::INFO, "From cache").await;
            context.merge(x.value());
            return Some(());
        }

        // let mut loader = LocalLoader::from_ctx(ctx);
        // let mut loader = ReqwestLoader::default();
        client
            .log_message(
                MessageType::INFO,
                format!("here1 ctx {:?} uri {}", ctx, uri),
            )
            .await;

        let loaded = ctx.load(uri, &client).await.ok()?.into_document();

        let v = ExtractContext::extract_context(loaded)
            .map_err(|e| e.to_string())
            .ok()?;
        let new_context = v.into_value();

        let definitions: HashMap<String, Definition> = new_context
            .traverse()
            .filter_map(Self::filter_definition)
            .map(|x| (x.key.clone(), x))
            .collect();

        self.cache.insert(uri.to_string(), Context { definitions });

        if let Some(x) = self.cache.get(uri) {
            context.merge(x.value());
            Some(())
        } else {
            None
        }
    }

    fn try_inner_context(
        &self,
        parents: &ParentingSystem,
        context: &mut Context,
        index: usize,
    ) -> Result<(), String> {
        if let Some(Meta(v, span)) = into_jsonld_value(index, parents) {
            let mut object = syntax::Object::new();
            object.insert(
                Meta(smallstr::SmallString::from_str("@context"), Span::new(0, 1)),
                Meta(v, span),
            );
            let root = Meta(syntax::Value::Object(object), span);

            let v = ExtractContext::extract_context(root).map_err(|e| e.to_string())?;
            let new_context = v.into_value();

            new_context
                .traverse()
                .filter_map(Self::filter_definition)
                .for_each(|x| {
                    context.definitions.insert(x.key.clone(), x);
                });
            // }
        } else {
            return Err(format!("Invalid jsonld value {:?}", parents[0].value()));
        }

        Ok(())
    }

    #[async_recursion::async_recursion]
    async fn resolve_inner_context<'a, 'b: 'a>(
        &'a self,
        parents: &ParentingSystem,
        index: usize,
        context: &mut Context,
        ctx: &'a LocalCtx<'a>,
        client: Client,
    ) {
        client
            .log_message(
                MessageType::INFO,
                format!("Resolve inner type {}", parents[index].ty()),
            )
            .await;
        match parents[index].value() {
            JsonToken::Str(x) => {
                self.resolve_remote(x, context, ctx, client).await;
                return;
            }
            JsonToken::Array(ix) => {
                for i in ix {
                    self.resolve_inner_context(parents, *i, context, ctx, client.clone())
                        .await;
                }
                return;
            }
            JsonToken::KV(t, c) => {
                if t.as_str() == "@context" {
                    let _ = self.try_inner_context(parents, context, *c);
                }
            }
            JsonToken::Obj(_) => {
                let _ = self.try_inner_context(parents, context, index);
            }
            _ => {}
        }
    }

    fn get_root(uri: &str) -> &str {
        let uri = uri.strip_prefix("file://").unwrap_or(uri);
        uri
    }

    pub async fn resolve<'a>(
        &'a self,
        obj: &ParentingSystem,
        documents: &Documents,
        uri: &str,
        client: Client,
    ) -> Option<Context> {
        let mut out = Context::default();

        let root = uri;

        client.log_message(MessageType::INFO, "got root").await;

        let base = obj
            .iter()
            .filter_map(|x| x.1.as_kv())
            .filter(|x| x.0.value() == "@base")
            .find_map(|x| obj[x.1].as_str())
            .unwrap_or(root);

        client
            .log_message(MessageType::INFO, format!("extracted base {:?}", base))
            .await;

        let ctx = LocalCtx::new(base, documents).unwrap();

        client.log_message(MessageType::INFO, "got ctx").await;

        let root = obj[0].as_obj()?;

        for remote_context in root
            .iter()
            .map(|&i| &obj[i])
            .filter_map(|x| x.as_kv())
            .filter(|x| x.0.as_str() == "@context")
        {
            self.resolve_inner_context(obj, remote_context.1, &mut out, &ctx, client.clone())
                .await;
        }

        Some(out)
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
