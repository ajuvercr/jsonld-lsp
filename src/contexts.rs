use std::collections::HashMap;
use std::ops::{Deref, Range};

use dashmap::DashMap;
use iref::IriBuf;
use json_ld::syntax::context::{definition, FragmentRef, Value};
use json_ld::{syntax, ReqwestLoader};
use locspan::{Meta, Span};

use crate::model::{JsonToken, ParentingSystem, Spanned};

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

    async fn resolve_remote<'a>(&'a self, uri: &str, context: &mut Context) -> Option<()> {
        use json_ld::ContextLoader;

        if let Some(x) = self.cache.get(uri) {
            context.merge(x.value());
            return Some(());
        }

        let mut loader = ReqwestLoader::default();
        let iri = IriBuf::new(uri).ok()?;
        let loaded = loader
            .load_context(iri)
            .await
            .ok()?
            .into_document()
            .into_value();

        let definitions: HashMap<String, Definition> = loaded
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
        use json_ld::ExtractContext;
        if let Some(Meta(v, span)) = into_jsonld_value(index, parents) {
            let mut object = syntax::Object::new();
            object.insert(
                Meta(smallstr::SmallString::from_str("@context"), Span::new(0, 1)),
                Meta(v, span),
            );
            let root = Meta(syntax::Value::Object(object), span);

            let v = ExtractContext::extract_context(root).map_err(|e| e.to_string())?;
            // if let Ok(v) = ExtractContext::extract_context(root).map_err(|e| e.to_string()) {
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
    async fn resolve_inner_context(
        &self,
        parents: &ParentingSystem,
        index: usize,
        context: &mut Context,
    ) {
        match parents[index].value() {
            JsonToken::Str(x) => {
                self.resolve_remote(x, context).await;
                return;
            }
            JsonToken::Array(ix) => {
                for i in ix {
                    self.resolve_inner_context(parents, *i, context).await;
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

    pub async fn resolve<'a>(&'a self, obj: &ParentingSystem) -> Option<Context> {
        let mut out = Context::default();

        let root = obj[0].as_obj()?;

        for remote_context in root
            .iter()
            .map(|&i| &obj[i])
            .filter_map(|x| x.as_kv())
            .filter(|x| x.0.as_str() == "@context")
        {
            self.resolve_inner_context(obj, remote_context.1, &mut out)
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
