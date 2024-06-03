use futures::lock::Mutex;
use futures::FutureExt;
use lsp_types::{CompletionItemKind, Range};
use sophia_api::{
    dataset::Dataset,
    quad::Quad,
    term::{matcher::Any, Term},
};
use std::{ops::DerefMut, sync::Arc};
use tracing::info;

use crate::{
    lang::{
        turtle::{
            green::{
                BasicClassProvider, Class, ClassProvider, NsPropertyProvider, Property,
                PropertyProvider, Range as R,
            },
            shacl::{MyTerm, Triples},
            token::Token,
        },
        SimpleCompletion,
    },
    ns::rdf,
    utils::{fetch, turtle::*},
};

use super::{CompletionProvider, ShapeCompletionProvider, Turtle};
use hashbrown::{HashMap, HashSet};

#[derive(Default, Debug)]
struct NamespaceState {
    triples: Triples<'static>,
}

impl NamespaceState {
    fn new(turtle: &Turtle) -> Self {
        let triples = turtle.get_simple_triples().unwrap_or_default().to_owned();

        Self { triples }
    }
}

#[derive(Clone, Default)]
pub struct ArcedNamespaceCompletionProvider {
    inner: Arc<Mutex<NamespaceCompletionProvider>>,
}
impl ArcedNamespaceCompletionProvider {
    pub fn new(state: &Self) -> Self {
        state.clone()
    }
}

#[derive(Default)]
pub struct NamespaceCompletionProvider {
    ontology_states: HashMap<String, NamespaceState>,
    properties: HashMap<String, Vec<Property>>,
    types: HashMap<MyTerm<'static>, HashSet<usize>>,
    shape_provider: ShapeCompletionProvider,
    done: HashSet<String>,
    provider: BasicClassProvider,
}

impl ArcedNamespaceCompletionProvider {
    pub async fn update(
        &self,
        turtle: &Turtle,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send>> {
        // Add todo vec etc, add owl:imports
        let mut prefixes: Vec<_> = {
            let mut guard = self.inner.lock().await;
            let props = &mut guard.done;
            turtle
                .prefixes
                .iter()
                .flat_map(|x| x.value.expand(turtle))
                .flat_map(|x| {
                    if !props.contains(&x) {
                        props.insert(x.clone());
                        Some(x)
                    } else {
                        None
                    }
                })
                .collect()
        };

        do_update(turtle, self, &mut prefixes).await;

        let url = turtle.set_base.to_string();
        let this = self.clone();
        update_types(&url, &this).await;

        async move {
            let headers = [("Accept".to_string(), "text/turtle".to_string())].into();
            while let Some(prefix) = prefixes.pop() {
                if let Ok(resp) = fetch(&prefix, &headers).await {
                    if let Ok(turtle) = parse(&resp.body, &lsp_types::Url::parse(&prefix).unwrap())
                    {
                        do_update(&turtle, &this, &mut prefixes).await;
                        continue;
                    }
                }

                let mut guard = this.inner.lock().await;
                guard
                    .ontology_states
                    .insert(prefix.to_string(), NamespaceState::default());
            }

            update_types(&url, &this).await;
        }
        .boxed()
    }
}

pub struct NsCompletionCtx<'a> {
    pub turtle: &'a Turtle,
    pub location: usize,
    pub triples: &'a Triples<'a>,
}

#[async_trait::async_trait]
impl<'a> CompletionProvider<NsCompletionCtx<'a>> for ArcedNamespaceCompletionProvider {
    async fn find_completions(&self, ctx: &NsCompletionCtx, range: Range) -> Vec<SimpleCompletion> {
        let triples = &ctx.triples;

        if let Some(triple) = triples
            .triples
            .iter()
            .map(|x| {
                info!("({}) Maybe triple {}", x.span.contains(&ctx.location), x);
                x
            })
            .filter(|x| x.span.contains(&ctx.location))
            .min_by_key(|x| x.span.end - x.span.start)
        {
            let mut guard = self.inner.lock().await;
            let NamespaceCompletionProvider {
                ontology_states: _,
                ref mut properties,
                ref mut types,
                shape_provider: _,
                done: _,
                ref mut provider,
            } = guard.deref_mut();
            info!("Found triple {}", triple);

            if let Some(ty) = types.get(&triple.subject) {
                info!(
                    "Found types {:?}",
                    ty.iter()
                        .map(|x| provider.class(*x).as_str())
                        .collect::<Vec<_>>()
                );
                return properties
                    .values()
                    .flatten()
                    .filter(|x| {
                        // info!(
                        //     "({}) Checking {} {} -> {}",
                        //     ty.contains(&x.domain),
                        //     x.property,
                        //     handler.class(x.domain).as_str(),
                        //     x.range.as_str(handler.deref()),
                        // );
                        ty.contains(&x.domain)
                    })
                    .flat_map(|x| x.into_completion(&ctx.turtle, range.clone()))
                    .collect();
            } else {
                info!("No types found :(");
                return properties
                    .values()
                    .flatten()
                    .flat_map(|x| x.into_completion(&ctx.turtle, range.clone()))
                    .collect();
            }
        } else {
            info!("No triple found");
        }

        vec![]
    }
}

pub struct NextTokenCompletionCtx<'a> {
    pub turtle: &'a Turtle,
    pub location: usize,
    pub triples: &'a Triples<'a>,
    pub prev_token: &'a Token,
    pub current_token: &'a Token,
}
#[async_trait::async_trait]
impl<'a> CompletionProvider<NextTokenCompletionCtx<'a>> for ArcedNamespaceCompletionProvider {
    async fn find_completions(
        &self,
        ctx: &NextTokenCompletionCtx,
        range: Range,
    ) -> Vec<SimpleCompletion> {
        info!("Next token completion! {:?}", ctx.prev_token);

        if !(*ctx.prev_token == Token::PredType || *ctx.current_token == Token::PredType) {
            return Vec::new();
        }

        let triples = &ctx.triples;

        let mut guard = self.inner.lock().await;
        let NamespaceCompletionProvider {
            ontology_states: _,
            properties: _,
            ref mut types,
            shape_provider,
            done: _,
            ref mut provider,
        } = guard.deref_mut();

        let mut out = Vec::new();

        let mut class_to_completion = |class: &str| {
            if let Some(property) = ctx.turtle.shorten(class) {
                let edits = vec![lsp_types::TextEdit {
                    new_text: property.clone(),
                    range: range.clone(),
                }];

                out.push(SimpleCompletion {
                    kind: CompletionItemKind::CLASS,
                    label: property,
                    filter_text: None,
                    sort_text: None,
                    documentation: None,
                    edits,
                });
            }

            shape_provider.find_snippets(class, &ctx.turtle, range.clone(), &mut out);
        };

        if let Some(triple) = triples
            .triples
            .iter()
            .filter(|x| x.span.contains(&ctx.location))
            .min_by_key(|x| x.span.end - x.span.start)
        {
            info!(
                "Found subject {} with types {}",
                triple.subject,
                types.get(&triple.subject).is_some()
            );

            if let Some(types) = types.get(&triple.subject) {
                if types.len() > 1 {
                    for ty in types {
                        info!("This might be type {}", provider.class(*ty).as_str())
                    }

                    types
                        .iter()
                        .map(|ty| provider.class(*ty).as_str())
                        .for_each(|class| class_to_completion(class));
                    return out;
                }
            }
        }

        provider
            .classes
            .iter()
            .flat_map(|x| {
                if let Class::Named(x) = x {
                    Some(x)
                } else {
                    None
                }
            })
            .for_each(|class| class_to_completion(class));

        out
    }
}

async fn update_types(url: &str, this: &ArcedNamespaceCompletionProvider) {
    let mut guard = this.inner.lock().await;
    let NamespaceCompletionProvider {
        ref mut ontology_states,
        ref mut properties,
        ref mut types,
        shape_provider: _,
        done: _,
        ref mut provider,
    } = guard.deref_mut();

    let state = ontology_states.get(url).unwrap();

    types.clear();

    for matched in state
        .triples
        .quads_matching(Any, [rdf::type_], Any, Any)
        .flatten()
    {
        let sub = matched.s().to_owned();

        if let Some(ty) = matched.o().iri() {
            let class_id = provider.named(ty.as_str());
            types.insert(sub, provider.subclass_dict[class_id].clone());
        }
    }

    for p in properties.values().flatten() {
        for mat in state
            .triples
            .quads_matching(Any, [p.property.borrow_term()], Any, Any)
            .flatten()
        {
            // Check domain
            let s = mat.s().to_owned();
            if !types.contains_key(&s) {
                types.insert(s.clone(), HashSet::new());
            }

            types
                .get_mut(&s)
                .unwrap()
                .extend(&provider.subclass_dict[p.domain]);

            // Check range
            if let R::Class(range) = p.range {
                let o = mat.o().to_owned();
                if !types.contains_key(&o) {
                    types.insert(o.clone(), HashSet::new());
                }

                types
                    .get_mut(&o)
                    .unwrap()
                    .extend(&provider.subclass_dict[range]);
            }
        }
    }

    // Reset type things
    for matched in state
        .triples
        .quads_matching(Any, [rdf::type_], Any, Any)
        .flatten()
    {
        let sub = matched.s().to_owned();

        if let Some(ty) = matched.o().iri() {
            let class_id = provider.named(ty.as_str());
            types.insert(sub, provider.subclass_dict[class_id].clone());
        }
    }

    for (k, v) in types.iter() {
        info!(
            "Types {}: {:?}",
            k,
            v.iter()
                .map(|x| provider.class(*x).as_str())
                .collect::<Vec<_>>()
        );
    }
}

async fn do_update(
    turtle: &Turtle,
    this: &ArcedNamespaceCompletionProvider,
    todo: &mut Vec<String>,
) {
    let url = turtle.set_base.to_string();
    let state = NamespaceState::new(turtle);

    let mut guard = this.inner.lock().await;
    let NamespaceCompletionProvider {
        ref mut ontology_states,
        ref mut properties,
        types: _,
        ref mut shape_provider,
        ref mut done,
        ref mut provider,
    } = guard.deref_mut();

    state.triples.imports(|x| {
        if !done.contains(x.as_str()) {
            done.insert(x.as_str().to_string());
            todo.push(x.as_str().to_string());
        }
    });

    let mut new_props = vec![];
    new_props.extend(NsPropertyProvider.provide(&state.triples, provider));
    new_props.extend(shape_provider.provide(&state.triples, provider));
    properties.insert(url.clone(), new_props);
    ontology_states.insert(url, state);
}
