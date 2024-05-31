use futures::lock::Mutex;
use futures::FutureExt;
use lsp_types::{CompletionItemKind, Range};
use sophia_api::{
    dataset::Dataset,
    quad::Quad,
    term::{matcher::Any, Term},
};
use std::{ops::Deref, ops::DerefMut, sync::Arc};
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
    prefixes: Vec<String>,
}

impl NamespaceState {
    fn new(turtle: &Turtle) -> Self {
        let triples = turtle.get_simple_triples().unwrap_or_default().to_owned();
        let prefixes = turtle
            .prefixes
            .iter()
            .flat_map(|x| x.value.expand(turtle))
            .collect();

        Self { triples, prefixes }
    }
}
#[derive(Clone, Default)]
pub struct NamespaceCompletionProvider {
    ontology_states: Arc<Mutex<HashMap<String, NamespaceState>>>,
    properties: Arc<Mutex<Vec<Property>>>,
    provider: Arc<Mutex<BasicClassProvider>>,
    types: Arc<Mutex<HashMap<MyTerm<'static>, HashSet<usize>>>>,

    shape_provider: Arc<Mutex<ShapeCompletionProvider>>,
    done: Arc<Mutex<HashSet<String>>>,
}
impl NamespaceCompletionProvider {
    pub fn new(state: &Self) -> Self {
        state.clone()
    }

    pub async fn update(
        &self,
        turtle: &Turtle,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send>> {
        // Add todo vec etc, add owl:imports
        let mut prefixes: Vec<_> = {
            let props = self.ontology_states.lock().await;
            turtle
                .prefixes
                .iter()
                .flat_map(|x| x.value.expand(turtle))
                .filter(|x| !props.contains_key(x))
                .collect()
        };

        do_update(turtle, self, &mut prefixes).await;

        let this = self.clone();
        let url = turtle.set_base.to_string();
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

                let mut props = this.ontology_states.lock().await;
                props.insert(prefix.to_string(), NamespaceState::default());
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
impl<'a> CompletionProvider<NsCompletionCtx<'a>> for NamespaceCompletionProvider {
    async fn find_completions(&self, ctx: &NsCompletionCtx, range: Range) -> Vec<SimpleCompletion> {
        let triples = &ctx.triples;

        if let Some(triple) = triples
            .triples
            .iter()
            .filter(|x| x.span.contains(&ctx.location))
            .min_by_key(|x| x.span.end - x.span.start)
        {
            info!("Found triple {}", triple);
            let props = self.properties.lock().await;
            let types = self.types.lock().await;
            let handler = self.provider.lock().await;

            if let Some(ty) = types.get(&triple.subject) {
                info!(
                    "Found types {:?}",
                    ty.iter()
                        .map(|x| handler.class(*x).as_str())
                        .collect::<Vec<_>>()
                );
                return props
                    .iter()
                    .filter(|x| {
                        info!(
                            "({}) Checking {} {} -> {}",
                            ty.contains(&x.domain),
                            x.property,
                            handler.class(x.domain).as_str(),
                            x.range.as_str(handler.deref()),
                        );
                        ty.contains(&x.domain)
                    })
                    .flat_map(|x| x.into_completion(&ctx.turtle, range.clone()))
                    .collect();
            } else {
                return props
                    .iter()
                    .flat_map(|x| x.into_completion(&ctx.turtle, range.clone()))
                    .collect();
            }
        } else {
            info!("No triple found");
        }

        // let props = self.ontology_states.lock().await;

        // props
        //     .iter()
        //     .filter(|(l, _)| l.starts_with(&ctx.prefix_url))
        //     .flat_map(|(_, xs)| xs.iter())
        //     .map(|prop: &NamespaceCompletion| prop.into_completion(ctx, range.clone()))
        //     .collect()
        vec![]
    }
}

pub struct NextTokenCompletionCtx<'a> {
    pub turtle: &'a Turtle,
    pub location: usize,
    pub triples: &'a Triples<'a>,
    pub prev_token: &'a Token,
}
#[async_trait::async_trait]
impl<'a> CompletionProvider<NextTokenCompletionCtx<'a>> for NamespaceCompletionProvider {
    async fn find_completions(
        &self,
        ctx: &NextTokenCompletionCtx,
        range: Range,
    ) -> Vec<SimpleCompletion> {
        let triples = &ctx.triples;

        if *ctx.prev_token == Token::PredType {
            let prov = self.provider.lock().await;
            let types = self.types.lock().await;
            let shape_provider = self.shape_provider.lock().await;

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
                    for ty in types {
                        info!("This might be type {}", prov.class(*ty).as_str())
                    }

                    types
                        .iter()
                        .map(|ty| prov.class(*ty).as_str())
                        .for_each(|class| class_to_completion(class));
                    return out;
                }
            }

            prov.classes
                .iter()
                .flat_map(|x| {
                    if let Class::Named(x) = x {
                        Some(x)
                    } else {
                        None
                    }
                })
                .for_each(|class| class_to_completion(class));
            return out;
        }
        vec![]
    }
}

async fn update_types(url: &str, this: &NamespaceCompletionProvider) {
    let props = this.ontology_states.lock().await;
    let state = props.get(url).unwrap();

    let mut provider = this.provider.lock().await;
    let mut types = this.types.lock().await;
    let props = this.properties.lock().await;

    for matched in state
        .triples
        .quads_matching(Any, [rdf::type_], Any, Any)
        .flatten()
    {
        let sub = matched.s().to_owned();

        if let Some(ty) = matched.o().iri() {
            let class_id = provider.named(ty.as_str());
            types
                .deref_mut()
                .insert(sub, provider.subclass_dict[class_id].clone());
        }
    }

    for p in props.iter() {
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
            types
                .deref_mut()
                .insert(sub, provider.subclass_dict[class_id].clone());
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

async fn do_update(turtle: &Turtle, this: &NamespaceCompletionProvider, todo: &mut Vec<String>) {
    let url = turtle.set_base.to_string();
    let state = NamespaceState::new(turtle);

    {
        let mut done = this.done.lock().await;
        state.triples.imports(|x| {
            if !done.contains(x.as_str()) {
                done.insert(x.as_str().to_string());
                todo.push(x.as_str().to_string());
            }
        })
    }

    let mut provider = this.provider.lock().await;
    let mut props = this.properties.lock().await;
    let mut shape_completion_provider = this.shape_provider.lock().await;

    let properties = NsPropertyProvider.provide(&state.triples, provider.deref_mut());
    props.extend(properties);
    let properties = shape_completion_provider.provide(&state.triples, provider.deref_mut());
    props.extend(properties);

    let mut props = this.ontology_states.lock().await;
    props.insert(url, state);
}
