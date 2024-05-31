use futures::lock::Mutex;
use futures::FutureExt;
use lsp_types::Range;
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
                BasicClassProvider, ClassProvider, NsPropertyProvider, Property, PropertyProvider,
                Range as R,
            },
            shacl::{MyTerm, Triples},
        },
        SimpleCompletion,
    },
    ns::rdf,
    utils::{fetch, turtle::*},
};

use super::{CompletionProvider, Turtle};
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
}
impl NamespaceCompletionProvider {
    pub fn new(state: &Self) -> Self {
        state.clone()
    }

    pub async fn update(
        &self,
        turtle: &Turtle,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send>> {
        let prefixes: Vec<_> = {
            let props = self.ontology_states.lock().await;
            turtle
                .prefixes
                .iter()
                .flat_map(|x| x.value.expand(turtle))
                .filter(|x| !props.contains_key(x))
                .collect()
        };

        do_update(turtle, self).await;

        let this = self.clone();
        let url = turtle.set_base.to_string();
        async move {
            let headers = [("Accept".to_string(), "text/turtle".to_string())].into();
            for prefix in prefixes {
                if let Ok(resp) = fetch(&prefix, &headers).await {
                    if let Ok(turtle) = parse(&resp.body, &lsp_types::Url::parse(&prefix).unwrap())
                    {
                        do_update(&turtle, &this).await;
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
}

#[async_trait::async_trait]
impl<'a> CompletionProvider<NsCompletionCtx<'a>> for NamespaceCompletionProvider {
    async fn find_completions(&self, ctx: &NsCompletionCtx, range: Range) -> Vec<SimpleCompletion> {
        let triples = ctx.turtle.get_simple_triples().unwrap_or_default();

        for triple in &triples.triples {
            info!(
                "triple {} {} {}",
                triple,
                ctx.location,
                triple.span.contains(&ctx.location)
            );
        }

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
                            "Checking {} ({}) {} -> {}",
                            x.property,
                            ty.contains(&x.domain),
                            handler.class(x.domain).as_str(),
                            x.range.as_str(handler.deref()),
                        );
                        ty.contains(&x.domain)
                    })
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

async fn do_update(turtle: &Turtle, this: &NamespaceCompletionProvider) {
    let url = turtle.set_base.to_string();
    let state = NamespaceState::new(turtle);

    let mut provider = this.provider.lock().await;
    let mut props = this.properties.lock().await;

    let properties = NsPropertyProvider::new(&state.triples).provide(provider.deref_mut());
    props.extend(properties);

    let mut props = this.ontology_states.lock().await;
    props.insert(url, state);
}
