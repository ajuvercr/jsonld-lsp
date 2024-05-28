use std::sync::Arc;

use futures::{lock::Mutex, FutureExt};
use hashbrown::{HashMap, HashSet};
use lsp_types::Range;
use tracing::info;

use crate::{
    lang::{
        turtle::{
            node::Leaf,
            shacl::{parse_shapes, Shape, Triples},
            token::Token,
            NamedNode, Turtle, TurtleLang,
        },
        CurrentLangState, SimpleCompletion,
    },
    model::Spanned,
    utils::{fetch, turtle::parse},
};

use super::CompletionProvider;

pub struct ShapeCompletionCtx<'a> {
    pub state: &'a CurrentLangState<TurtleLang>,
    pub loc: usize,
    pub prefix_url: &'a str,
    pub prefix: &'a str,
    pub turtle: &'a Turtle,
}

pub struct PropertyCompletionCtx<'a> {
    pub state: &'a CurrentLangState<TurtleLang>,
    pub loc: usize,
}

#[derive(Clone, Debug)]
pub struct SubClass {
    clazz: String,
    sub_class: String,
}

#[derive(Clone, Debug, Default)]
pub struct ShapeCompletionProviderState {
    shapes: Arc<Mutex<HashMap<String, Vec<Shape>>>>,
    sub_classes: Arc<Mutex<HashMap<String, Vec<SubClass>>>>,
}

#[derive(Clone)]
pub struct ShapeCompletionProvider {
    state: ShapeCompletionProviderState,
}

impl ShapeCompletionProvider {
    pub fn new(state: &ShapeCompletionProviderState) -> Self {
        Self {
            state: state.clone(),
        }
    }

    pub async fn update(
        &self,
        turtle: &Turtle,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send>> {
        let triples = turtle.get_simple_triples().unwrap_or_default();
        let mut todo: Vec<String> = vec![];
        self.set_shapes(&triples, &mut todo).await;
        self.set_sub_classes(&triples).await;

        let this = self.clone();
        async move {
            let headers = [("Accept".to_string(), "text/turtle".to_string())].into();
            while let Some(url) = todo.pop() {
                if let Ok(resp) = fetch(&url, &headers).await {
                    if let Ok(turtle) = parse(&resp.body, &lsp_types::Url::parse(&url).unwrap()) {
                        let triples = turtle.get_simple_triples().unwrap_or_default();
                        this.set_shapes(&triples, &mut todo).await;
                        this.set_sub_classes(&triples).await;
                    }
                }
            }
        }
        .boxed()
    }

    async fn set_shapes(&self, triples: &Triples<'_>, todo: &mut Vec<String>) {
        let mut small_todo = vec![];

        triples.imports(|x| small_todo.push(x.to_string()));

        let shapes = parse_shapes(&triples);

        {
            let mut state = self.state.shapes.lock().await;
            state.insert(triples.base_url.clone(), shapes);

            small_todo
                .into_iter()
                .filter(|x| !state.contains_key(x))
                .for_each(|x| todo.push(x))
        }
    }

    async fn set_sub_classes(&self, triples: &Triples<'_>) {
        let mut sub_classes = vec![];

        triples.sub_class_of(|clazz, sub_class| {
            sub_classes.push(SubClass {
                clazz: clazz.to_string(),
                sub_class: sub_class.to_string(),
            })
        });

        {
            let mut state = self.state.sub_classes.lock().await;
            state.insert(triples.base_url.clone(), sub_classes);
        }
    }

    async fn find_subclasses<'a>(&'a self, start: &'a str) -> Vec<String> {
        let mut out = vec![];
        let sub_classes = self.state.sub_classes.lock().await;

        let mut done = HashSet::new();

        let mut todo = vec![start];

        while let Some(t) = todo.pop() {
            out.push(t.to_string());
            if done.contains(t) {
                continue;
            }
            done.insert(t);

            for v in sub_classes.values() {
                for sc in v {
                    if sc.clazz == t {
                        todo.push(&sc.sub_class);
                    }
                }
            }
        }

        out
    }

    async fn find_snippets(
        &self,
        ctx: &ShapeCompletionCtx<'_>,
        range: Range,
    ) -> Vec<SimpleCompletion> {
        if let Some(token) = ctx
            .state
            .tokens
            .current
            .iter()
            .position(|x| x.1.contains(&ctx.loc))
        {
            if token > 0 {
                let prev_token = ctx.state.tokens.current[token - 1].value();
                match prev_token {
                    Token::PredType => {
                        let props = self.state.shapes.lock().await;

                        return props
                            .values()
                            .flatten()
                            .filter(|prop| prop.clazz.starts_with(ctx.prefix_url))
                            .flat_map(|prop: &Shape| {
                                info!("Looking into shape {}", prop.clazz);
                                prop.into_completion(&ctx.turtle, range)
                            })
                            .collect();
                    }
                    _ => {}
                }
            }
        }
        vec![]
    }

    async fn find_class_properties(
        &self,
        ctx: &PropertyCompletionCtx<'_>,
        range: Range,
    ) -> Option<Vec<SimpleCompletion>> {
        let parents = &ctx.state.parents.last_valid;

        info!("Parent count {}", parents.iter().count());
        for n in parents.iter() {
            let span = n.1.span();
            info!(
                "{}: Span {:?} contains loc {} {}",
                n.1.ty_str(),
                span,
                ctx.loc,
                span.contains(&ctx.loc)
            );
        }

        let (idx, _) = parents
            .iter()
            .filter(|(_, Spanned(_, span))| span.contains(&ctx.loc))
            .min_by_key(|(_, Spanned(_, span))| span.end - span.start)?;

        info!("Found item {:?}", parents[idx]);

        let po = if let crate::lang::turtle::node::Node::Triple { po, .. } = &parents[idx].0 {
            po
        } else {
            parents
                .parent_iter(idx)
                .flat_map(|e| match &e.1 .0 {
                    crate::lang::turtle::node::Node::Triple { po, .. } => Some(po),
                    _ => None,
                })
                .next()?
        };

        info!("Found po {:?}", po);

        let mut classes = HashSet::new();

        for start in po
            .iter()
            .filter(|x| match parents.objects[x.predicate].0 {
                crate::lang::turtle::node::Node::Leaf(Leaf::NamedNode(NamedNode::A)) => true,
                _ => false,
            })
            .flat_map(|x| x.objects.iter().cloned().map(|x| &parents.objects[x].0))
            .filter_map(|o| match o {
                crate::lang::turtle::node::Node::Leaf(Leaf::NamedNode(nn)) => Some(nn),
                _ => None,
            })
            .filter_map(|nn| nn.expand(&ctx.state.element.last_valid))
        {
            classes.extend(self.find_subclasses(&start).await);
        }
        info!("Found classes {:?}", classes);

        let props = self.state.shapes.lock().await;
        return props
            .values()
            .flatten()
            .filter(|prop| classes.contains(&prop.clazz))
            .flat_map(|shape: &Shape| {
                shape
                    .properties
                    .iter()
                    .map(|x| x.into_completion(&ctx.state.element.last_valid, range.clone()))
            })
            .collect::<Vec<_>>()
            .into();
    }
}

#[async_trait::async_trait]
impl<'a> CompletionProvider<PropertyCompletionCtx<'a>> for ShapeCompletionProvider {
    async fn find_completions(
        &self,
        ctx: &PropertyCompletionCtx<'a>,
        range: Range,
    ) -> Vec<SimpleCompletion> {
        info!("Completion provider property completion! {:?}", range);
        self.find_class_properties(ctx, range)
            .await
            .unwrap_or_default()
    }
}

#[async_trait::async_trait]
impl<'a> CompletionProvider<ShapeCompletionCtx<'a>> for ShapeCompletionProvider {
    async fn find_completions(
        &self,
        ctx: &ShapeCompletionCtx<'a>,
        range: Range,
    ) -> Vec<SimpleCompletion> {
        self.find_snippets(ctx, range.clone()).await
    }
}
