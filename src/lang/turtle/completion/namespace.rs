use futures::lock::Mutex;
use futures::FutureExt;
use lsp_types::Range;
use std::sync::Arc;

use crate::{
    lang::SimpleCompletion,
    triple::{extract_triples, Triple},
    utils::{fetch, turtle::*},
};

use super::{CompletionProvider, Turtle};
use hashbrown::{HashMap, HashSet};
use tracing::info;

#[derive(Debug)]
pub struct NsCompletionCtx {
    pub prefix: String,
    pub prefix_url: String,
}

pub struct NamespaceCompletion {
    id: String,
    label: Option<String>,
    comment: Option<String>,
    ty: PropertyType,
}

impl NamespaceCompletion {
    fn short(&self, prefix: &str) -> String {
        let id = &self.id;
        if let Some(short) = id.strip_prefix(prefix) {
            short.to_string()
        } else {
            info!("did not strip prefix  {} {}", id, prefix);
            match (
                lsp_types::Url::parse(&self.id),
                lsp_types::Url::parse(prefix),
            ) {
                (Ok(id_url), Ok(prefix_url)) => prefix_url
                    .make_relative(&id_url)
                    .unwrap_or_else(|| id[prefix.len()..].to_string()),
                _ => id[prefix.len()..].to_string(),
            }
        }
    }

    fn into_completion(&self, ctx: &NsCompletionCtx, range: lsp_types::Range) -> SimpleCompletion {
        let short = self.short(&ctx.prefix_url);
        let new_text = format!("{}:{}", ctx.prefix, short);
        let edits = vec![lsp_types::TextEdit {
            new_text: new_text.clone(),
            range: range.clone(),
        }];

        let docs = match (&self.comment, &self.label) {
            (Some(comment), Some(label)) => {
                format!("{}: {}", label, comment)
            }
            (None, Some(label)) => label.clone(),
            (Some(comment), None) => format!("{}: {}", short, comment),
            (None, None) => short.clone(),
        };

        SimpleCompletion {
            kind: self.ty.into(),
            label: new_text,
            documentation: Some(docs),
            sort_text: None,
            filter_text: None,
            edits,
        }
    }
}

pub type NamespaceCompletionProviderState = Arc<Mutex<HashMap<String, Vec<NamespaceCompletion>>>>;
#[derive(Clone)]
pub struct NamespaceCompletionProvider {
    found_properties: NamespaceCompletionProviderState,
}
impl NamespaceCompletionProvider {
    pub fn new(state: &NamespaceCompletionProviderState) -> Self {
        Self {
            found_properties: state.clone(),
        }
    }

    pub async fn update(
        &self,
        turtle: &Turtle,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send>> {
        let prefixes: Vec<_> = {
            let props = self.found_properties.lock().await;
            turtle
                .prefixes
                .iter()
                .flat_map(|x| x.value.expand(turtle))
                .filter(|x| !props.contains_key(x))
                .collect()
        };

        do_update(turtle, self).await;

        let this = self.clone();
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

                let mut props = this.found_properties.lock().await;
                props.insert(prefix.to_string(), vec![]);
            }
        }
        .boxed()
    }
}

#[async_trait::async_trait]
impl CompletionProvider<NsCompletionCtx> for NamespaceCompletionProvider {
    async fn find_completions(&self, ctx: &NsCompletionCtx, range: Range) -> Vec<SimpleCompletion> {
        let props = self.found_properties.lock().await;

        props
            .iter()
            .filter(|(l, _)| l.starts_with(&ctx.prefix_url))
            .flat_map(|(_, xs)| xs.iter())
            .map(|prop: &NamespaceCompletion| prop.into_completion(ctx, range.clone()))
            .collect()
    }
}

async fn do_update(turtle: &Turtle, this: &NamespaceCompletionProvider) {
    let url = turtle.set_base.to_string();
    let properties = extract_properties(turtle);

    let mut props = this.found_properties.lock().await;
    props.insert(url, properties);
}

#[tracing::instrument(skip(turtle))]
fn extract_properties(turtle: &Turtle) -> Vec<NamespaceCompletion> {
    info!( triples = turtle.triples.len(), ?turtle.base);
    let triples: Vec<Triple> = extract_triples(turtle);

    info!(?turtle.base, triples = triples.len());

    let mut found = HashSet::new();
    let mut properties = Vec::new();
    for subj in triples.iter().filter(|x| x.predicate == TYPE) {
        let ty = if let Some(ty) = PROPERTIES.iter().find(|x| x.0 == subj.object.as_str()) {
            ty.1
        } else {
            continue;
        };

        let id = subj.subject.clone();
        if found.contains(&id) {
            continue;
        }
        found.insert(id.clone());

        let label = triples
            .iter()
            .find(|x| {
                x.subject == id.as_str()
                    && x.predicate == "http://www.w3.org/2000/01/rdf-schema#label"
            })
            .map(|x| x.object.clone());
        let comment = triples
            .iter()
            .find(|x| {
                x.subject == id.as_str()
                    && x.predicate == "http://www.w3.org/2000/01/rdf-schema#comment"
            })
            .map(|x| x.object.clone());

        properties.push(NamespaceCompletion {
            id,
            label,
            comment,
            ty,
        });
    }

    properties
}
