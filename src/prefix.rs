use std::{
    collections::{HashMap, HashSet},
    pin::Pin,
    sync::Arc,
};

use crate::{
    lang::turtle::{self, BlankNode, NamedNode, Turtle},
    model::spanned,
    utils::fetch,
};
use chumsky::{primitive::end, recovery::skip_then_retry_until, Parser};
use futures::lock::Mutex;
use lsp_types::{lsp_request, CompletionItemKind};
use tracing::info;

#[derive(Debug, Default, Clone, Copy)]
pub enum PropertyType {
    Class,
    DatatypeProperty,
    ObjectProperty,
    #[default]
    Property,
}

#[derive(Debug, Clone, Default)]
pub struct Property {
    pub id: String,
    pub short: String,
    pub label: Option<String>,
    pub comment: Option<String>,
    pub ty: PropertyType,
}

impl Into<CompletionItemKind> for PropertyType {
    fn into(self) -> CompletionItemKind {
        match self {
            PropertyType::Class => CompletionItemKind::CLASS,
            PropertyType::DatatypeProperty => CompletionItemKind::PROPERTY,
            PropertyType::ObjectProperty => CompletionItemKind::PROPERTY,
            PropertyType::Property => CompletionItemKind::PROPERTY,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Prefixes {
    names: Arc<HashMap<String, String>>,
    properties: Arc<Mutex<HashMap<String, Result<Vec<Property>, String>>>>,
}

fn parse(str: &str, location: &str) -> Result<Turtle, String> {
    let parser = turtle::tokenizer::parse_tokens();

    let tokens = parser
        .parse(str)
        .map_err(|_| String::from("Tokenizing failed"))?;
    info!("{} tokens", tokens.len());

    let stream = chumsky::Stream::from_iter(
        0..str.len() + 1,
        tokens.into_iter().filter(|x| !x.0.is_comment()),
    );

    let parser = turtle::turtle().then_ignore(end().recover_with(skip_then_retry_until([])));

    let (turtle, errors) = parser.parse_recovery(stream);
    info!(?errors);

    let mut turtle = turtle.ok_or(String::from("Not valid turtle"))?;

    if turtle.base.is_none() {
        let nn = NamedNode::Full(location.into());
        // iew
        turtle.base = Some(spanned(turtle::Base(0..1, spanned(nn, 0..1)), 0..1));
    }

    Ok(turtle)
}

struct Triple {
    subject: String,
    predicate: String,
    object: String,
}

const TYPE: &'static str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

const PROPERTIES: &'static [(&'static str, PropertyType)] = &[
    (
        "http://www.w3.org/2002/07/owl#DatatypeProperty",
        PropertyType::DatatypeProperty,
    ),
    (
        "http://www.w3.org/2002/07/owl#ObjectProperty",
        PropertyType::ObjectProperty,
    ),
    ("http://www.w3.org/2002/07/owl#Class", PropertyType::Class),
    (
        "http://www.w3.org/2000/01/rdf-schema#Class",
        PropertyType::Class,
    ),
    (
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property",
        PropertyType::Property,
    ),
];

#[tracing::instrument(skip(turtle))]
fn extract_properties(turtle: &Turtle, prefix: &str, base: &str) -> Vec<Property> {
    info!( triples = turtle.triples.len(), ?turtle.base);
    let mut triples: Vec<Triple> = Vec::new();

    let mut found = HashSet::new();

    for triple in &turtle.triples {
        let subject = match &triple.subject.0 {
            turtle::Subject::BlankNode(BlankNode::Named(x)) => x.clone(),
            turtle::Subject::NamedNode(n) => {
                if let Some(x) = n.expand(&turtle, base) {
                    info!("subject {:?} -> {}", n, x);
                    x
                } else {
                    continue;
                }
            }
            _ => continue,
        };

        for po in &triple.po {
            if let Some(pred) = po.predicate.expand(&turtle, base) {
                for ob in &po.object {
                    let object = match &ob.0 {
                        turtle::Term::Literal(s) => s.plain_string(),
                        turtle::Term::BlankNode(BlankNode::Named(x)) => x.clone(),
                        turtle::Term::NamedNode(n) => {
                            if let Some(o) = n.expand(&turtle, base) {
                                o
                            } else {
                                continue;
                            }
                        }
                        _ => continue,
                    };
                    triples.push(Triple {
                        subject: subject.clone(),
                        predicate: pred.clone(),
                        object,
                    });
                }
            } else {
                continue;
            }
        }
    }

    let prefix_url = lsp_types::Url::parse(prefix);
    info!(?turtle.base, triples = triples.len());

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

        let short = if let Some(short) = id.strip_prefix(prefix) {
            short.to_string()
        } else {
            match (lsp_types::Url::parse(&id), &prefix_url) {
                (Ok(id_url), Ok(prefix_url)) => prefix_url
                    .make_relative(&id_url)
                    .unwrap_or_else(|| id[prefix.len()..].to_string()),
                _ => id[prefix.len()..].to_string(),
            }
        };

        info!("Crash result {}", short);

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

        properties.push(Property {
            id,
            short,
            label,
            comment,
            ty,
        });
    }

    properties
}

impl Prefixes {
    async fn fetch_prefix() -> Option<HashMap<String, String>> {
        let headers = [].into();
        let resp = fetch("http://prefix.cc/context", &headers).await.ok()?;

        let context: HashMap<String, HashMap<String, String>> =
            serde_json::from_str(&resp.body).ok()?;
        let names = context.into_iter().find(|x| x.0 == "@context")?.1;

        Some(names)
    }

    fn default_prefix() -> HashMap<String, String> {
        let prefix_cc_str = include_str!("../prefix_cc.json");
        let out = serde_json::from_str(&prefix_cc_str).unwrap();
        out
    }

    pub async fn new() -> Option<Prefixes> {
        // let names = Self::fetch_prefix()
        //     .await
        //     .unwrap_or_else(Self::default_prefix);

        let names = Self::default_prefix();

        Some(Prefixes {
            names: names.into(),
            properties: Default::default(),
        })
    }

    pub fn get<'a>(&'a self, prefix: &str) -> Option<&'a str> {
        self.names.get(prefix).map(|x| x.as_str())
    }

    pub fn get_all<'a>(&'a self) -> impl Iterator<Item = &'a str> {
        self.names.keys().map(|x| x.as_str())
    }

    pub async fn update<'a>(
        &'a self,
        url: &str,
        turtle: &Turtle,
        alias: Option<&str>,
        f: impl FnOnce(&[Property]),
    ) {
        let properties = extract_properties(turtle, url, url);
        f(&properties);

        let mut props = self.properties.lock().await;
        if let Some(alias) = alias {
            props.insert(alias.to_string(), Ok(properties.clone()));
        }

        props.insert(url.to_string(), Ok(properties));
    }

    pub async fn fetch<'a>(
        &'a self,
        url: &str,
        f: impl FnOnce(&[Property]),
        msg: impl Fn(String) -> Pin<Box<dyn std::future::Future<Output = ()> + Send + 'a>>,
    ) -> Result<(), String> {
        {
            let props = self.properties.lock().await;
            if let Some(out) = props.get(url) {
                info!(
                    "Found properties for {} in cache (found = {})",
                    url,
                    out.is_ok()
                );
                f(out.as_ref()?);
                return Ok(());
            }
        }

        if let Ok(mut url_parsed) = lsp_types::Url::parse(url) {
            url_parsed.set_fragment(None);
            url_parsed.set_query(None);
            {
                let url_str = url_parsed.to_string();
                let props = self.properties.lock().await;
                if let Some(out) = props.get(&url_str) {
                    info!(
                        "Found properties for {} in cache (found = {})",
                        url,
                        out.is_ok()
                    );
                    f(out.as_ref()?);
                    return Ok(());
                }
            }
        }

        msg(format!("Properties not found in cache, fetching {}", url,)).await;
        info!("Properties not found in cache, fetching {}", url,);

        info!(%url);

        let headers = [("Accept".to_string(), "text/turtle".to_string())].into();
        let resp = match fetch(&url, &headers).await {
            Ok(resp) => {
                info!(?resp.status, len = resp.body.len());
                msg(format!(
                    "Fetch successful {} ({} bytes)",
                    url,
                    resp.body.len()
                ))
                .await;
                resp
            }
            Err(e) => {
                let mut props = self.properties.lock().await;
                props.insert(url.to_string(), Err(e.clone()));

                msg(format!("Failed to fetch {}", url,)).await;
                return Err(e);
            }
        };

        // Parse the turtle
        let out = match parse(&resp.body, &url) {
            Ok(turtle) => {
                msg(format!("Parse successful {}", url,)).await;
                let base = turtle.get_base(url);
                let alias = (base.to_string() != url).then_some(base);
                self.update(url, &turtle, alias.as_ref().map(|x| x.as_str()), f)
                    .await;
                Ok(())
            }
            Err(s) => {
                msg(format!("Failed to parse {}", url,)).await;
                let mut props = self.properties.lock().await;
                props.insert(url.to_string(), Err(s.clone()));
                Err(s)
            }
        };

        out
    }
}
