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
use lsp_types::CompletionItemKind;
use tracing::info;

#[derive(Debug, Default, Clone, Copy)]
pub enum PropertyType {
    Class,
    DatatypeProperty,
    ObjectProperty,
    #[default]
    Property,
}

#[derive(Debug, Default)]
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

fn extract_properties(turtle: Turtle, prefix: &str) -> Vec<Property> {
    info!( triples = turtle.triples.len(), ?turtle.base);
    let mut triples: Vec<Triple> = Vec::new();

    let mut found = HashSet::new();

    for triple in &turtle.triples {
        let subject = match &triple.subject.0 {
            turtle::Subject::BlankNode(BlankNode::Named(x)) => x.clone(),
            turtle::Subject::NamedNode(n) => {
                if let Some(x) = n.expand(&turtle) {
                    x
                } else {
                    continue;
                }
            }
            _ => continue,
        };

        for po in &triple.po {
            if let Some(pred) = po.predicate.expand(&turtle) {
                for ob in &po.object {
                    let object = match &ob.0 {
                        turtle::Term::Literal(s) => s.plain_string(),
                        turtle::Term::BlankNode(BlankNode::Named(x)) => x.clone(),
                        turtle::Term::NamedNode(n) => {
                            if let Some(o) = n.expand(&turtle) {
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

        let short = id[prefix.len()..].to_string();

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
    pub async fn new() -> Option<Prefixes> {
        let headers = [].into();
        info!("Fetching");
        let resp = fetch("http://prefix.cc/context", &headers).await.ok()?;
        info!("Fetched");

        let context: HashMap<String, HashMap<String, String>> =
            serde_json::from_str(&resp.body).ok()?;
        let names = context.into_iter().find(|x| x.0 == "@context")?.1;

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

    pub async fn fetch<'a>(
        &'a self,
        url: &str,
        f: impl FnOnce(&[Property]),
        msg: impl Fn(String) -> Pin<Box<dyn std::future::Future<Output = ()> + Send + 'a>>,
    ) -> Result<(), String> {
        {
            let props = self.properties.lock().await;
            if let Some(out) = props.get(url) {
                msg(format!(
                    "Found properties for {} in cache (found = {})",
                    url,
                    out.is_ok()
                ))
                .await;
                f(out.as_ref()?);
                return Ok(());
            }
            info!("Should drop props");
            drop(props);
            // Drop props lock before the request
        }

        msg(format!("Properties not found in cache, fetching {}", url,)).await;

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
        let properties = parse(&resp.body, &url).map(|x| extract_properties(x, url));

        let out = match properties.as_ref() {
            Ok(p) => {
                msg(format!("Parse successful {}", url,)).await;
                f(&p);
                Ok(())
            }
            Err(e) => {
                msg(format!("Failed to parse {}", url,)).await;
                Err(e.clone())
            }
        };

        info!("Locking properties");
        let mut props = self.properties.lock().await;
        props.insert(url.to_string(), properties);

        out
    }
}
