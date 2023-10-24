use std::{collections::HashMap, sync::Arc};

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
    properties: Arc<Mutex<HashMap<String, Option<Vec<Property>>>>>,
}

fn parse(str: &str, location: &str) -> Option<Turtle> {
    let parser = turtle::tokenizer::parse_tokens();

    let tokens = parser.parse(str).ok()?;
    info!("{} tokens", tokens.len());

    let stream = chumsky::Stream::from_iter(
        0..str.len() + 1,
        tokens.into_iter().filter(|x| !x.0.is_comment()),
    );

    let parser = turtle::turtle().then_ignore(end().recover_with(skip_then_retry_until([])));

    let (turtle, errors) = parser.parse_recovery(stream);
    info!(?errors);

    let mut turtle = turtle?;

    if turtle.base.is_none() {
        let nn = NamedNode::Full(location.into());
        // iew
        turtle.base = Some(spanned(turtle::Base(0..1, spanned(nn, 0..1)), 0..1));
    }
    info!("turtle {:?}", turtle);

    Some(turtle)
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
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#Class",
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
        let resp = fetch("http://prefix.cc/context", &headers).await.ok()?;

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

    pub async fn fetch<'a>(&'a self, prefix: &str, f: impl FnOnce(&[Property])) -> Option<()> {
        let url = self.get(prefix)?;
        {
            let props = self.properties.lock().await;
            if let Some(out) = props.get(prefix) {
                f(out.as_ref()?);
                return Some(());
            }
            // Drop props lock before the request
        }

        info!(%url);
        let headers = [("Accept".to_string(), "text/turtle".to_string())].into();
        let resp = if let Some(resp) = fetch(&url, &headers).await.ok() {
            info!(?resp.status, len = resp.body.len());
            resp
        } else {
            info!("Fetch failed");
            let mut props = self.properties.lock().await;
            props.insert(prefix.to_string(), None);
            return None;
        };

        // Parse the turtle
        let properties = parse(&resp.body, &url).map(|x| extract_properties(x, url));

        let out = if let Some(p) = properties.as_ref() {
            f(&p);
            Some(())
        } else {
            None
        };

        let mut props = self.properties.lock().await;
        props.insert(prefix.to_string(), properties);

        out
    }
}
