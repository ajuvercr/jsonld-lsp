use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
    pin::Pin,
    sync::Arc,
};

use crate::{
    lang::turtle::{self, NamedNode, Turtle},
    model::spanned,
    triple::{extract_triples, Triple},
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

#[derive(Debug, Clone, Default)]
pub struct Property {
    pub id: String,
    pub label: Option<String>,
    pub comment: Option<String>,
    pub ty: PropertyType,
}

impl Property {
    pub fn short(&self, prefix: &str) -> String {
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
enum Source {
    Http(lsp_types::Url),
    File(lsp_types::Url),
}

impl Source {
    pub fn from_str(uri: &str) -> Option<Self> {
        let mut uri = lsp_types::Url::parse(uri).ok()?;
        uri.set_query(None);
        uri.set_fragment(None);

        match uri.scheme() {
            "file" => Some(Source::File(uri)),
            "http" | "https" => Some(Source::Http(uri)),
            _ => None,
        }
    }
    pub fn from_url(uri: &lsp_types::Url) -> Option<Self> {
        let mut url = uri.clone();
        url.set_query(None);
        url.set_fragment(None);
        match url.scheme() {
            "file" => Some(Source::File(url)),
            "http" | "https" => Some(Source::Http(url)),
            _ => None,
        }
    }
}

impl Deref for Source {
    type Target = lsp_types::Url;

    fn deref(&self) -> &Self::Target {
        match self {
            Source::Http(x) => x,
            Source::File(x) => x,
        }
    }
}

struct FoundProperty {
    property: Property,
    source: Source,
    deleted: bool,
}

impl FoundProperty {
    fn new(property: Property, source: Source) -> Self {
        Self {
            property,
            source,
            deleted: false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Prefixes {
    names: Arc<HashMap<String, String>>,
    sources: Arc<Mutex<Vec<Source>>>,
    found_properties: Arc<Mutex<Vec<FoundProperty>>>,
}

fn parse(str: &str, location: &lsp_types::Url) -> Result<Turtle, String> {
    let parser = turtle::tokenizer::parse_tokens();

    let tokens = parser
        .parse(str)
        .map_err(|_| String::from("Tokenizing failed"))?;
    info!("{} tokens", tokens.len());

    let stream = chumsky::Stream::from_iter(
        0..str.len() + 1,
        tokens.into_iter().filter(|x| !x.0.is_comment()),
    );

    let parser =
        turtle::turtle(&location).then_ignore(end().recover_with(skip_then_retry_until([])));

    let (turtle, errors) = parser.parse_recovery(stream);
    info!(?errors);

    let mut turtle = turtle.ok_or(String::from("Not valid turtle"))?;

    if turtle.base.is_none() {
        let nn = NamedNode::Full(location.to_string());
        // iew
        turtle.base = Some(spanned(turtle::Base(0..1, spanned(nn, 0..1)), 0..1));
    }

    Ok(turtle)
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
fn extract_properties(turtle: &Turtle) -> Vec<Property> {
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

        properties.push(Property {
            id,
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
            sources: Default::default(),
            found_properties: Default::default(),
        })
    }

    pub fn get<'a>(&'a self, prefix: &str) -> Option<&'a str> {
        self.names.get(prefix).map(|x| x.as_str())
    }

    pub fn get_all<'a>(&'a self) -> impl Iterator<Item = &'a str> {
        self.names.keys().map(|x| x.as_str())
    }

    pub async fn update<'a>(&'a self, url: &lsp_types::Url, turtle: &Turtle) -> Option<()> {
        let source = Source::from_url(url)?;
        let props = extract_properties(turtle);

        let mut prop_iter = props
            .into_iter()
            .map(|x| FoundProperty::new(x, source.clone()));

        let mut found = self.found_properties.lock().await;

        for existing_prop in found.iter_mut() {
            if source.deref() == url {
                if let Some(p) = prop_iter.next() {
                    *existing_prop = p;
                } else {
                    existing_prop.deleted = true;
                }
            }
        }

        found.extend(prop_iter);

        Some(())
    }

    pub async fn fetch<'a>(
        &'a self,
        url_str: &str,
        msg: impl Fn(String) -> Pin<Box<dyn std::future::Future<Output = ()> + Send + 'a>>,
        mut f: impl FnMut(&Property),
    ) -> Result<(), String> {
        let mut url = lsp_types::Url::parse(url_str).map_err(|x| format!("invalid uri {}", x))?;
        url.set_fragment(None);
        url.set_query(None);

        let mut sources = self.sources.lock().await;
        let fetched = sources.iter().find(|x| url.eq(&x)).is_some();
        if fetched {
            drop(sources);
            info!("Found properties for {} in cache", url,);
            let props = self.found_properties.lock().await;
            props
                .iter()
                .filter(|prop| prop.property.id.starts_with(url_str))
                .for_each(|x| f(&x.property));

            return Ok(());
        }

        sources.push(Source::from_url(&url).ok_or("Incorrect scheme".to_string())?);
        drop(sources);

        info!("Properties not found in cache, fetching {}", url,);

        let headers = [("Accept".to_string(), "text/turtle".to_string())].into();
        let resp = fetch(&url_str, &headers).await?;
        info!(?resp.status, len = resp.body.len());
        msg(format!(
            "Fetch successful {} ({} bytes)",
            url,
            resp.body.len()
        ))
        .await;

        // Parse the turtle
        let turtle = parse(&resp.body, &url)?;
        msg(format!("Parse successful {}", url,)).await;
        let _ = self.update(&url, &turtle).await;

        Ok(())
    }
}
