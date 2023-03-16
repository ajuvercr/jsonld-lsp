use futures::future::{BoxFuture, FutureExt};
use iref::{Iri, IriBuf, IriRef, IriRefBuf};
use json_ld::{Loader, RemoteDocument};
use json_ld_syntax::{Parse, Value};
use locspan::{Location, Meta, Span};
use rdf_types::{vocabulary::Index, IriVocabulary};
use std::fmt;
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use tower_lsp::lsp_types::MessageType;
use tower_lsp::Client;

use crate::Documents;

/// Loading error.
#[derive(Debug)]
pub enum Error {
    /// No mount point found for the given IRI.
    NoMountPoint,

    /// IO error.
    IO(std::io::Error),

    /// Parse error.
    ParseError,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::NoMountPoint => write!(f, "no mount point"),
            Self::IO(e) => e.fmt(f),
            Self::ParseError => write!(f, "parse error"),
        }
    }
}

pub type V = Value<Span>;
pub type E = Meta<json_ld_syntax::parse::Error<Span>, Span>;
pub type Parsed = Result<Meta<V, Span>, E>;
/// Dynamic parser type.
type DynParser<'a, I, M, T, E> = dyn 'static
    + Send
    + Sync
    + FnMut(&'a dyn IriVocabulary<Iri = I>, &I, &str) -> Result<Meta<T, M>, E>;

#[derive(Debug)]
pub struct LocalCtx<'a> {
    root: PathBuf,
    documents: &'a Documents,
}

impl<'a> LocalCtx<'a> {
    pub fn new(root: &'a str, documents: &'a Documents) -> Option<Self> {
        let root = root.strip_prefix("file://").unwrap_or(root);
        let mut root = PathBuf::from_str(root).ok()?;
        if root.is_file() {
            root.pop();
        }
        Self { root, documents }.into()
    }

    fn try_from_buffer(&self, path: &str) -> Option<Parsed> {
        let x = self.documents.get(path)?;
        let x = x.2.as_ref()?;

        Some(json_syntax::Value::parse_str(x, |span| span))
    }

    pub async fn load(
        &self,
        url: &str,
        client: &Client,
    ) -> Result<RemoteDocument<std::path::PathBuf, Span, V>, Error> {
        let mut filepath = self.root.clone();
        filepath.push(url);

        let buffer_path = format!("file://{}", filepath.display());

        if let Some(Ok(doc)) = self.try_from_buffer(&buffer_path) {
            client
                .log_message(MessageType::INFO, "From documents")
                .await;
            return Ok(RemoteDocument::new(
                Some(filepath),
                Some("application/ld+json".parse().unwrap()),
                doc,
            ));
        }

        let file = File::open(&filepath).map_err(Error::IO)?;
        let mut buf_reader = BufReader::new(file);
        let mut contents = String::new();

        buf_reader
            .read_to_string(&mut contents)
            .map_err(Error::IO)?;

        client.log_message(MessageType::INFO, "File read!").await;

        let doc = Value::parse_str(&contents, |span| span).map_err(|_| Error::ParseError)?;

        Ok(RemoteDocument::new(
            Some(filepath),
            Some("application/ld+json".parse().unwrap()),
            doc,
        ))
    }
}

