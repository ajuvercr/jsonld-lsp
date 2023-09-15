////! Simple document and context loader based on [`reqwest`](https://crates.io/crates/reqwest)
use bytes::Bytes;
use futures::future::{BoxFuture, FutureExt};
use hashbrown::HashSet;
use iref::Iri;
use json_ld::{Loader, Profile, RemoteDocument};
use locspan::{Meta, Span};
use mime::Mime;
use once_cell::sync::OnceCell;
use rdf_types::{vocabulary::Index, IriVocabulary, IriVocabularyMut};
use reqwest::StatusCode;
use std::{collections::HashMap, fmt, hash::Hash, str::FromStr, string::FromUtf8Error};

use super::fetch::fetch;

/// Loader options.
pub struct Options<I> {
    /// One or more IRIs to use in the request as a profile parameter.
    ///
    /// (See [IANA Considerations](https://www.w3.org/TR/json-ld11/#iana-considerations)).
    pub request_profile: Vec<Profile<I>>,

    /// Maximum number of allowed redirections before the loader fails.
    ///
    /// Defaults to 8.
    pub max_redirections: usize,
}

impl<I> Default for Options<I> {
    fn default() -> Self {
        Self {
            request_profile: Vec::new(),
            max_redirections: 8,
        }
    }
}
/// Loading error.
#[derive(Debug)]
pub enum Error<E> {
    Reqwest(reqwest::Error),

    /// The server returned a `303 See Other` redirection status code.
    Redirection303,

    MissingRedirectionLocation,

    InvalidRedirectionUrl(iref::Error),

    QueryFailed(StatusCode),

    InvalidContentType,

    MultipleContextLinkHeaders,

    TooManyRedirections,

    Parse(E),
}

/// `reqwest`-based loader
/// Dynamic parser type.
type DynParser<I, M, T, E> = dyn 'static
    + Send
    + Sync
    + FnMut(&dyn IriVocabulary<Iri = I>, &I, Bytes) -> Result<Meta<T, M>, E>;

///
/// Only works with the [`tokio`](https://tokio.rs/) runtime.
///
/// The loader will follow indirections and `Link` headers.
///
/// Loaded documents are not cached: a new network query is made each time
/// an URL is loaded even if it has already been queried before.
pub struct ReqwestLoader<
    I = Index,
    M = locspan::Location<I>,
    T = json_ld_syntax::Value<M>,
    E = ParseError<M>,
> {
    parser: Box<DynParser<I, M, T, E>>,
    options: Options<I>,
    data: OnceCell<Data>,
}

impl<I: AsRef<str>, M, T, E> std::fmt::Debug for ReqwestLoader<I, M, T, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ReqwestLoader")
    }
}

impl<I, M, T, E> ReqwestLoader<I, M, T, E> {
    /// Creates a new loader with the given parsing function.
    pub fn new(
        parser: impl 'static
            + Send
            + Sync
            + FnMut(&dyn IriVocabulary<Iri = I>, &I, Bytes) -> Result<Meta<T, M>, E>,
    ) -> Self {
        Self::new_using(parser, Options::default())
    }

    /// Creates a new leader with the given parsing function and options.
    pub fn new_using(
        parser: impl 'static
            + Send
            + Sync
            + FnMut(&dyn IriVocabulary<Iri = I>, &I, Bytes) -> Result<Meta<T, M>, E>,
        options: Options<I>,
    ) -> Self {
        Self {
            parser: Box::new(parser),
            options,
            data: OnceCell::new(),
        }
    }
}

/// Precomputed data.
struct Data {
    accept_header: String,
}

impl Data {
    fn new<I>(options: &Options<I>, vocabulary: &impl IriVocabulary<Iri = I>) -> Self {
        let mut json_ld_params = String::new();

        if !options.request_profile.is_empty() {
            json_ld_params.push_str("; profile=");

            if options.request_profile.len() > 1 {
                json_ld_params.push('"');
            }

            for (i, p) in options.request_profile.iter().enumerate() {
                if i > 0 {
                    json_ld_params.push(' ');
                }

                json_ld_params.push_str(p.iri(vocabulary).as_str());
            }

            if options.request_profile.len() > 1 {
                json_ld_params.push('"');
            }
        }

        Self {
            accept_header: format!("application/ld+json{json_ld_params}, application/json"),
        }
    }
}

impl<I: Clone, M> ReqwestLoader<I, M, json_ld_syntax::Value<M>, ParseError<M>> {
    /// Creates a new loader with the default parser and given metadata map
    /// function.
    pub fn new_with_metadata_map(
        f: impl 'static + Send + Sync + Fn(&dyn IriVocabulary<Iri = I>, &I, Span) -> M,
    ) -> Self {
        use json_syntax::Parse;
        Self::new(move |vocab, file: &I, bytes| {
            let content = String::from_utf8(bytes.to_vec()).map_err(ParseError::InvalidEncoding)?;
            json_syntax::Value::parse_str(&content, |span| f(vocab, file, span))
                .map_err(ParseError::Json)
        })
    }
}

impl<I: Clone> Default for ReqwestLoader<I, Span, json_ld_syntax::Value<Span>, ParseError<Span>> {
    fn default() -> Self {
        Self::new_with_metadata_map(|_, _file, span| span)
    }
}

/// HTTP body parse error.
#[derive(Debug)]
pub enum ParseError<M> {
    /// Invalid encoding.
    InvalidEncoding(FromUtf8Error),

    /// JSON parse error.
    Json(json_ld_syntax::parse::MetaError<M>),
}

impl<M> fmt::Display for ParseError<M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidEncoding(_) => write!(f, "invalid encoding"),
            Self::Json(e) => e.fmt(f),
        }
    }
}

impl<I: Clone + Eq + Hash + Sync + Send + AsRef<str>, T: Clone + Send, M: Send, E> Loader<I, M>
    for ReqwestLoader<I, M, T, E>
{
    type Output = T;
    type Error = Error<E>;

    fn load_with<'a>(
        &'a mut self,
        vocabulary: &'a mut (impl Send + Sync + IriVocabularyMut<Iri = I>),
        mut url: I,
    ) -> BoxFuture<'a, Result<RemoteDocument<I, M, T>, Self::Error>>
    where
        I: 'a,
    {
        async move {
            // if let Some(bytes) = self.cache.get(&url) {
            //     let document =
            //         (*self.parser)(vocabulary, &url, bytes.clone()).map_err(Error::Parse)?;
            //
            //     let document = RemoteDocument::new(
            //         Some(url),
            //         Some(Mime::from_str("application/json+ld").unwrap()),
            //         document,
            //     );
            //
            //     return Ok(document);
            // }

            let data = self
                .data
                .get_or_init(|| Data::new(&self.options, vocabulary));

            let mut redirection_number = 0;

            loop {
                if redirection_number > self.options.max_redirections {
                    return Err(Error::TooManyRedirections);
                }
                let mut headers = HashMap::new();
                headers.insert("accept".to_string(), data.accept_header.to_string());

                let resp = fetch(url.as_ref(), &headers).await.unwrap();

                let status = StatusCode::from_u16(resp.status).unwrap();

                match status {
                    StatusCode::OK => {
                        let profile = HashSet::new();

                        let bytes = Bytes::from(resp.body.into_bytes());

                        let document = (*self.parser)(vocabulary, &url, bytes.clone())
                            .map_err(Error::Parse)?;

                        // self.cache.insert(url.clone(), bytes);

                        let document = RemoteDocument::new_full(
                            Some(url),
                            Some(Mime::from_str("application/json+ld").unwrap()),
                            None,
                            profile,
                            document,
                        );

                        break Ok(document);

                        // match content_types.find(ContentType::is_json_ld) {
                        //     Some(content_type) => {
                        //     }
                        //     }
                        //     None => {
                        //         log::debug!("no valid media type found");
                        //         for link in response.headers().get_all(LINK).into_iter() {
                        //             if let Some(link) = Link::new(link) {
                        //                 if link.rel() == Some(b"alternate")
                        //                     && link.type_() == Some(b"application/ld+json")
                        //                 {
                        //                     log::debug!("link found");
                        //                     let u =
                        //                         link.href().resolved(vocabulary.iri(&url).unwrap());
                        //                     url = vocabulary.insert(u.as_iri());
                        //                     redirection_number += 1;
                        //                     continue 'next_url;
                        //                 }
                        //             }
                        //         }
                        //
                        //         break Err(Error::InvalidContentType);
                        //     }
                        // }
                        // break Err(Error::InvalidContentType);
                    }
                    StatusCode::SEE_OTHER => {
                        break Err(Error::Redirection303);
                    }
                    _code if status.is_redirection() => match resp.headers.get("location") {
                        Some(location) => {
                            redirection_number += 1;
                            let u = Iri::new(location.as_bytes())
                                .map_err(Error::InvalidRedirectionUrl)?;
                            url = vocabulary.insert(u);
                        }
                        None => break Err(Error::MissingRedirectionLocation),
                    },
                    code => break Err(Error::QueryFailed(code)),
                }
            }
        }
        .boxed()
    }
}
