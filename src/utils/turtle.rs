use crate::{
    lang::turtle::{self, NamedNode, Turtle},
    model::spanned,
};
use chumsky::{primitive::end, recovery::skip_then_retry_until, Parser};
use lsp_types::CompletionItemKind;
use tracing::info;

pub fn parse(str: &str, location: &lsp_types::Url) -> Result<Turtle, String> {
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

#[derive(Default, Clone, Copy, Debug)]
pub enum PropertyType {
    Class,
    DatatypeProperty,
    ObjectProperty,
    #[default]
    Property,
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
pub const TYPE: &'static str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

pub const PROPERTIES: &'static [(&'static str, PropertyType)] = &[
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
