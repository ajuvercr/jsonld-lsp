use crate::{
    lang::turtle::{parse_turtle, tokenizer, Turtle},
    model::spanned,
};
use chumsky::Parser;
use lsp_types::CompletionItemKind;

pub fn parse(str: &str, location: &lsp_types::Url) -> Result<Turtle, String> {
    let tokens = tokenizer::parse_tokens().parse(str).map_err(|err| {
        println!("Token error {:?}", err);
        String::from("Tokenizing failed")
    })?;

    let mut comments: Vec<_> = tokens
        .iter()
        .filter(|x| x.0.is_comment())
        .cloned()
        .map(|x| spanned(x.0.to_comment(), x.1))
        .collect();
    comments.sort_by_key(|x| x.1.start);

    let (turtle, _) = parse_turtle(location, tokens, str.len());

    Ok(turtle.into_value())
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
