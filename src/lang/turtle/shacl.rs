use sophia_api::{
    ns::{self},
    prelude::{Any, Dataset},
    quad::Quad,
    term::{BnodeId, IriRef, Term, TermKind},
    MownStr,
};
use std::borrow::Cow;
use tracing::info;

use crate::model::Spanned;
use hashbrown::HashSet;

use super::{Base, Turtle};

#[derive(Debug, Clone)]
pub struct MyTerm<'a> {
    value: Cow<'a, str>,
    ty: TermKind,
}

impl<'a> MyTerm<'a> {
    pub fn named_node<T: Into<Cow<'a, str>>>(value: T) -> Self {
        Self {
            value: value.into(),
            ty: TermKind::Iri,
        }
    }
    pub fn blank_node<T: Into<Cow<'a, str>>>(value: T) -> Self {
        Self {
            value: value.into(),
            ty: TermKind::BlankNode,
        }
    }
    pub fn literal<T: Into<Cow<'a, str>>>(value: T) -> Self {
        Self {
            value: value.into(),
            ty: TermKind::Literal,
        }
    }
}

impl<'a> Term for MyTerm<'a> {
    type BorrowTerm<'x> = &'x Self
    where
        Self: 'x;

    fn kind(&self) -> sophia_api::term::TermKind {
        self.ty
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        self
    }

    fn iri(&self) -> Option<sophia_api::term::IriRef<sophia_api::MownStr>> {
        self.is_iri()
            .then(|| IriRef::new_unchecked(MownStr::from_str(&self.value)))
    }

    fn bnode_id(&self) -> Option<sophia_api::term::BnodeId<sophia_api::MownStr>> {
        self.is_blank_node()
            .then(|| BnodeId::new_unchecked(MownStr::from_str(&self.value)))
    }

    fn lexical_form(&self) -> Option<sophia_api::MownStr> {
        self.is_literal().then(|| MownStr::from_str(&self.value))
    }

    fn datatype(&self) -> Option<sophia_api::term::IriRef<sophia_api::MownStr>> {
        None
    }

    fn language_tag(&self) -> Option<sophia_api::term::LanguageTag<sophia_api::MownStr>> {
        None
    }

    fn variable(&self) -> Option<sophia_api::term::VarName<sophia_api::MownStr>> {
        panic!("MyTerm does not supported variables")
    }

    fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
        panic!("MyTerm does not supported triples")
    }

    fn to_triple(self) -> Option<[Self; 3]>
    where
        Self: Sized,
    {
        panic!("MyTerm does not supported triples")
    }
}

pub mod shacl {
    use sophia_api::namespace;

    namespace! {
     "http://www.w3.org/ns/shacl#",
        NodeShape,
        property,
        targetClass,
        path,
        name,
        class,
        datatype
    }
}

pub mod xsd {
    use sophia_api::namespace;

    namespace! {
     "http://www.w3.org/2001/XMLSchema#",
        NodeShape,
        property,
        path
    }
}

pub mod owl {
    use sophia_api::namespace;

    namespace! {
     "http://www.w3.org/2002/07/owl#",
        imports
    }
}

#[derive(Clone, Debug)]
pub enum PropertyType {
    Primitive(String),
    Clazz(String),
}

#[derive(Clone, Debug)]
pub struct Property {
    pub name: Option<String>,
    pub path: String,
    pub ty: Option<PropertyType>,
}

#[derive(Clone, Debug)]
pub struct Shape {
    pub name: Option<String>,
    pub clazz: String,
    pub properties: Vec<Property>,
}

fn parse_property<T: Term + std::fmt::Debug, Q: Quad>(
    data: &Vec<Q>,
    id_term: T,
) -> Option<Property> {
    let id_ref = id_term.borrow_term();

    let name = data
        .quads_matching([id_ref], [shacl::name], Any, Any)
        .next()
        .and_then(|x| x.ok())
        .and_then(|x| x.to_o().lexical_form().map(|x| x.to_string()));

    let path = data
        .quads_matching([id_ref], [shacl::path], Any, Any)
        .next()
        .and_then(|x| x.ok())
        .and_then(|x| x.to_o().iri().map(|x| x.unwrap().to_string()))?;

    let primitive = data
        .quads_matching([id_ref], [shacl::datatype], Any, Any)
        .next()
        .and_then(|x| x.ok())
        .and_then(|x| x.to_o().iri().map(|x| x.unwrap().to_string()))
        .map(|x| PropertyType::Primitive(x));

    let clazz = data
        .quads_matching([id_ref], [shacl::class], Any, Any)
        .next()
        .and_then(|x| x.ok())
        .and_then(|x| x.to_o().iri().map(|x| x.unwrap().to_string()))
        .map(|x| PropertyType::Clazz(x));

    Some(Property {
        name,
        path,
        ty: primitive.or(clazz),
    })
}

pub fn parse_shape<T: Term + std::fmt::Debug, Q: Quad>(data: &Vec<Q>, id_term: T) -> Option<Shape> {
    info!("Trying to parse shape {:?}", id_term);
    let id_ref = id_term.borrow_term();
    let name = data
        .quads_matching([id_ref], [shacl::name], Any, Any)
        .next()
        .and_then(|x| x.ok())
        .and_then(|x| x.to_o().lexical_form().map(|x| x.to_string()));

    let clazz = data
        .quads_matching([id_ref], [shacl::targetClass], Any, Any)
        .next()
        .and_then(|x| x.ok())
        .and_then(|x| x.to_o().iri().map(|x| x.unwrap().to_string()))?;

    let properties = data
        .quads_matching([id_ref], [shacl::property], Any, Any)
        .flatten()
        .flat_map(|id| parse_property(data, id.to_o()))
        .collect();

    Some(Shape {
        name,
        clazz,
        properties,
    })
}

pub fn parse_shapes(turtle: &Turtle, mut on_import: impl FnMut(&str) -> ()) -> Vec<Shape> {
    let triples = turtle.get_simple_triples().unwrap_or_default();

    if let Some(base) = match &turtle.base {
        Some(Spanned(Base(_, Spanned(named_node, _)), _)) => named_node
            .expand_step(turtle, HashSet::new())
            .map(|st| MyTerm::named_node(st)),
        None => Some(MyTerm::named_node(turtle.set_base.as_str().to_string())),
    } {
        triples
            .quads_matching([&base], [owl::imports], Any, Any)
            .flatten()
            .flat_map(|s| s.o().iri())
            .for_each(|x| on_import(x.as_str()));
    }

    info!(
        "Parsing shapes  {} ({} triples)",
        turtle.set_base.as_str(),
        triples.len()
    );

    triples
        .quads_matching(Any, [ns::rdf::type_], [shacl::NodeShape], Any)
        .flatten()
        .flat_map(|x| parse_shape(&triples, x.to_s()))
        .collect()
}

#[cfg(test)]
mod test {

    use std::str::FromStr;

    use chumsky::{Parser, Stream};
    use sophia_api::{
        term::{IriRef, SimpleTerm},
        MownStr,
    };

    use crate::{
        lang::turtle::{parser::turtle, shacl::parse_shape, tokenizer, Turtle},
        model::{spanned, Spanned},
    };

    #[derive(Debug)]
    pub enum Err {
        Tokenizing,
        Parsing,
    }

    fn parse_turtle(
        inp: &str,
        url: &lsp_types::Url,
    ) -> Result<(Turtle, Vec<Spanned<String>>), Err> {
        let tokens = tokenizer::parse_tokens().parse(inp).map_err(|err| {
            println!("Token error {:?}", err);
            Err::Tokenizing
        })?;
        let end = inp.len() - 1..inp.len() + 1;

        let mut comments: Vec<_> = tokens
            .iter()
            .filter(|x| x.0.is_comment())
            .cloned()
            .map(|x| spanned(x.0.to_comment(), x.1))
            .collect();
        comments.sort_by_key(|x| x.1.start);

        let stream = Stream::from_iter(end, tokens.into_iter().filter(|x| !x.0.is_comment()));

        turtle(&url)
            .parse(stream)
            .map_err(|err| {
                println!("Parse error {:?}", err);
                Err::Parsing
            })
            .map(|t| (t, comments))
    }

    #[test]
    fn easy_triples() {
        let txt = r#"
@prefix js: <https://w3id.org/conn/js#> .
@prefix fno: <https://w3id.org/function/ontology#> .
@prefix fnom: <https://w3id.org/function/vocabulary/mapping#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix : <https://w3id.org/conn#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

<#testShape> a sh:NodeShape;
  sh:targetClass js:MumoFetch;
  sh:property [
    sh:class :WriterChannel;
    sh:path js:dataOutput;
    sh:name "Data output";
    sh:minCount 1;
    sh:maxCount 1;
  ], [
    sh:datatype xsd:string;
    sh:path js:startUrl;
    sh:name "Mumo start url";
    sh:minCount 1;
    sh:maxCount 1;
  ], [
    sh:datatype xsd:string;
    sh:path js:savePath;
    sh:name "Save path";
    sh:maxCount 1;
  ], [
    sh:datatype xsd:integer;
    sh:path js:intervalMs;
    sh:name "Interval";
    sh:maxCount 1;
  ].
"#;

        let url = lsp_types::Url::from_str("http://example.com/ns").unwrap();
        let (output, _) = parse_turtle(txt, &url).expect("Simple");
        let triples = output.get_simple_triples().expect("Triples found");

        let id = SimpleTerm::Iri(
            IriRef::new(MownStr::from_str("http://example.com/ns#testShape")).unwrap(),
        );
        let m_shape = parse_shape(&triples, id);
        assert!(m_shape.is_some());

        let shape = m_shape.unwrap();

        println!("Shape {:?}", shape);
        assert_eq!(shape.properties.len(), 4);
        assert_eq!(
            shape
                .properties
                .into_iter()
                .flat_map(|x| x.name)
                .collect::<Vec<_>>(),
            vec!["Data output", "Mumo start url", "Save path", "Interval"]
        );
    }
}
