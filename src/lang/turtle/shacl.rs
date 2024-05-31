use lsp_types::{CompletionItemKind, Range};
use sophia_api::{
    ns,
    prelude::{Any, Dataset},
    quad::Quad,
    term::{BnodeId, GraphName, IriRef, Term, TermKind},
    MownStr,
};
use std::{borrow::Cow, ops::Deref, usize};
use tracing::info;

use crate::{lang::SimpleCompletion, model::Spanned};
use hashbrown::HashSet;

use super::{
    green::{self, ClassProvider},
    Base, Turtle,
};

// use super::{Base, Turtle};

#[derive(Debug, Clone)]
pub struct MyQuad<'a> {
    pub subject: MyTerm<'a>,
    pub predicate: MyTerm<'a>,
    pub object: MyTerm<'a>,
    pub span: std::ops::Range<usize>,
}
impl<'a> std::fmt::Display for MyQuad<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}. # {:?}",
            self.subject, self.predicate, self.object, self.span
        )
    }
}

impl<'a> MyQuad<'a> {
    pub fn to_owned(&self) -> MyQuad<'static> {
        MyQuad {
            subject: self.subject.to_owned(),
            predicate: self.predicate.to_owned(),
            object: self.object.to_owned(),
            span: self.span.clone(),
        }
    }
}

impl<'a> Quad for MyQuad<'a> {
    type Term = MyTerm<'a>;

    fn s(&self) -> sophia_api::quad::QBorrowTerm<Self> {
        self.subject.borrow_term()
    }

    fn p(&self) -> sophia_api::quad::QBorrowTerm<Self> {
        self.predicate.borrow_term()
    }

    fn o(&self) -> sophia_api::quad::QBorrowTerm<Self> {
        self.object.borrow_term()
    }

    fn g(&self) -> GraphName<sophia_api::quad::QBorrowTerm<Self>> {
        None
    }

    fn to_spog(self) -> sophia_api::quad::Spog<Self::Term> {
        ([self.subject, self.predicate, self.object], None)
    }
}
// pub type MyQuad<'a> = ([MyTerm<'a>; 3], GraphName<MyTerm<'a>>);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MyTerm<'a> {
    value: Cow<'a, str>,
    ty: TermKind,
}

impl<'a> std::fmt::Display for MyTerm<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}>", self.value)
    }
}

impl<'a> MyTerm<'a> {
    pub fn to_owned(&self) -> MyTerm<'static> {
        let value = Cow::Owned(self.value.to_string());
        MyTerm {
            value,
            ty: self.ty.clone(),
        }
    }
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

    pub fn as_str(&'a self) -> &'a str {
        &self.value
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

#[derive(Default, Debug)]
pub struct Triples<'a> {
    pub base_url: String,
    pub triples: Vec<MyQuad<'a>>,
    base: Option<MyTerm<'a>>,
}

impl<'a> Triples<'a> {
    pub fn new(turtle: &Turtle, triples: Vec<MyQuad<'a>>) -> Self {
        let base = match &turtle.base {
            Some(Spanned(Base(_, Spanned(named_node, _)), _)) => named_node
                .expand_step(turtle, HashSet::new())
                .map(|st| MyTerm::named_node(st)),
            None => Some(MyTerm::named_node(turtle.set_base.as_str().to_string())),
        };

        let base_url = turtle.set_base.to_string();
        Self {
            triples,
            base,
            base_url,
        }
    }

    pub fn to_owned(&self) -> Triples<'static> {
        let triples = self.triples.iter().map(|q| q.to_owned()).collect();
        let base: Option<MyTerm<'static>> = self.base.as_ref().map(|x| x.to_owned());

        Triples {
            base,
            triples,
            base_url: self.base_url.clone(),
        }
    }

    pub fn imports(&self, cb: impl FnMut(IriRef<MownStr<'_>>) -> ()) {
        if let Some(ref base) = self.base {
            self.triples
                .quads_matching([base], [owl::imports], Any, Any)
                .flatten()
                .flat_map(|s| s.o().iri())
                .for_each(cb);
        }
    }

    pub fn sub_class_of(&self, mut cb: impl FnMut(IriRef<MownStr<'_>>, IriRef<MownStr<'_>>) -> ()) {
        self.triples
            .quads_matching(Any, [rdfs::subClassOf], Any, Any)
            .flatten()
            .flat_map(|s| match (s.s().iri(), s.o().iri()) {
                (Some(s), Some(o)) => Some((s, o)),
                _ => None,
            })
            .for_each(|(x, y)| cb(x, y));
    }
}

impl<'a> Deref for Triples<'a> {
    type Target = Vec<MyQuad<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.triples
    }
}

pub mod shacl {
    use sophia_api::namespace;

    namespace! {
     "http://www.w3.org/ns/shacl#",
        NodeShape,
        property,
        targetClass,
        targetSubjectsOf,
        targetObjectsOf,
        path,
        name,
        class,
        node,
        datatype,
        minCount,
        maxCount
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

pub mod rdfs {
    use sophia_api::namespace;

    namespace! {
     "http://www.w3.org/2000/01/rdf-schema#",
        subClassOf
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
    pub min_count: Option<usize>,
    pub max_count: Option<usize>,
}

impl Property {
    fn into_edit_part(&self, turtle: &Turtle, index: &mut usize) -> String {
        info!("Editing part of {:?}", self);
        *index += 1;
        let path = turtle
            .shorten(&self.path)
            .unwrap_or(format!("<{}>", self.path));
        let placeholder = match &self.name {
            Some(name) => format!("${{{}:{}}}", index, name),
            None => format!("${}", index),
        };
        format!("{} {}", path, placeholder)
    }

    pub fn into_completion(&self, turtle: &Turtle, range: Range) -> SimpleCompletion {
        let path = turtle
            .shorten(&self.path)
            .unwrap_or(format!("<{}>", self.path));

        let edits = vec![lsp_types::TextEdit {
            new_text: path.clone(),
            range: range.clone(),
        }];

        let label = format!("{} (class property)", path);

        SimpleCompletion {
            kind: CompletionItemKind::PROPERTY,
            label,
            documentation: self.name.clone(),
            filter_text: Some(path),
            sort_text: None,
            edits,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Shape {
    pub name: Option<String>,
    pub clazz: Option<String>,
    pub node: String,
    pub properties: Vec<Property>,
    pub target_objects_of: Option<String>,
    pub target_subjects_of: Option<String>,
}

impl Shape {
    pub fn into_completion(
        &self,
        turtle: &Turtle,
        range: lsp_types::Range,
    ) -> Option<SimpleCompletion> {
        let mut index = 0;

        let required = self
            .properties
            .iter()
            .filter(|prop| prop.min_count.unwrap_or(0) > 0)
            .count();
        let total = self.properties.len();

        let fields = self
            .properties
            .iter()
            .filter(|prop| prop.min_count.unwrap_or(0) > 0)
            .map(|x| x.into_edit_part(turtle, &mut index))
            .collect::<Vec<_>>()
            .join(";\n    ");

        let clazz_id = turtle.shorten(self.name())?;
        let new_text = format!("{};\n    {}.", clazz_id, fields);
        let edits = vec![lsp_types::TextEdit {
            new_text,
            range: range.clone(),
        }];

        let description = format!(
            "Snippet for {}, with {} reqruied properties (out of {})",
            clazz_id, required, total
        );

        Some(SimpleCompletion {
            kind: CompletionItemKind::SNIPPET,
            label: format!("{} (snippet)", clazz_id),
            filter_text: Some(clazz_id),
            sort_text: None,
            documentation: Some(description),
            edits,
        })
    }

    pub fn name<'a>(&'a self) -> &'a str {
        self.clazz.as_ref().unwrap_or(&self.node)
    }

    pub fn get_properties(&self, provider: &mut impl ClassProvider) -> Vec<green::Property> {
        if let Some(claz) = &self.clazz {
            let claz_id = provider.named(claz);
            let node_id = provider.named(&self.node);
            provider.add_subclass(claz_id, node_id);
        }

        let domain = provider.named(self.name());

        let mut props: Vec<_> = self
            .properties
            .iter()
            .map(|prop| {
                let range = match &prop.ty {
                    Some(PropertyType::Clazz(class)) => green::Range::Class(provider.named(&class)),
                    Some(PropertyType::Primitive(_)) => green::Range::Primitive("Some primitive"),
                    None => green::Range::Class(provider.unnamed(None, "shacl")),
                };

                green::Property {
                    range,
                    domain,
                    property: MyTerm::named_node(&prop.path).to_owned(),
                    required: prop.min_count.clone().unwrap_or(0) > 0,
                }
            })
            .collect();

        if let Some(target_objects_of) = &self.target_objects_of {
            let prop = green::Property {
                range: green::Range::Class(domain),
                domain: provider.unnamed(None, "target_objects_of"),
                property: MyTerm::named_node(target_objects_of).to_owned(),
                required: false,
            };
            props.push(prop);
        }

        if let Some(target_subjects_of) = &self.target_subjects_of {
            let prop = green::Property {
                range: green::Range::Class(provider.unnamed(None, "target_subjects_of")),
                domain,
                property: MyTerm::named_node(target_subjects_of).to_owned(),
                required: false,
            };
            props.push(prop);
        }

        props
    }
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
        .quads_matching([id_ref], [shacl::class, shacl::node], Any, Any)
        .next()
        .and_then(|x| x.ok())
        .and_then(|x| x.to_o().iri().map(|x| x.unwrap().to_string()))
        .map(|x| PropertyType::Clazz(x));

    let min_count = data
        .quads_matching([id_ref], [shacl::minCount], Any, Any)
        .next()
        .and_then(|x| x.ok())
        .and_then(|x| x.to_o().lexical_form().and_then(|x| x.parse().ok()));

    let max_count = data
        .quads_matching([id_ref], [shacl::maxCount], Any, Any)
        .next()
        .and_then(|x| x.ok())
        .and_then(|x| x.to_o().lexical_form().and_then(|x| x.parse().ok()));

    Some(Property {
        name,
        path,
        ty: primitive.or(clazz),
        min_count,
        max_count,
    })
}

fn term_to_string(term: impl Term) -> Option<String> {
    if let Some(iri) = term.iri() {
        return Some(iri.as_str().to_string());
    }
    if let Some(iri) = term.bnode_id() {
        return Some(iri.as_str().to_string());
    }
    None
}

pub fn parse_shape<T: Term + std::fmt::Debug, Q: Quad>(data: &Vec<Q>, id_term: T) -> Option<Shape> {
    info!("{:?} Trying to parse shape", id_term);
    let id_ref = id_term.borrow_term();
    let node = term_to_string(id_ref).unwrap_or_else(|| String::from("Unnamed node???"));
    let name = data
        .quads_matching([id_ref], [shacl::name], Any, Any)
        .next()
        .and_then(|x| x.ok())
        .and_then(|x| x.to_o().lexical_form().map(|x| x.to_string()));

    let target_subjects_of = data
        .quads_matching([id_ref], [shacl::targetSubjectsOf], Any, Any)
        .next()
        .and_then(|x| x.ok())
        .and_then(|x| x.to_o().lexical_form().map(|x| x.to_string()));

    let target_objects_of = data
        .quads_matching([id_ref], [shacl::targetObjectsOf], Any, Any)
        .next()
        .and_then(|x| x.ok())
        .and_then(|x| x.to_o().lexical_form().map(|x| x.to_string()));

    let clazz = data
        .quads_matching([id_ref], [shacl::targetClass], Any, Any)
        .next()
        .and_then(|x| x.ok())
        .and_then(|x| x.to_o().iri().map(|x| x.unwrap().to_string()));

    let properties = data
        .quads_matching([id_ref], [shacl::property], Any, Any)
        .flatten()
        .flat_map(|id| parse_property(data, id.to_o()))
        .collect();

    info!("{:?} Success!", id_term);

    Some(Shape {
        node,
        name,
        clazz,
        target_subjects_of,
        target_objects_of,
        properties,
    })
}

pub fn parse_shapes(triples: &Triples<'_>) -> Vec<Shape> {
    info!(
        "Parsing shapes from {} ({} triples)",
        triples.base_url,
        triples.triples.len()
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
