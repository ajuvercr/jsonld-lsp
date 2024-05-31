use lsp_types::CompletionItemKind;
use sophia_api::{
    prelude::{Any, Dataset},
    quad::Quad,
    term::Term,
};

use crate::{
    lang::{
        turtle::{
            shacl::{MyTerm, Triples},
            Turtle,
        },
        SimpleCompletion,
    },
    ns::*,
};

use super::ClassProvider;

#[derive(Clone, Debug)]
pub enum Range {
    Class(usize),
    Primitive(&'static str),
}

impl Range {
    pub fn as_str<'a>(&self, handler: &'a impl ClassProvider) -> &'a str {
        match self {
            Range::Class(x) => handler.class(*x).as_str(),
            Range::Primitive(x) => x,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Property {
    pub range: Range,
    pub domain: usize,

    pub property: MyTerm<'static>,

    pub required: bool,
}
impl Property {
    pub fn into_completion(
        &self,
        turtle: &Turtle,
        range: lsp_types::Range,
    ) -> Option<SimpleCompletion> {
        let property = turtle.shorten(self.property.as_str())?;

        let edits = vec![lsp_types::TextEdit {
            new_text: property.clone(),
            range: range.clone(),
        }];
        Some(SimpleCompletion {
            kind: CompletionItemKind::PROPERTY,
            label: property,
            filter_text: None,
            sort_text: None,
            documentation: None,
            edits,
        })
    }
}

pub trait PropertyProvider {
    fn provide<'a>(
        &mut self,
        triples: &'a Triples<'a>,
        class_provider: &mut impl ClassProvider,
    ) -> Vec<Property>;
}

pub struct NsPropertyProvider;

impl PropertyProvider for NsPropertyProvider {
    fn provide<'a>(
        &mut self,
        triples: &'a Triples<'a>,
        class_provider: &mut impl ClassProvider,
    ) -> Vec<Property> {
        self.find_classes(triples, class_provider);
        self.find_subclasses(triples, class_provider);
        let mut out = vec![];

        self.find_object_properties(triples, class_provider, &mut out);
        self.find_datatype_properties(triples, class_provider, &mut out);

        out
    }
}

impl NsPropertyProvider {
    fn find_classes(&self, triples: &Triples<'_>, class_provider: &mut impl ClassProvider) {
        for t in triples
            .triples
            .quads_matching(Any, [rdf::type_], [owl::Class], Any)
            .flatten()
        {
            if let Some(x) = t.to_s().iri() {
                class_provider.named(x.as_str());
            }
        }
    }

    fn find_subclasses(&self, triples: &Triples<'_>, class_provider: &mut impl ClassProvider) {
        for t in triples
            .quads_matching(Any, [rdfs::subClassOf], Any, Any)
            .flatten()
        {
            if let (Some(a), Some(b)) = (t.s().iri(), t.o().iri()) {
                let class = class_provider.named(b.as_str());
                let subclass = class_provider.named(a.as_str());
                class_provider.add_subclass(subclass, class);
            }
        }
    }

    fn classed(
        &self,
        triples: &Triples<'_>,
        class_provider: &mut impl ClassProvider,
        subject: &MyTerm<'_>,
        predicate: impl Term,
    ) -> usize {
        if let Some(x) = triples
            .triples
            .quads_matching([subject], [predicate], Any, Any)
            .flatten()
            .map(|x| x.to_o())
            .next()
        {
            if let Some(iri) = x.iri() {
                return class_provider.named(iri.as_str());
            }
            if let Some(iri) = x.bnode_id() {
                // TODO: This might give collisions :(
                return class_provider.unnamed(iri.as_str().into(), "ns_class");
            }
        }
        return class_provider.unnamed(None, "ns_class");
    }

    fn find_object_properties(
        &self,
        triples: &Triples<'_>,
        class_provider: &mut impl ClassProvider,
        properties: &mut Vec<Property>,
    ) {
        properties.extend(
            triples
                .quads_matching(Any, [rdf::type_], [owl::ObjectProperty, rdf::Property], Any)
                .flatten()
                .map(|t| {
                    let range = self.classed(triples, class_provider, t.s(), rdfs::range);
                    let domain = self.classed(triples, class_provider, t.s(), rdfs::domain);
                    let property = t.s().to_owned();
                    Property {
                        range: Range::Class(range),
                        domain,
                        property,
                        required: false,
                    }
                }),
        )
    }

    fn find_datatype_properties(
        &self,
        triples: &Triples<'_>,
        class_provider: &mut impl ClassProvider,
        properties: &mut Vec<Property>,
    ) {
        properties.extend(
            triples
                .quads_matching(Any, [rdf::type_], [owl::DatatypeProperty], Any)
                .flatten()
                .map(|t| {
                    // let range = self.classed(class_provider, t.s(), rdfs::range);
                    let domain = self.classed(triples, class_provider, t.s(), rdfs::domain);
                    let property = t.s().to_owned();
                    Property {
                        range: Range::Primitive("something"),
                        domain,
                        property,
                        required: false,
                    }
                }),
        )
    }
}
