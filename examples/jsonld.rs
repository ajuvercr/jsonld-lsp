use json_ld::{
    syntax::{
        context::{definition, Definition, FragmentRef, Value},
        ContextRef,
    },
    ContextLoader, Print, ReqwestLoader,
};

use iref::IriBuf;
use locspan::Location;

#[tokio::main]
async fn main() {
    println!("hello world");
    do_things().await;
}
type D = Definition<Location<IriBuf>>;

fn print_context_ref(re: &ContextRef<D>) {
    match re {
        ContextRef::Null => println!("null"),
        ContextRef::IriRef(x) => println!("iri {}", x.as_str()),
        ContextRef::Definition(d) => {
            for (key, value) in d.bindings.iter() {
                let binding = match value.definition.value().as_ref().unwrap() {
                    json_ld::syntax::context::TermDefinition::Simple(s) => s.as_str().to_string(),
                    json_ld::syntax::context::TermDefinition::Expanded(_) => "expanded".to_string(),
                };
                println!("{} => {}", key.as_str(), binding);
            }
        }
    }
}

fn print_definition_ref(re: &definition::FragmentRef<Location<IriBuf>, Value<Location<IriBuf>>>) {
    match re {
        definition::FragmentRef::Entry(definition::EntryRef::Definition(key, value)) => {
            let binding = match value.definition.value().as_ref().unwrap() {
                json_ld::syntax::context::TermDefinitionRef::Simple(s) => s.as_str().to_string(),
                json_ld::syntax::context::TermDefinitionRef::Expanded(e) => {
                    e.id.as_ref()
                        .unwrap()
                        .as_ref()
                        .unwrap()
                        .as_str()
                        .to_string()
                }
            };
            println!("{:?}", value.definition.metadata());
            println!("{} -> {}", key.as_str(), binding);
        }
        _ => {}
    }
}

async fn do_things() {
    let mut loader = ReqwestLoader::default();
    let iri = IriBuf::new("https://data.vlaanderen.be/doc/applicatieprofiel/sensoren-en-bemonstering/kandidaatstandaard/2022-04-28/context/ap-sensoren-en-bemonstering.jsonld").unwrap();
    let context = loader.load_context(iri).await.unwrap();

    let value = context.document().value();
    for thing in value.traverse() {
        match thing {
            // FragmentRef::Context(x) => print_context_ref(&x),
            FragmentRef::DefinitionFragment(x) => print_definition_ref(&x),
            // FragmentRef::ContextArray(_) => println!("context array"),
            _ => {}
        }
    }
    // println!("{}", value.pretty_print());
}
