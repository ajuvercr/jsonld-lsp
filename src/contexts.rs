use json_ld::syntax::context::{definition, FragmentRef, Value};

fn get_definition_ref<M: Send + Sync + Clone>(
    re: &definition::FragmentRef<M, Value<M>>,
) -> Option<Definition> {
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
            Some(Definition {
                key: key.as_str().to_string(),
                value: binding,
            })
        }
        _ => None,
    }
}

pub fn filter_definition<M: Sync + Send + Clone>(
    frag: FragmentRef<M, Value<M>>,
) -> Option<Definition> {
    match frag {
        FragmentRef::DefinitionFragment(x) => get_definition_ref(&x),
        _ => None,
    }
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub key: String,
    pub value: String,
}
