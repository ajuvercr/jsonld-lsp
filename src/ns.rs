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
        path,
        string
    }
}

pub mod owl {
    use sophia_api::namespace;

    namespace! {
     "http://www.w3.org/2002/07/owl#",
        imports,
        ObjectProperty,
        DatatypeProperty,
        Class
    }
}

pub mod rdfs {
    use sophia_api::namespace;

    namespace! {
     "http://www.w3.org/2000/01/rdf-schema#",
        subClassOf,
        label,
        comment,
        domain,
        range
    }
}

pub use sophia_api::ns::rdf;
