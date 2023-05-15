use std::ops::Range;

use dashmap::DashMap;
use model::ParentingSystem;
use ropey::Rope;

pub mod backend;
pub mod contexts;
pub mod model;
pub mod parser;
pub mod semantics;
pub mod utils;
pub mod web;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Error {
    pub msg: String,
    pub span: Range<usize>,
}

pub type Documents = DashMap<String, (ParentingSystem, Rope)>;

#[cfg(feature = "bin")]
pub use tower_lsp::lsp_types as lsp_types;

#[cfg(not(feature = "bin"))]
pub use lsp_types as lsp_types;


#[cfg(test)]
mod tests {
    use iref::{Iri, IriRefBuf};

    #[test]
    fn test_iri_resolve() {
        let resolved: Result<_, iref::Error> = (|| {
            let base_iri = Iri::new("http://a/b/c/d;p?q")?;
            let iri_ref = IriRefBuf::new("tetten")?;

            Ok(iri_ref.resolved(base_iri))
        })();

        assert!(resolved.is_ok());
        let resolved = resolved.unwrap();
        assert_eq!(resolved, "http://a/b/c/tetten");
    }

    #[test]
    fn test_iri_resolve_abs() {
        let resolved: Result<_, iref::Error> = (|| {
            let base_iri = Iri::new("http://a/b/c/d;p?q")?;
            let iri_ref = IriRefBuf::new("http://tetten.com")?;

            Ok(iri_ref.resolved(base_iri))
        })();

        assert!(resolved.is_ok());
        let resolved = resolved.unwrap();
        assert_eq!(resolved, "http://tetten.com");
    }
}
