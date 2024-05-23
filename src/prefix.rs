use std::{collections::HashMap, sync::Arc};

use crate::utils::fetch;

#[derive(Clone, Debug)]
pub struct Prefixes {
    names: Arc<HashMap<String, String>>,
}

impl Prefixes {
    #[allow(unused)]
    async fn fetch_prefix() -> Option<HashMap<String, String>> {
        let headers = [].into();
        let resp = fetch("http://prefix.cc/context", &headers).await.ok()?;

        let context: HashMap<String, HashMap<String, String>> =
            serde_json::from_str(&resp.body).ok()?;
        let names = context.into_iter().find(|x| x.0 == "@context")?.1;

        Some(names)
    }

    fn default_prefix() -> HashMap<String, String> {
        let prefix_cc_str = include_str!("../prefix_cc.json");
        let out = serde_json::from_str(&prefix_cc_str).unwrap();
        out
    }

    pub async fn new() -> Option<Prefixes> {
        // let names = Self::fetch_prefix()
        //     .await
        //     .unwrap_or_else(Self::default_prefix);

        let names = Self::default_prefix();

        Some(Prefixes {
            names: names.into(),
        })
    }

    pub fn get<'a>(&'a self, prefix: &str) -> Option<&'a str> {
        self.names.get(prefix).map(|x| x.as_str())
    }

    pub fn get_all<'a>(&'a self) -> impl Iterator<Item = &'a str> {
        self.names.keys().map(|x| x.as_str())
    }
}
