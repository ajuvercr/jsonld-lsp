use crate::{backend::Client, lsp_types::*};
use tower_lsp::LanguageServer;
use wasm_bindgen::{prelude::wasm_bindgen, JsValue};

use crate::backend::Backend;

#[wasm_bindgen]
extern "C" {
    fn log_message(msg: JsValue);
    fn publish_diagnostics(diags: JsValue);
}

#[wasm_bindgen]
pub struct WebClient;
#[wasm_bindgen]
impl WebClient {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self
    }
}

#[tower_lsp::async_trait]
impl Client for WebClient {
    async fn log_message<M: std::fmt::Display + Sync + Send + 'static>(
        &self,
        _ty: lsp_types::MessageType,
        msg: M,
    ) -> () {
        let msg = serde_wasm_bindgen::to_value(&msg.to_string()).unwrap();
        log_message(msg);
    }

    async fn publish_diagnostics(
        &self,
        _uri: lsp_types::Url,
        diags: Vec<Diagnostic>,
        _version: Option<i32>,
    ) -> () {
        let diags = serde_wasm_bindgen::to_value(&diags).unwrap();
        unsafe { publish_diagnostics(diags) }
    }
}

macro_rules! gen {
    ($($tail:tt)*) => {
        #[wasm_bindgen]
        impl WebBackend {
            $(
            pub async fn $tail(&self, params: JsValue) -> Option<JsValue> {
                let params = serde_wasm_bindgen::from_value(params).ok()?;
                let out = self.inner.$tail(params).await.ok()?;
                serde_wasm_bindgen::to_value(&out).ok()
            }
            )*
        }
    };
}

macro_rules! gen2 {
    ($($no_ret:tt)*) => {
        #[wasm_bindgen]
        impl WebBackend {
            $(
            pub async fn $no_ret(&self, params: JsValue) -> Option<&str> {
                let params = serde_wasm_bindgen::from_value(params).ok()?;
                self.inner.$no_ret(params).await;
                Some("good")
            }
            )*
        }
    };
}

#[wasm_bindgen]
pub struct WebBackend {
    inner: Backend<WebClient>,
}
#[wasm_bindgen]
impl WebBackend {
    #[wasm_bindgen(constructor)]
    pub fn new(client: WebClient) -> Self {
        Self {
            inner: Backend::new(client),
        }
    }
}

gen!(initialize prepare_rename rename semantic_tokens_full completion);

gen2!(did_open did_change did_save);

