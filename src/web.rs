use crate::{backend::Client, lsp_types::*, utils::web_types as wt, lang::jsonld::JsonLd};
use serde::Serializer;
use serde_json::json;
use tower_lsp::LanguageServer;
use wasm_bindgen::{prelude::wasm_bindgen, JsCast, JsValue};

use crate::backend::Backend;

const SER: serde_wasm_bindgen::Serializer = serde_wasm_bindgen::Serializer::json_compatible();
static mut LOG_FN: Option<js_sys::Function> = None;
static mut DIAGS_FN: Option<js_sys::Function> = None;

fn publish_diagnostics(diags: JsValue) -> Result<(), String> {
    unsafe {
        let this = JsValue::null();
        if let Some(f) = &DIAGS_FN {
            if let Err(e) = f.call1(&this, &diags) {
                let msg: serde_json::Value = serde_wasm_bindgen::from_value(e).unwrap();
                return Err(format!("Call failed {:?}", msg));
            }
        } else {
            return Err("Not set!".to_string());
        }
    }
    Ok(())
}

fn log_message(msg: JsValue) -> Result<(), String> {
    unsafe {
        let this = JsValue::null();
        if let Some(f) = &LOG_FN {
            if let Err(e) = f.call1(&this, &msg) {
                let msg: serde_json::Value = serde_wasm_bindgen::from_value(e).unwrap();
                return Err(msg.to_string());
            }
        } else {
            return Err("Not set!".to_string());
        }
    }
    Ok(())
}

pub fn log_msg(msg: impl std::fmt::Display) {
    if let Err(e) = log_message(msg.to_string().into()) {
        let _ = log_message(format!("Failed logging msg {}", e).into());
    }
}

#[wasm_bindgen]
pub struct WebClient;
#[wasm_bindgen]
impl WebClient {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self
    }

    pub fn name(&self) -> String {
        "My name is Jeff".to_string()
    }
}

#[wasm_bindgen]
pub fn init_panic_hook() {
    console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub fn set_logger(f: wt::SetLoggerFn) {
    unsafe {
        let jsvalue: JsValue = f.into();
        LOG_FN = Some(jsvalue.unchecked_into());
    }
}

#[wasm_bindgen]
pub fn set_diags(f: wt::SetDiagnosticsFn) {
    unsafe {
        let jsvalue: JsValue = f.into();
        DIAGS_FN = Some(jsvalue.unchecked_into());
    }
}

#[wasm_bindgen]
pub fn create_webclient() -> WebClient {
    WebClient::new()
}

#[tower_lsp::async_trait]
impl Client for WebClient {
    async fn log_message<M: std::fmt::Display + Sync + Send + 'static>(
        &self,
        _ty: lsp_types::MessageType,
        msg: M,
    ) -> () {
        if let Err(e) = log_message(msg.to_string().into()) {
            let _ = log_message(format!("Failed logging msg {}", e).into());
        }
    }

    async fn publish_diagnostics(
        &self,
        uri: lsp_types::Url,
        diags: Vec<Diagnostic>,
        _version: Option<i32>,
    ) -> () {
        let json = json!({
            "uri": uri.as_str(),
            "diagnostics": diags
        });
        let diags = SER.serialize_some(&json).unwrap();
        if let Err(e) = publish_diagnostics(diags) {
            let _ = log_message(format!("Failed publishing diags {}", e).into());
        }
    }
}

macro_rules! gen {
    ($($fn:tt $ty:path)* ) => {
        #[wasm_bindgen]
        impl WebBackend {
            $(
            pub async fn $fn(&self, params: $ty) -> Result<JsValue, JsValue> {
                log_message(format!("Running {}", stringify!($fn)).into())?;
                let params = serde_wasm_bindgen::from_value(params.into())?;
                let out = self.inner.$fn(params).await
                    .map_err(|e| format!("{} failed {}", stringify!($fn), e.to_string()))?;
                let out = SER.serialize_some(&out)?;
                Ok(out)
            }
            )*
        }
    };
}

macro_rules! gen2 {
    ($($fn:tt $ty:path)*) => {
        #[wasm_bindgen]
        impl WebBackend {
            $(
            pub async fn $fn(&self, params: $ty) -> Result<JsValue, JsValue> {
                log_message(format!("Running {}", stringify!($fn)).into())?;
                let params = match serde_wasm_bindgen::from_value(params.into()) {
                    Ok(x) => x,
                    Err(e) => {
                        log_message(format!("Error {}", e).into())?;
                        return Err(e.to_string().into());
                    }
                };
                self.inner.$fn(params).await;
                Ok("Ok".to_string().into())
            }
            )*
        }
    };
}

#[wasm_bindgen]
pub struct WebBackend {
    inner: Backend<WebClient, JsonLd>,
}

#[wasm_bindgen]
impl WebBackend {
    #[wasm_bindgen(constructor)]
    pub fn new(client: WebClient) -> Self {
        Self {
            inner: Backend::new(client, Default::default()),
        }
    }
}

gen!(initialize wt::InitializeParams prepare_rename wt::PrepareRenameParams  rename wt::RenameParams semantic_tokens_full wt::SemanticTokensParams completion wt::CompletionParams);

gen2!(did_open  wt::DidOpenTextDocumentParams did_change wt::DidChangeTextDocumentParams did_save wt::DidSaveTextDocumentParams);
