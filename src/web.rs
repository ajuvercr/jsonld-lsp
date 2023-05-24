use crate::{backend::Client, lsp_types::*};
use serde::Serializer;
use serde_json::json;
use tower_lsp::LanguageServer;
use wasm_bindgen::{prelude::wasm_bindgen, JsValue};

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
pub fn set_logger(f: js_sys::Function) {
    unsafe {
        LOG_FN = Some(f);
    }
}

#[wasm_bindgen]
pub fn set_diags(f: js_sys::Function) {
    unsafe {
        DIAGS_FN = Some(f);
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
    ($($tail:tt)*) => {
        #[wasm_bindgen]
        impl WebBackend {
            $(
            pub async fn $tail(&self, params: JsValue) -> Result<JsValue, JsValue> {
                let params = serde_wasm_bindgen::from_value(params)?;
                let out = self.inner.$tail(params).await.map_err(|_| format!("{} failed", stringify!($tail)))?;
                let out = SER.serialize_some(&out)?;
                Ok(out)
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
            pub async fn $no_ret(&self, params: JsValue) -> Result<JsValue, JsValue> {
                log_message(format!("Running {}", stringify!($no_ret)).into())?;
                let params = match serde_wasm_bindgen::from_value(params) {
                    Ok(x) => x,
                    Err(e) => {
                        log_message(format!("Error {}", e).into())?;
                        return Err(e.to_string().into());
                    }
                };
                self.inner.$no_ret(params).await;
                Ok("Ok".to_string().into())
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

    // pub async fn testing(&self) -> Result<JsValue, JsValue> {
    //     let resp = self
    //         .inner
    //         .client
    //         .fetch(
    //             "https://jsonplaceholder.typicode.com/todos/1",
    //             &HashMap::new(),
    //         )
    //         .await
    //         .map_err(|_| "")?;
    //
    //     let _ = log_message(format!("got {:?}", resp).into());
    //
    //     Ok("".into())
    // }
}

gen!(initialize prepare_rename rename semantic_tokens_full completion);

gen2!(did_open did_change did_save);
