use std::collections::HashMap;

use reqwest::header::HeaderMap;

#[derive(Debug)]
pub struct Resp {
    pub headers: HeaderMap,
    pub body: String,
    pub status: u16,
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch(url: &str, headers: &HashMap<String, String>) -> std::result::Result<Resp, ()> {
    use tracing::{debug, error};
    // TODO: This should not need to be reqwest blocking, but using the standard reqwest caused a
    // hang on `send`.
    // The same happened when testing with hyper
    // Even blocking didn't work on a later version of reqwest

    let client = reqwest::blocking::Client::new();
    let builder = client.get(url);

    let builder = headers
        .into_iter()
        .fold(builder, |builder, (k, v)| builder.header(k, v));

    debug!("sending blcoking");
    let resp = match builder.send() {
        Ok(x) => x,
        Err(e) => {
            error!(error = ?e);
            return Err(());
        }
    };

    let status = resp.status().as_u16();
    let headers = resp.headers().clone();
    debug!("got resp");
    let body = resp.text().unwrap();

    Ok(Resp {
        headers,
        body,
        status,
    })
}

#[cfg(target_arch = "wasm32")]
pub async fn fetch(url: &str, headers: &HashMap<String, String>) -> std::result::Result<Resp, ()> {
    use futures::channel::oneshot;
    let (tx, rx) = oneshot::channel();
    let _ = wasm_bindgen_futures::future_to_promise(web::local_fetch(
        url.to_string(),
        headers.clone(),
        tx,
    ));
    let x = rx.await.map_err(|_| ())?;
    x
}

#[cfg(target_arch = "wasm32")]
mod web {
    use std::collections::HashMap;

    use crate::utils::Resp;
    use reqwest::header::{HeaderMap, HeaderName, HeaderValue};
    use serde::Serializer;
    use serde_json::json;
    use wasm_bindgen::{prelude::wasm_bindgen, JsCast, JsValue};
    use web_sys::Response;

    #[wasm_bindgen]
    extern "C" {
        async fn fetch(url: JsValue, options: JsValue) -> JsValue;
    }

    pub async fn local_fetch(
        url: String,
        headers: HashMap<String, String>,
        tx: futures::channel::oneshot::Sender<Result<Resp, ()>>,
    ) -> Result<wasm_bindgen::JsValue, wasm_bindgen::JsValue> {
        let ser: serde_wasm_bindgen::Serializer = serde_wasm_bindgen::Serializer::json_compatible();
        let options_json = json!({ "headers": headers });
        let url = url.into();
        let options = ser.serialize_some(&options_json).unwrap();

        let resp_value = fetch(url, options).await;

        // `resp_value` is a `Response` object.
        if !resp_value.is_instance_of::<Response>() {
            todo!()
            // return Err("Not a response!".into());
        }

        let resp: Response = resp_value.dyn_into().unwrap();
        let status = resp.status();
        let headers = resp.headers();
        let headers: HashMap<String, String> =
            serde_wasm_bindgen::from_value(headers.into()).unwrap();

        let mut map = HeaderMap::new();
        headers
            .into_iter()
            .filter_map(|(k, v)| {
                let key = HeaderName::try_from(k).ok()?;
                let value = HeaderValue::from_str(&v).ok()?;
                Some((key, value))
            })
            .for_each(|(k, v)| {
                map.insert(k, v);
            });

        // Convert this other `Promise` into a rust `Future`.
        let body = wasm_bindgen_futures::JsFuture::from(resp.text().unwrap())
            .await
            .unwrap()
            .as_string()
            .unwrap();

        tx.send(Ok(Resp {
            headers: map,
            body,
            status,
        }))
        .unwrap();

        Ok("".into())
    }
}
