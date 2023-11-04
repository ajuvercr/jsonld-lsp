use std::collections::HashMap;

use reqwest::header::HeaderMap;

#[derive(Debug)]
pub struct Resp {
    pub headers: HeaderMap,
    pub body: String,
    pub status: u16,
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch(
    url: &str,
    headers: &HashMap<String, String>,
) -> std::result::Result<Resp, String> {
    use tokio::{fs::File, io::AsyncReadExt};
    use tracing::{debug, error, info};

    let url = reqwest::Url::parse(url).map_err(|_| String::from("invalid url!"))?;
    info!("Found url {} {}", url.scheme(), url);
    if url.scheme() == "file" {
        let mut file = File::open(url.path())
            .await
            .map_err(|_| format!("File not found {}", url.path()))?;
        let mut body = String::new();
        file.read_to_string(&mut body)
            .await
            .map_err(|_| format!("Failed to read file"))?;
        let status = 200;
        let headers = HeaderMap::new();
        return Ok(Resp {
            headers,
            body,
            status,
        });
    }

    let client = reqwest::Client::new();
    let builder = client.get(url);

    let builder = headers
        .into_iter()
        .fold(builder, |builder, (k, v)| builder.header(k, v));

    debug!("sending blcoking");
    let resp = match builder.send().await {
        Ok(x) => x,
        Err(e) => {
            error!(error = ?e);
            return Err(e.to_string());
        }
    };

    let status = resp.status().as_u16();
    let headers = resp.headers().clone();
    debug!("got resp");
    let body = resp.text().await.unwrap();

    Ok(Resp {
        headers,
        body,
        status,
    })
}

// This cannot be programmed with reqwest, because that reqwest::Response is not Send
#[cfg(target_arch = "wasm32")]
pub async fn fetch(
    url: &str,
    headers: &HashMap<String, String>,
) -> std::result::Result<Resp, String> {
    use futures::channel::oneshot;
    let (tx, rx) = oneshot::channel();
    let _ = wasm_bindgen_futures::future_to_promise(web::local_fetch(
        url.to_string(),
        headers.clone(),
        tx,
    ));
    let x = rx.await.map_err(|e| e.to_string())?;
    x
}

#[cfg(target_arch = "wasm32")]
mod web {
    use std::collections::HashMap;

    use crate::utils::Resp;
    use crate::web::read_file;
    use reqwest::{
        header::{HeaderMap, HeaderName, HeaderValue},
        Url,
    };
    use serde::Serializer;
    use serde_json::json;
    use tracing::info;
    use wasm_bindgen::{prelude::wasm_bindgen, JsCast, JsValue};
    use web_sys::Response;

    #[wasm_bindgen]
    extern "C" {
        #[wasm_bindgen(catch)]
        async fn fetch(url: JsValue, options: JsValue) -> Result<JsValue, JsValue>;
    }

    pub async fn try_fetch(url: String, headers: HashMap<String, String>) -> Result<Resp, String> {
        if let Ok(url) = Url::parse(&url) {
            if url.scheme() == "file" {
                info!("Url scheme is file, let's do that! {}", url.path());
                let body = read_file(url.path()).await?;
                let status = 200;
                let headers = HeaderMap::new();
                return Ok(Resp {
                    headers,
                    body,
                    status,
                });
            }
        }

        let ser: serde_wasm_bindgen::Serializer = serde_wasm_bindgen::Serializer::json_compatible();
        let options_json = json!({ "headers": headers });
        let url = format!("https://proxy.linkeddatafragments.org/{}", url);
        let options = ser
            .serialize_some(&options_json)
            .map_err(|_| String::from("failed to serialize headers"))?;

        let resp_value = fetch(url.clone().into(), options)
            .await
            .map_err(|e| format!("{:?}", e))?;
        info!("Got resp {}", url);

        // `resp_value` is a `Response` object.
        if !resp_value.is_instance_of::<Response>() {
            return Err("Not a response!".into());
        }

        let resp: Response = resp_value.dyn_into().unwrap();
        let status = resp.status();
        let headers = resp.headers();
        let headers: HashMap<String, String> =
            serde_wasm_bindgen::from_value(headers.into()).map_err(|e| e.to_string())?;

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
        let body =
            wasm_bindgen_futures::JsFuture::from(resp.text().map_err(|e| format!("{:?}", e))?)
                .await
                .map_err(|e| format!("{:?}", e))?
                .as_string()
                .ok_or(String::from("Not a string"))?;

        info!("Got resp {} body {}", url, body.len());

        Ok(Resp {
            headers: map,
            body,
            status,
        })
    }

    pub async fn local_fetch(
        url: String,
        headers: HashMap<String, String>,
        tx: futures::channel::oneshot::Sender<Result<Resp, String>>,
    ) -> Result<wasm_bindgen::JsValue, wasm_bindgen::JsValue> {
        let resp = try_fetch(url, headers).await;

        tx.send(resp).unwrap();

        Ok("".into())
    }
}
