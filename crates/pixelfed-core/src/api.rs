use serde::{Deserialize, Serialize};
use serde_json::Value;
use url::Url;

use crate::error::AppError;
use crate::link::parse_link_header;
use crate::session::Session;

/// A request the UI wants made as the logged-in user. Only the path is
/// supplied; the origin and the bearer token are the session's business.
#[derive(Debug, Clone, Deserialize)]
pub struct ApiRequest {
    pub method: Method,
    /// Must start with `/api/`. Example: `/api/v1/timelines/home`.
    pub path: String,
    #[serde(default)]
    pub query: Vec<(String, String)>,
    #[serde(default)]
    pub body: Option<Value>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum Method {
    Get,
    Post,
    Put,
    Delete,
}

impl From<Method> for reqwest::Method {
    fn from(m: Method) -> Self {
        match m {
            Method::Get => reqwest::Method::GET,
            Method::Post => reqwest::Method::POST,
            Method::Put => reqwest::Method::PUT,
            Method::Delete => reqwest::Method::DELETE,
        }
    }
}

/// Decoded JSON plus the pagination cursors from the `Link` header.
#[derive(Debug, Clone, Serialize)]
pub struct ApiResponse {
    pub status: u16,
    pub body: Value,
    pub next: Option<String>,
    pub prev: Option<String>,
}

/// One file for `POST /api/v2/media`. The bytes arrive from the webview as
/// a raw IPC body; nothing here touches the filesystem.
#[derive(Debug, Clone)]
pub struct MediaUpload {
    pub bytes: Vec<u8>,
    pub filename: String,
    pub mime: String,
    pub description: Option<String>,
}

/// Largest upload the core will hand to the server. Pixelfed's own limit
/// (`MAX_PHOTO_SIZE`, 15 MB by default) is enforced server-side; this only
/// stops a runaway webview from pinning memory.
pub const MAX_UPLOAD_BYTES: usize = 512 * 1024 * 1024;

pub fn request_url(session: &Session, req: &ApiRequest) -> Result<Url, AppError> {
    if !req.path.starts_with("/api/") {
        return Err(AppError::protocol(format!(
            "refusing to send the token to a non-API path: {}",
            req.path
        )));
    }
    let mut url = session.domain.base_url().join(req.path.trim_start_matches('/'))?;
    if !req.query.is_empty() {
        let mut pairs = url.query_pairs_mut();
        for (k, v) in &req.query {
            pairs.append_pair(k, v);
        }
    }
    Ok(url)
}

/// Validates a URL taken from a `Link` header before the token is attached to
/// it: https, same instance, under `/api/`.
pub fn follow_url(session: &Session, raw: &str) -> Result<Url, AppError> {
    let url = Url::parse(raw).map_err(|e| AppError::protocol(format!("bad cursor url: {e}")))?;
    if !session.domain.owns(&url) {
        return Err(AppError::protocol(format!(
            "cursor points away from {}: {}",
            session.domain, raw
        )));
    }
    if !url.path().starts_with("/api/") {
        return Err(AppError::protocol(format!("cursor is not an API path: {raw}")));
    }
    Ok(url)
}

pub async fn send(
    http: &reqwest::Client,
    session: &Session,
    method: Method,
    url: Url,
    body: Option<&Value>,
) -> Result<ApiResponse, AppError> {
    let mut builder = http
        .request(method.into(), url)
        .bearer_auth(&session.access_token)
        .header(reqwest::header::ACCEPT, "application/json");
    if let Some(json) = body {
        builder = builder.json(json);
    }
    finish(builder.send().await?).await
}

/// `POST /api/v2/media` as multipart form data with the `file` part and an
/// optional `description`. The answer is a MediaAttachment with `url` set to
/// null and `preview_url` pointing at the stored file.
pub async fn upload(
    http: &reqwest::Client,
    session: &Session,
    upload: MediaUpload,
) -> Result<ApiResponse, AppError> {
    if upload.bytes.is_empty() {
        return Err(AppError::protocol("refusing to upload an empty file"));
    }
    if upload.bytes.len() > MAX_UPLOAD_BYTES {
        return Err(AppError::protocol(format!(
            "file is larger than the {} MB the client will send",
            MAX_UPLOAD_BYTES / (1024 * 1024)
        )));
    }
    let part = reqwest::multipart::Part::bytes(upload.bytes)
        .file_name(upload.filename)
        .mime_str(&upload.mime)
        .map_err(|e| AppError::protocol(format!("bad media type: {e}")))?;
    let mut form = reqwest::multipart::Form::new().part("file", part);
    if let Some(description) = upload.description.filter(|d| !d.trim().is_empty()) {
        form = form.text("description", description);
    }
    let url = session.domain.base_url().join("api/v2/media")?;
    let response = http
        .post(url)
        .bearer_auth(&session.access_token)
        .header(reqwest::header::ACCEPT, "application/json")
        .multipart(form)
        .send()
        .await?;
    finish(response).await
}

async fn finish(response: reqwest::Response) -> Result<ApiResponse, AppError> {
    let status = response.status();
    let links = response
        .headers()
        .get(reqwest::header::LINK)
        .and_then(|v| v.to_str().ok())
        .map(parse_link_header)
        .unwrap_or_default();
    let text = response.text().await?;

    if !status.is_success() {
        return Err(AppError::Http {
            status: status.as_u16(),
            message: error_message(&text),
        });
    }

    let body = if text.trim().is_empty() {
        Value::Null
    } else {
        serde_json::from_str(&text).map_err(|e| AppError::protocol(format!("response is not JSON: {e}")))?
    };

    Ok(ApiResponse {
        status: status.as_u16(),
        body,
        next: links.next,
        prev: links.prev,
    })
}

/// Pixelfed error bodies are `{"error": "..."}` or `{"message": "..."}`.
fn error_message(text: &str) -> String {
    match serde_json::from_str::<Value>(text) {
        Ok(Value::Object(map)) => map
            .get("error")
            .or_else(|| map.get("message"))
            .and_then(Value::as_str)
            .map(str::to_string)
            .unwrap_or_else(|| text.chars().take(200).collect()),
        _ => text.chars().take(200).collect(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::Domain;

    fn session() -> Session {
        Session {
            domain: Domain::parse("pixelfed.social").unwrap(),
            client_id: "1".into(),
            client_secret: "s".into(),
            access_token: "t".into(),
            refresh_token: None,
            account: None,
        }
    }

    #[test]
    fn builds_query_urls() {
        let req = ApiRequest {
            method: Method::Get,
            path: "/api/v1/timelines/home".into(),
            query: vec![("_pe".into(), "1".into()), ("limit".into(), "20".into())],
            body: None,
        };
        let url = request_url(&session(), &req).unwrap();
        assert_eq!(
            url.as_str(),
            "https://pixelfed.social/api/v1/timelines/home?_pe=1&limit=20"
        );
    }

    #[test]
    fn refuses_non_api_paths() {
        let req = ApiRequest {
            method: Method::Get,
            path: "/oauth/token".into(),
            query: vec![],
            body: None,
        };
        assert!(request_url(&session(), &req).is_err());
    }

    #[test]
    fn follow_rejects_foreign_cursors() {
        let s = session();
        assert!(follow_url(&s, "https://pixelfed.social/api/v1/timelines/home?max_id=9").is_ok());
        assert!(follow_url(&s, "https://attacker.example/api/v1/timelines/home?max_id=9").is_err());
        assert!(follow_url(&s, "https://pixelfed.social/oauth/authorize").is_err());
        assert!(follow_url(&s, "not a url").is_err());
    }

    #[tokio::test]
    async fn upload_refuses_empty_and_oversized_files_before_the_network() {
        let http = reqwest::Client::new();
        let empty = MediaUpload {
            bytes: vec![],
            filename: "a.jpg".into(),
            mime: "image/jpeg".into(),
            description: None,
        };
        assert!(matches!(
            upload(&http, &session(), empty).await,
            Err(AppError::Protocol { .. })
        ));
        let huge = MediaUpload {
            bytes: vec![0u8; MAX_UPLOAD_BYTES + 1],
            filename: "a.jpg".into(),
            mime: "image/jpeg".into(),
            description: None,
        };
        assert!(matches!(
            upload(&http, &session(), huge).await,
            Err(AppError::Protocol { .. })
        ));
    }

    #[test]
    fn extracts_error_messages() {
        assert_eq!(
            error_message(r#"{"error":"Unauthenticated."}"#),
            "Unauthenticated."
        );
        assert_eq!(
            error_message(r#"{"message":"Too Many Attempts."}"#),
            "Too Many Attempts."
        );
        assert_eq!(error_message("<html>nope</html>"), "<html>nope</html>");
    }
}
