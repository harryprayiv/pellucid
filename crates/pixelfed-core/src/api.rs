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
    let response = builder.send().await?;
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

/// Pellucid error bodies are `{"error": "..."}` or `{"message": "..."}`.
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
            domain: Domain::parse("pellucid.social").unwrap(),
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
            "https://pellucid.social/api/v1/timelines/home?_pe=1&limit=20"
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
        assert!(follow_url(&s, "https://pellucid.social/api/v1/timelines/home?max_id=9").is_ok());
        assert!(follow_url(&s, "https://attacker.example/api/v1/timelines/home?max_id=9").is_err());
        assert!(follow_url(&s, "https://pellucid.social/oauth/authorize").is_err());
        assert!(follow_url(&s, "not a url").is_err());
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
