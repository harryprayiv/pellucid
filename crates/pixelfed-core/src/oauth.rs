use std::time::Duration;

use serde::Deserialize;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpListener;
use url::Url;

use crate::domain::Domain;
use crate::error::AppError;

/// Scopes pixelfed-rn asks for by default. Admin scopes are deliberately not
/// requested; the moderation screens are a separate port.
pub const SCOPES: &str = "read write follow push";

/// Shown to the user on the instance's consent page.
pub const CLIENT_NAME: &str = "Pellucid";
pub const CLIENT_WEBSITE: &str = "https://harryprayiv.com";

/// Result of `POST /api/v1/apps`. Pixelfed issues a fresh client per login,
/// exactly like pixelfed-rn does.
#[derive(Debug, Clone)]
pub struct RegisteredApp {
    pub client_id: String,
    pub client_secret: String,
    pub redirect_uri: String,
}

#[derive(Deserialize)]
struct AppsResponse {
    client_id: String,
    client_secret: String,
}

pub async fn register_app(
    http: &reqwest::Client,
    domain: &Domain,
    redirect_uri: &str,
) -> Result<RegisteredApp, AppError> {
    let url = domain.base_url().join("api/v1/apps")?;
    let response = http
        .post(url)
        .form(&[
            ("client_name", CLIENT_NAME),
            ("redirect_uris", redirect_uri),
            ("scopes", SCOPES),
            ("website", CLIENT_WEBSITE),
        ])
        .send()
        .await?;
    let status = response.status();
    if !status.is_success() {
        let message = response.text().await.unwrap_or_default();
        return Err(AppError::Http {
            status: status.as_u16(),
            message: if message.is_empty() {
                "app registration refused".into()
            } else {
                message
            },
        });
    }
    let body: AppsResponse = response
        .json()
        .await
        .map_err(|e| AppError::protocol(format!("unexpected /api/v1/apps response: {e}")))?;
    Ok(RegisteredApp {
        client_id: body.client_id,
        client_secret: body.client_secret,
        redirect_uri: redirect_uri.to_string(),
    })
}

/// The URL the system browser is sent to. `state` is checked when the
/// redirect comes back so a stray request to the loopback port cannot
/// complete someone else's login.
pub fn authorize_url(domain: &Domain, app: &RegisteredApp, state: &str) -> Url {
    let mut url = domain.base_url().join("oauth/authorize").expect("static path");
    url.query_pairs_mut()
        .append_pair("client_id", &app.client_id)
        .append_pair("redirect_uri", &app.redirect_uri)
        .append_pair("response_type", "code")
        .append_pair("scope", SCOPES)
        .append_pair("state", state);
    url
}

#[derive(Debug, Clone, Deserialize)]
pub struct TokenResponse {
    pub access_token: String,
    #[serde(default)]
    pub refresh_token: Option<String>,
    #[serde(default)]
    pub expires_in: Option<u64>,
}

pub async fn exchange_code(
    http: &reqwest::Client,
    domain: &Domain,
    app: &RegisteredApp,
    code: &str,
) -> Result<TokenResponse, AppError> {
    let url = domain.base_url().join("oauth/token")?;
    let response = http
        .post(url)
        .form(&[
            ("grant_type", "authorization_code"),
            ("code", code),
            ("client_id", app.client_id.as_str()),
            ("client_secret", app.client_secret.as_str()),
            ("redirect_uri", app.redirect_uri.as_str()),
            ("scope", SCOPES),
        ])
        .send()
        .await?;
    let status = response.status();
    if !status.is_success() {
        let message = response.text().await.unwrap_or_default();
        return Err(AppError::Http {
            status: status.as_u16(),
            message: if message.is_empty() {
                "token exchange refused".into()
            } else {
                message
            },
        });
    }
    response
        .json()
        .await
        .map_err(|e| AppError::protocol(format!("unexpected /oauth/token response: {e}")))
}

/// One-shot HTTP listener on 127.0.0.1 that receives the OAuth redirect on
/// desktop. Mobile shells replace this with a deep link; the rest of the flow
/// is unchanged.
pub struct LoopbackListener {
    listener: TcpListener,
    redirect_uri: String,
}

const CALLBACK_PATH: &str = "/callback";

impl LoopbackListener {
    pub async fn bind() -> Result<LoopbackListener, AppError> {
        let listener = TcpListener::bind(("127.0.0.1", 0)).await?;
        let port = listener.local_addr()?.port();
        Ok(LoopbackListener {
            listener,
            redirect_uri: format!("http://127.0.0.1:{port}{CALLBACK_PATH}"),
        })
    }

    pub fn redirect_uri(&self) -> &str {
        &self.redirect_uri
    }

    /// Waits for `GET /callback?code=...&state=...` with the expected state and
    /// returns the code. Unrelated requests (favicons, wrong state) are answered
    /// and ignored. Gives up after `timeout`.
    pub async fn wait_for_code(self, expected_state: &str, timeout: Duration) -> Result<String, AppError> {
        tokio::time::timeout(timeout, self.accept_loop(expected_state))
            .await
            .map_err(|_| AppError::aborted("no response from the browser within the time limit"))?
    }

    async fn accept_loop(self, expected_state: &str) -> Result<String, AppError> {
        loop {
            let (mut socket, _) = self.listener.accept().await?;
            let mut buf = vec![0u8; 8192];
            let mut filled = 0usize;
            loop {
                if filled == buf.len() {
                    break;
                }
                let n = socket.read(&mut buf[filled..]).await?;
                if n == 0 {
                    break;
                }
                filled += n;
                if buf[..filled].windows(4).any(|w| w == b"\r\n\r\n") {
                    break;
                }
            }
            let head = String::from_utf8_lossy(&buf[..filled]);
            let request_line = head.lines().next().unwrap_or_default();

            match classify(request_line, expected_state) {
                Callback::Code(code) => {
                    respond(&mut socket, 200, SUCCESS_PAGE).await;
                    return Ok(code);
                }
                Callback::Denied(message) => {
                    respond(&mut socket, 200, FAILURE_PAGE).await;
                    return Err(AppError::aborted(message));
                }
                Callback::Ignore => {
                    respond(&mut socket, 404, "").await;
                }
            }
        }
    }
}

enum Callback {
    Code(String),
    Denied(String),
    Ignore,
}

/// Interprets the request line of the redirect. Anything that is not our
/// callback path with the right state is ignored, so port scanners and
/// browser prefetches cannot terminate the login.
fn classify(request_line: &str, expected_state: &str) -> Callback {
    let mut parts = request_line.split_whitespace();
    let method = parts.next().unwrap_or_default();
    let target = parts.next().unwrap_or_default();
    if method != "GET" {
        return Callback::Ignore;
    }
    let Ok(url) = Url::parse(&format!("http://127.0.0.1{target}")) else {
        return Callback::Ignore;
    };
    if url.path() != CALLBACK_PATH {
        return Callback::Ignore;
    }
    let pairs: Vec<(String, String)> = url
        .query_pairs()
        .map(|(k, v)| (k.into_owned(), v.into_owned()))
        .collect();
    let get = |key: &str| pairs.iter().find(|(k, _)| k == key).map(|(_, v)| v.clone());

    if get("state").as_deref() != Some(expected_state) {
        return Callback::Ignore;
    }
    if let Some(error) = get("error") {
        let description = get("error_description").unwrap_or_default();
        return Callback::Denied(if description.is_empty() {
            error
        } else {
            format!("{error}: {description}")
        });
    }
    match get("code") {
        Some(code) if !code.is_empty() => Callback::Code(code),
        _ => Callback::Denied("the instance did not return an authorization code".into()),
    }
}

async fn respond(socket: &mut tokio::net::TcpStream, status: u16, body: &str) {
    let reason = match status {
        200 => "OK",
        404 => "Not Found",
        _ => "Bad Request",
    };
    let response = format!(
        "HTTP/1.1 {status} {reason}\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
        body.len()
    );
    let _ = socket.write_all(response.as_bytes()).await;
    let _ = socket.shutdown().await;
}

const SUCCESS_PAGE: &str = "<!doctype html><meta charset=utf-8><title>Logged in</title><body style=\"font-family:sans-serif;padding:2rem\"><h1>Logged in</h1><p>You can close this tab and return to the app.</p></body>";
const FAILURE_PAGE: &str = "<!doctype html><meta charset=utf-8><title>Login failed</title><body style=\"font-family:sans-serif;padding:2rem\"><h1>Login was not completed</h1><p>Return to the app and try again.</p></body>";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn authorize_url_carries_every_parameter() {
        let domain = Domain::parse("pixelfed.social").unwrap();
        let app = RegisteredApp {
            client_id: "42".into(),
            client_secret: "secret".into(),
            redirect_uri: "http://127.0.0.1:5555/callback".into(),
        };
        let url = authorize_url(&domain, &app, "abc");
        assert_eq!(url.host_str(), Some("pixelfed.social"));
        assert_eq!(url.path(), "/oauth/authorize");
        let q: Vec<(String, String)> = url
            .query_pairs()
            .map(|(k, v)| (k.into_owned(), v.into_owned()))
            .collect();
        assert!(q.contains(&("client_id".into(), "42".into())));
        assert!(q.contains(&("response_type".into(), "code".into())));
        assert!(q.contains(&("scope".into(), SCOPES.into())));
        assert!(q.contains(&("state".into(), "abc".into())));
        assert!(!url.as_str().contains("secret"));
    }

    #[test]
    fn classifies_callbacks() {
        assert!(matches!(
            classify("GET /callback?code=xyz&state=s1 HTTP/1.1", "s1"),
            Callback::Code(c) if c == "xyz"
        ));
        assert!(matches!(
            classify("GET /callback?code=xyz&state=other HTTP/1.1", "s1"),
            Callback::Ignore
        ));
        assert!(matches!(
            classify("GET /favicon.ico HTTP/1.1", "s1"),
            Callback::Ignore
        ));
        assert!(matches!(
            classify("POST /callback?code=x&state=s1 HTTP/1.1", "s1"),
            Callback::Ignore
        ));
        assert!(matches!(
            classify("GET /callback?error=access_denied&state=s1 HTTP/1.1", "s1"),
            Callback::Denied(_)
        ));
    }

    #[tokio::test]
    async fn loopback_round_trip() {
        let listener = LoopbackListener::bind().await.unwrap();
        let uri = listener.redirect_uri().to_string();
        let waiter = tokio::spawn(listener.wait_for_code("st", Duration::from_secs(5)));

        let url = Url::parse(&uri).unwrap();
        let addr = format!("127.0.0.1:{}", url.port().unwrap());
        let mut stream = tokio::net::TcpStream::connect(addr).await.unwrap();
        stream
            .write_all(b"GET /callback?code=the-code&state=st HTTP/1.1\r\nHost: x\r\n\r\n")
            .await
            .unwrap();
        let mut reply = String::new();
        stream.read_to_string(&mut reply).await.unwrap();
        assert!(reply.starts_with("HTTP/1.1 200"));

        assert_eq!(waiter.await.unwrap().unwrap(), "the-code");
    }
}