use std::sync::RwLock;
use std::time::Duration;

use serde::Deserialize;
use url::Url;

use crate::api::{self, ApiRequest, ApiResponse, MediaUpload};
use crate::domain::Domain;
use crate::error::AppError;
use crate::instance::{self, InstanceProbe};
use crate::oauth::{self, LoopbackListener, Pkce, RegisteredApp};
use crate::session::{AccountRef, Session, SessionSummary};
use crate::store::SecretStore;

pub const USER_AGENT: &str = concat!(
    "Pellucid/",
    env!("CARGO_PKG_VERSION"),
    " (+https://harryprayiv.com)"
);

/// How long the browser gets to finish the consent screen.
pub const LOGIN_TIMEOUT: Duration = Duration::from_secs(10 * 60);

/// The one object the shell owns. Holds the HTTP client, the secret store and
/// the current session. All methods are safe to call from any thread.
pub struct Core {
    http: reqwest::Client,
    store: Box<dyn SecretStore>,
    session: RwLock<Option<Session>>,
}

/// A login that has been started: the loopback port is bound and the app is
/// registered, but the user has not yet consented in the browser.
pub struct PendingLogin {
    domain: Domain,
    app: RegisteredApp,
    state: String,
    pkce: Pkce,
    listener: LoopbackListener,
    pub authorize_url: Url,
}

impl Core {
    /// A saved session that cannot be read counts as no session: the app
    /// starts signed out, and the file stays where it is until a sign-in
    /// replaces it or a logout removes it. Failing here would leave an app
    /// that does not open and offers nothing to repair itself with. A release
    /// build meets this case when the file was written by a development build
    /// signed in to a server on this machine (see `Domain`).
    pub fn new(store: Box<dyn SecretStore>) -> Result<Core, AppError> {
        let http = reqwest::Client::builder()
            .user_agent(USER_AGENT)
            .timeout(Duration::from_secs(30))
            .redirect(reqwest::redirect::Policy::limited(5))
            .build()?;
        let session = store.load().unwrap_or(None);
        Ok(Core {
            http,
            store,
            session: RwLock::new(session),
        })
    }

    fn current(&self) -> Result<Session, AppError> {
        self.session
            .read()
            .map_err(|_| AppError::storage("session lock poisoned"))?
            .clone()
            .ok_or(AppError::NoSession)
    }

    fn replace(&self, session: Option<Session>) -> Result<(), AppError> {
        let mut guard = self
            .session
            .write()
            .map_err(|_| AppError::storage("session lock poisoned"))?;
        *guard = session;
        Ok(())
    }

    pub fn session_summary(&self) -> Option<SessionSummary> {
        self.session
            .read()
            .ok()
            .and_then(|s| s.as_ref().map(Session::summary))
    }

    pub async fn probe(&self, domain: &Domain) -> Result<InstanceProbe, AppError> {
        instance::probe(&self.http, domain).await
    }

    /// Step one of login. Returns the URL to open in the system browser.
    pub async fn login_begin(&self, domain: Domain) -> Result<PendingLogin, AppError> {
        instance::probe(&self.http, &domain).await?;
        let listener = LoopbackListener::bind().await?;
        let app = oauth::register_app(&self.http, &domain, listener.redirect_uri()).await?;
        let state = uuid::Uuid::new_v4().simple().to_string();
        let pkce = Pkce::generate()?;
        let authorize_url = oauth::authorize_url(&domain, &app, &state, &pkce);
        Ok(PendingLogin {
            domain,
            app,
            state,
            pkce,
            listener,
            authorize_url,
        })
    }

    /// Step two: waits for the redirect, exchanges the code, verifies the
    /// token and persists the session. Resolves only when the browser dance
    /// has finished one way or the other.
    pub async fn login_finish(&self, pending: PendingLogin) -> Result<SessionSummary, AppError> {
        let code = pending
            .listener
            .wait_for_code(&pending.state, LOGIN_TIMEOUT)
            .await?;
        let token =
            oauth::exchange_code(&self.http, &pending.domain, &pending.app, &code, &pending.pkce).await?;

        let mut session = Session {
            domain: pending.domain,
            client_id: pending.app.client_id,
            client_secret: pending.app.client_secret,
            access_token: token.access_token,
            refresh_token: token.refresh_token,
            account: None,
        };
        session.account = Some(verify_credentials(&self.http, &session).await?);

        self.store.save(&session)?;
        let summary = session.summary();
        self.replace(Some(session))?;
        Ok(summary)
    }

    /// Forgets the session locally. Pixelfed's token stays valid server-side
    /// until it expires; revoking it is a later step.
    pub fn logout(&self) -> Result<(), AppError> {
        self.store.clear()?;
        self.replace(None)
    }

    pub async fn api_request(&self, req: ApiRequest) -> Result<ApiResponse, AppError> {
        let session = self.current()?;
        let url = api::request_url(&session, &req)?;
        api::send(&self.http, &session, req.method, url, req.body.as_ref()).await
    }

    /// GET a cursor URL from a previous response's `next` / `prev`.
    pub async fn api_follow(&self, raw_url: &str) -> Result<ApiResponse, AppError> {
        let session = self.current()?;
        let url = api::follow_url(&session, raw_url)?;
        api::send(&self.http, &session, api::Method::Get, url, None).await
    }

    /// Uploads one media file; the answer is the MediaAttachment entity.
    pub async fn media_upload(&self, upload: MediaUpload) -> Result<ApiResponse, AppError> {
        let session = self.current()?;
        api::upload(&self.http, &session, upload).await
    }
}

#[derive(Deserialize)]
struct VerifyCredentials {
    id: String,
    acct: String,
}

async fn verify_credentials(http: &reqwest::Client, session: &Session) -> Result<AccountRef, AppError> {
    let mut url = session
        .domain
        .base_url()
        .join("api/v1/accounts/verify_credentials")?;
    url.query_pairs_mut().append_pair("_pe", "1");
    let response = api::send(http, session, api::Method::Get, url, None).await?;
    let account: VerifyCredentials = serde_json::from_value(response.body)
        .map_err(|e| AppError::protocol(format!("verify_credentials: {e}")))?;
    Ok(AccountRef {
        id: account.id,
        acct: account.acct,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Unreadable;

    impl SecretStore for Unreadable {
        fn load(&self) -> Result<Option<Session>, AppError> {
            Err(AppError::storage("session file is corrupt"))
        }

        fn save(&self, _session: &Session) -> Result<(), AppError> {
            Ok(())
        }

        fn clear(&self) -> Result<(), AppError> {
            Ok(())
        }
    }

    #[test]
    fn a_session_that_cannot_be_read_starts_the_app_signed_out() {
        let core = Core::new(Box::new(Unreadable)).unwrap();
        assert_eq!(core.session_summary(), None);
        assert!(matches!(core.current(), Err(AppError::NoSession)));
    }
}