use serde::{Deserialize, Serialize};

use crate::domain::Domain;

/// Everything needed to act as the user on one instance. Lives only on the
/// Rust side; the webview never receives this struct.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Session {
    pub domain: Domain,
    pub client_id: String,
    pub client_secret: String,
    pub access_token: String,
    #[serde(default)]
    pub refresh_token: Option<String>,
    #[serde(default)]
    pub account: Option<AccountRef>,
}

/// The minimum the UI needs to know about who is logged in.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AccountRef {
    pub id: String,
    pub acct: String,
}

/// What the webview is allowed to see about the session.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SessionSummary {
    pub domain: String,
    pub account: Option<AccountRef>,
}

impl Session {
    pub fn summary(&self) -> SessionSummary {
        SessionSummary {
            domain: self.domain.as_str().to_string(),
            account: self.account.clone(),
        }
    }
}
