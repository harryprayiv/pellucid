use std::fmt;

use serde::{Deserialize, Serialize};
use url::Url;

use crate::error::AppError;

/// A validated instance host, e.g. `pellucid.social` or `pix.example.org:8443`.
/// Constructing one is the only way to get a base URL, so every request in the
/// crate is guaranteed to target an https origin the user actually typed.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct Domain(String);

impl Domain {
    /// Accepts what people paste: optional scheme, trailing slashes, mixed case.
    /// Rejects anything with a path, query, userinfo or characters that are not
    /// part of a host.
    pub fn parse(input: &str) -> Result<Domain, AppError> {
        let trimmed = input.trim().trim_end_matches('/');
        let without_scheme = trimmed
            .strip_prefix("https://")
            .or_else(|| trimmed.strip_prefix("http://"))
            .unwrap_or(trimmed)
            .trim_end_matches('/');

        if without_scheme.is_empty() {
            return Err(AppError::InvalidDomain {
                message: "domain is empty".into(),
            });
        }
        if without_scheme.contains(['/', '?', '#', '@', ' ']) {
            return Err(AppError::InvalidDomain {
                message: "expected a bare host like pellucid.social".into(),
            });
        }

        let candidate = without_scheme.to_ascii_lowercase();
        let url = Url::parse(&format!("https://{candidate}/")).map_err(|e| AppError::InvalidDomain {
            message: e.to_string(),
        })?;

        let host = url.host_str().ok_or_else(|| AppError::InvalidDomain {
            message: "no host".into(),
        })?;
        if !host.contains('.') && host != "localhost" {
            return Err(AppError::InvalidDomain {
                message: "host needs a dot (pellucid.social, not pellucid)".into(),
            });
        }

        let normalised = match url.port() {
            Some(port) => format!("{host}:{port}"),
            None => host.to_string(),
        };
        Ok(Domain(normalised))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// `https://{domain}/`
    pub fn base_url(&self) -> Url {
        // Validated in `parse`, so this cannot fail.
        Url::parse(&format!("https://{}/", self.0)).expect("validated domain")
    }

    /// True when `url` points at this instance over https.
    pub fn owns(&self, url: &Url) -> bool {
        if url.scheme() != "https" {
            return false;
        }
        let base = self.base_url();
        url.host_str().map(str::to_ascii_lowercase) == base.host_str().map(str::to_ascii_lowercase)
            && url.port_or_known_default() == base.port_or_known_default()
    }
}

impl fmt::Display for Domain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl TryFrom<String> for Domain {
    type Error = AppError;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Domain::parse(&value)
    }
}

impl From<Domain> for String {
    fn from(value: Domain) -> Self {
        value.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strips_scheme_and_slashes() {
        assert_eq!(
            Domain::parse(" https://Pellucid.Social/ ").unwrap().as_str(),
            "pellucid.social"
        );
    }

    #[test]
    fn keeps_explicit_port() {
        assert_eq!(
            Domain::parse("pix.example.org:8443").unwrap().as_str(),
            "pix.example.org:8443"
        );
    }

    #[test]
    fn rejects_paths_and_userinfo() {
        assert!(Domain::parse("pellucid.social/api").is_err());
        assert!(Domain::parse("user@pellucid.social").is_err());
        assert!(Domain::parse("").is_err());
        assert!(Domain::parse("pellucid").is_err());
    }

    #[test]
    fn owns_only_same_https_origin() {
        let d = Domain::parse("pellucid.social").unwrap();
        assert!(d.owns(&Url::parse("https://pellucid.social/api/v1/timelines/home?max_id=1").unwrap()));
        assert!(!d.owns(&Url::parse("http://pellucid.social/api/v1/timelines/home").unwrap()));
        assert!(!d.owns(&Url::parse("https://evil.example/api/v1/timelines/home").unwrap()));
        assert!(!d.owns(&Url::parse("https://pellucid.social:8443/api").unwrap()));
    }
}
