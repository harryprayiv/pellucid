use std::fmt;
use std::net::Ipv4Addr;

use serde::{Deserialize, Serialize};
use url::{Host, Url};

use crate::error::AppError;

/// A validated server address, e.g. `pixelfed.social` or `pix.example.org:8443`.
/// Constructing one is the only way to get a base URL, so every request in the
/// crate targets an origin the user typed.
///
/// That origin is https. There is one exception, and only a development build
/// has it: a server on this machine, typed with its scheme and by its address,
/// as in `http://127.0.0.1:8080`. A request to that address never leaves the
/// machine, so there is no network for the token to be read from. The name
/// `localhost` does not get the exception, because a name is whatever the
/// resolver says it is. A release build cannot construct such a value: it
/// refuses the text when it is typed and when it is read back from a saved
/// session.
///
/// Nothing is ever downgraded. `http://` in front of any other host is taken
/// as a pasted link and the server is reached over https, as before, and so is
/// `127.0.0.1` typed without a scheme.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct Domain {
    /// The address as it is stored and shown: `host` or `host:port` for an
    /// https server, and the same behind `http://` for the loopback exception.
    text: String,
    scheme: Scheme,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Scheme {
    Https,
    LoopbackHttp,
}

impl Domain {
    /// Accepts what people paste: optional scheme, trailing slashes, mixed case.
    /// Rejects anything with a path, query, userinfo or characters that are not
    /// part of a host.
    pub fn parse(input: &str) -> Result<Domain, AppError> {
        Domain::parse_with(input, cfg!(debug_assertions))
    }

    /// `parse`, with the loopback exception switched on or off by the caller
    /// instead of by the build, so that the tests cover both builds' behaviour
    /// whichever one they are compiled in.
    fn parse_with(input: &str, development: bool) -> Result<Domain, AppError> {
        let trimmed = input.trim().trim_end_matches('/');
        let (typed_http, without_scheme) = match trimmed.strip_prefix("https://") {
            Some(rest) => (false, rest),
            None => match trimmed.strip_prefix("http://") {
                Some(rest) => (true, rest),
                None => (false, trimmed),
            },
        };
        let without_scheme = without_scheme.trim_end_matches('/');

        if without_scheme.is_empty() {
            return Err(AppError::InvalidDomain {
                message: "domain is empty".into(),
            });
        }
        if without_scheme.contains(['/', '?', '#', '@', ' ']) {
            return Err(AppError::InvalidDomain {
                message: "expected a bare host like pixelfed.social".into(),
            });
        }

        let candidate = without_scheme.to_ascii_lowercase();
        let https = Domain::authority("https", &candidate)?;

        let this_machine =
            matches!(https.host(), Some(Host::Ipv4(address)) if address == Ipv4Addr::LOCALHOST);
        if typed_http && this_machine {
            if !development {
                return Err(AppError::InvalidDomain {
                    message:
                        "a server on this machine over plain http is accepted by development builds only"
                            .into(),
                });
            }
            // Read again as http, so that the port that goes without saying is
            // http's and not https's.
            let http = Domain::authority("http", &candidate)?;
            return Ok(Domain {
                text: format!("http://{}", Domain::host_and_port(&http)?),
                scheme: Scheme::LoopbackHttp,
            });
        }

        Ok(Domain {
            text: Domain::host_and_port(&https)?,
            scheme: Scheme::Https,
        })
    }

    fn authority(scheme: &str, candidate: &str) -> Result<Url, AppError> {
        Url::parse(&format!("{scheme}://{candidate}/")).map_err(|e| AppError::InvalidDomain {
            message: e.to_string(),
        })
    }

    fn host_and_port(url: &Url) -> Result<String, AppError> {
        let host = url.host_str().ok_or_else(|| AppError::InvalidDomain {
            message: "no host".into(),
        })?;
        if !host.contains('.') && host != "localhost" {
            return Err(AppError::InvalidDomain {
                message: "host needs a dot (pixelfed.social, not pixelfed)".into(),
            });
        }
        Ok(match url.port() {
            Some(port) => format!("{host}:{port}"),
            None => host.to_string(),
        })
    }

    pub fn as_str(&self) -> &str {
        &self.text
    }

    /// `https://{domain}/`, or `http://{domain}/` for the loopback exception.
    pub fn base_url(&self) -> Url {
        let text = match self.scheme {
            Scheme::Https => format!("https://{}/", self.text),
            Scheme::LoopbackHttp => format!("{}/", self.text),
        };
        // Validated in `parse_with`, so this cannot fail.
        Url::parse(&text).expect("validated domain")
    }

    /// True when `url` points at this server: the same scheme, host and port.
    /// For every domain but the loopback exception the scheme is https.
    pub fn owns(&self, url: &Url) -> bool {
        let base = self.base_url();
        url.scheme() == base.scheme()
            && url.host_str().map(str::to_ascii_lowercase) == base.host_str().map(str::to_ascii_lowercase)
            && url.port_or_known_default() == base.port_or_known_default()
    }
}

impl fmt::Display for Domain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.text)
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
        value.text
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strips_scheme_and_slashes() {
        assert_eq!(
            Domain::parse(" https://Pixelfed.Social/ ").unwrap().as_str(),
            "pixelfed.social"
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
        assert!(Domain::parse("pixelfed.social/api").is_err());
        assert!(Domain::parse("user@pixelfed.social").is_err());
        assert!(Domain::parse("").is_err());
        assert!(Domain::parse("pixelfed").is_err());
    }

    #[test]
    fn owns_only_same_https_origin() {
        let d = Domain::parse("pixelfed.social").unwrap();
        assert!(d.owns(&Url::parse("https://pixelfed.social/api/v1/timelines/home?max_id=1").unwrap()));
        assert!(!d.owns(&Url::parse("http://pixelfed.social/api/v1/timelines/home").unwrap()));
        assert!(!d.owns(&Url::parse("https://evil.example/api/v1/timelines/home").unwrap()));
        assert!(!d.owns(&Url::parse("https://pixelfed.social:8443/api").unwrap()));
    }

    #[test]
    fn http_in_front_of_another_host_is_reached_over_https_in_both_builds() {
        let upgraded = [
            ("http://pixelfed.social", "https://pixelfed.social/"),
            // A private address is another machine.
            ("http://192.168.1.10:8080", "https://192.168.1.10:8080/"),
            // So is a name that only begins like this machine's address.
            (
                "http://127.0.0.1.evil.example:8080",
                "https://127.0.0.1.evil.example:8080/",
            ),
            // A name resolves to whatever the resolver says, and the rest of
            // the loopback range is outside what the webview is allowed to
            // load pictures from.
            ("http://localhost:8080", "https://localhost:8080/"),
            ("http://127.0.0.2:8080", "https://127.0.0.2:8080/"),
        ];
        for development in [true, false] {
            for (typed, reached) in upgraded {
                let d = Domain::parse_with(typed, development).unwrap();
                assert_eq!(d.base_url().as_str(), reached);
                assert!(!d.as_str().contains("://"));
            }
        }
    }

    #[test]
    fn this_machine_without_a_scheme_is_https_in_both_builds() {
        for development in [true, false] {
            let d = Domain::parse_with("127.0.0.1:8080", development).unwrap();
            assert_eq!(d.base_url().as_str(), "https://127.0.0.1:8080/");
            let explicit = Domain::parse_with("https://127.0.0.1:8443", development).unwrap();
            assert_eq!(explicit.base_url().as_str(), "https://127.0.0.1:8443/");
        }
    }

    #[test]
    fn a_development_build_reaches_a_server_on_this_machine_over_http() {
        let d = Domain::parse_with(" http://127.0.0.1:8080/ ", true).unwrap();
        assert_eq!(d.as_str(), "http://127.0.0.1:8080");
        assert_eq!(d.base_url().as_str(), "http://127.0.0.1:8080/");
        // Other spellings of the same address are stored as the one spelling.
        assert_eq!(
            Domain::parse_with("http://127.1:8080", true).unwrap().as_str(),
            "http://127.0.0.1:8080"
        );
        // http's own port goes without saying; https's does not.
        assert_eq!(
            Domain::parse_with("http://127.0.0.1:80", true).unwrap().as_str(),
            "http://127.0.0.1"
        );
        assert_eq!(
            Domain::parse_with("http://127.0.0.1:443", true).unwrap().as_str(),
            "http://127.0.0.1:443"
        );
    }

    #[test]
    fn a_release_build_refuses_plain_http_even_to_this_machine() {
        assert!(matches!(
            Domain::parse_with("http://127.0.0.1:8080", false),
            Err(AppError::InvalidDomain { .. })
        ));
        assert!(matches!(
            Domain::parse_with("http://127.1:8080", false),
            Err(AppError::InvalidDomain { .. })
        ));
    }

    #[test]
    fn a_server_on_this_machine_owns_only_its_own_http_origin() {
        let d = Domain::parse_with("http://127.0.0.1:8080", true).unwrap();
        assert!(d.owns(&Url::parse("http://127.0.0.1:8080/api/v1/timelines/home?cursor=x").unwrap()));
        assert!(!d.owns(&Url::parse("https://127.0.0.1:8080/api/v1/timelines/home").unwrap()));
        assert!(!d.owns(&Url::parse("http://127.0.0.1:9090/api/v1/timelines/home").unwrap()));
        assert!(!d.owns(&Url::parse("http://evil.example:8080/api/v1/timelines/home").unwrap()));
        assert!(!d.owns(&Url::parse("http://192.168.1.10:8080/api/v1/timelines/home").unwrap()));
    }

    #[test]
    fn a_session_on_this_machine_reads_back_only_in_a_build_that_could_write_it() {
        let text = String::from(Domain::parse_with("http://127.0.0.1:8080", true).unwrap());
        assert_eq!(text, "http://127.0.0.1:8080");
        // Reading back goes through `parse`, so it follows the build.
        assert_eq!(Domain::try_from(text).is_ok(), cfg!(debug_assertions));
    }
}