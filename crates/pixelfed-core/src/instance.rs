use serde::{Deserialize, Serialize};

use crate::domain::Domain;
use crate::error::AppError;

/// Oldest Pixelfed release whose API this client is written against.
/// pixelfed-rn enforces the same floor.
pub const MIN_PIXELFED_VERSION: (u64, u64, u64) = (0, 12, 3);

/// Oldest pellucid-server release this client signs in to: the first one.
pub const MIN_PELLUCID_VERSION: (u64, u64, u64) = (0, 1, 0);

/// A kind of server this client signs in to.
pub struct ServerSoftware {
    /// The `software.name` the server reports in its nodeinfo document.
    pub name: &'static str,
    /// What to call it in a message.
    pub label: &'static str,
    /// The oldest release accepted.
    pub minimum: (u64, u64, u64),
}

/// Every name here is a server's own name for itself, not this app's. A
/// Pixelfed instance reports `pixelfed` whatever client is asking. A
/// pellucid-server reports `pellucid`: the server software has that name, and
/// so does this app, and it is the server's that is compared.
pub const SUPPORTED_SERVERS: [ServerSoftware; 2] = [
    ServerSoftware {
        name: "pixelfed",
        label: "Pixelfed",
        minimum: MIN_PIXELFED_VERSION,
    },
    ServerSoftware {
        name: "pellucid",
        label: "Pellucid",
        minimum: MIN_PELLUCID_VERSION,
    },
];

#[derive(Debug, Clone, Serialize)]
pub struct InstanceProbe {
    pub domain: Domain,
    pub software: String,
    pub version: String,
}

#[derive(Deserialize)]
struct NodeInfo {
    software: NodeInfoSoftware,
}

#[derive(Deserialize)]
struct NodeInfoSoftware {
    name: String,
    version: String,
}

/// GET /api/nodeinfo/2.0.json and refuse anything that is not a supported
/// server at a supported version.
pub async fn probe(http: &reqwest::Client, domain: &Domain) -> Result<InstanceProbe, AppError> {
    let url = domain.base_url().join("api/nodeinfo/2.0.json")?;
    let response = http.get(url).send().await?;
    let status = response.status();
    if !status.is_success() {
        return Err(AppError::Http {
            status: status.as_u16(),
            message: "nodeinfo not available".into(),
        });
    }
    let info: NodeInfo = response
        .json()
        .await
        .map_err(|_| AppError::unsupported("nodeinfo document is not valid"))?;

    check_software(&info.software.name, &info.software.version)?;

    Ok(InstanceProbe {
        domain: domain.clone(),
        software: info.software.name,
        version: info.software.version,
    })
}

/// The decision `probe` makes about a nodeinfo document, without the network:
/// the server must be one of `SUPPORTED_SERVERS`, at its minimum version or
/// newer.
pub fn check_software(name: &str, version: &str) -> Result<(), AppError> {
    let Some(server) = SUPPORTED_SERVERS
        .iter()
        .find(|server| name.eq_ignore_ascii_case(server.name))
    else {
        return Err(AppError::unsupported(format!(
            "this server runs {name}, which this app cannot sign in to"
        )));
    };
    let (major, minor, patch) = server.minimum;
    match parse_version(version) {
        Some(found) if found >= server.minimum => Ok(()),
        Some(_) => Err(AppError::unsupported(format!(
            "{} {} is older than the supported minimum {}.{}.{}",
            server.label, version, major, minor, patch
        ))),
        None => Err(AppError::unsupported(format!(
            "cannot read {} version {version}",
            server.label
        ))),
    }
}

/// Reads the leading `major.minor.patch` of strings like `0.12.5`, `v0.12.5` or
/// `0.12.5-beta.1`. Anything after the patch number is ignored.
pub fn parse_version(raw: &str) -> Option<(u64, u64, u64)> {
    let core = raw.trim().trim_start_matches('v');
    let core = core.split(['-', '+', ' ']).next()?;
    let mut parts = core.split('.');
    let major = parts.next()?.parse().ok()?;
    let minor = parts.next()?.parse().ok()?;
    let patch = parts.next().unwrap_or("0").parse().ok()?;
    Some((major, minor, patch))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_release_strings() {
        assert_eq!(parse_version("0.12.5"), Some((0, 12, 5)));
        assert_eq!(parse_version("v0.12.5"), Some((0, 12, 5)));
        assert_eq!(parse_version("0.12.5-beta.2"), Some((0, 12, 5)));
        assert_eq!(parse_version("0.13"), Some((0, 13, 0)));
        assert_eq!(parse_version("nightly"), None);
    }

    #[test]
    fn accepts_a_recent_pixelfed_by_its_nodeinfo_name() {
        assert!(check_software("pixelfed", "0.12.3").is_ok());
        assert!(check_software("pixelfed", "0.15.2").is_ok());
        assert!(check_software("Pixelfed", "v0.12.5-beta.1").is_ok());
    }

    #[test]
    fn accepts_a_pellucid_server_by_its_nodeinfo_name() {
        assert!(check_software("pellucid", "0.1.0").is_ok());
        assert!(check_software("Pellucid", "0.2.0").is_ok());
        // Pixelfed's floor is not applied to it: 0.1.0 is below 0.12.3.
        assert!(check_software("pixelfed", "0.1.0").is_err());
    }

    #[test]
    fn refuses_other_software_an_old_release_and_an_unreadable_version() {
        assert!(matches!(
            check_software("mastodon", "4.3.0"),
            Err(AppError::Unsupported { .. })
        ));
        assert!(matches!(
            check_software("pixelfed-fork", "0.15.2"),
            Err(AppError::Unsupported { .. })
        ));
        assert!(matches!(
            check_software("", "0.15.2"),
            Err(AppError::Unsupported { .. })
        ));
        assert!(matches!(
            check_software("pixelfed", "0.12.2"),
            Err(AppError::Unsupported { .. })
        ));
        assert!(matches!(
            check_software("pixelfed", "nightly"),
            Err(AppError::Unsupported { .. })
        ));
        assert!(matches!(
            check_software("pellucid", "0.0.9"),
            Err(AppError::Unsupported { .. })
        ));
        assert!(matches!(
            check_software("pellucid", "nightly"),
            Err(AppError::Unsupported { .. })
        ));
    }

    #[test]
    fn compares_as_tuples() {
        assert!(parse_version("0.12.3").unwrap() >= MIN_PIXELFED_VERSION);
        assert!(parse_version("0.12.2").unwrap() < MIN_PIXELFED_VERSION);
        assert!(parse_version("1.0.0").unwrap() > MIN_PIXELFED_VERSION);
    }
}