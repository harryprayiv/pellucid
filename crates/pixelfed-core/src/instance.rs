use serde::{Deserialize, Serialize};

use crate::domain::Domain;
use crate::error::AppError;

/// The `software.name` a server must report in its nodeinfo document. This is
/// the server's name, not this app's: Pixelfed instances report `pixelfed`
/// whatever client is asking.
pub const SERVER_SOFTWARE: &str = "pixelfed";

/// Oldest Pixelfed release whose API this client is written against.
/// pixelfed-rn enforces the same floor.
pub const MIN_PIXELFED_VERSION: (u64, u64, u64) = (0, 12, 3);

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

/// GET /api/nodeinfo/2.0.json and refuse anything that is not a recent Pixelfed.
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
/// the server must be Pixelfed and at least `MIN_PIXELFED_VERSION`.
pub fn check_software(name: &str, version: &str) -> Result<(), AppError> {
    if !name.eq_ignore_ascii_case(SERVER_SOFTWARE) {
        return Err(AppError::unsupported(format!(
            "this server runs {name}, not Pixelfed"
        )));
    }
    match parse_version(version) {
        Some(v) if v >= MIN_PIXELFED_VERSION => Ok(()),
        Some(_) => Err(AppError::unsupported(format!(
            "Pixelfed {} is older than the supported minimum {}.{}.{}",
            version, MIN_PIXELFED_VERSION.0, MIN_PIXELFED_VERSION.1, MIN_PIXELFED_VERSION.2
        ))),
        None => Err(AppError::unsupported(format!(
            "cannot read Pixelfed version {version}"
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
    fn refuses_other_software_an_old_pixelfed_and_an_unreadable_version() {
        // The app's own name is not a server anyone runs.
        assert!(matches!(
            check_software("pellucid", "0.15.2"),
            Err(AppError::Unsupported { .. })
        ));
        assert!(matches!(
            check_software("mastodon", "4.3.0"),
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
    }

    #[test]
    fn compares_as_tuples() {
        assert!(parse_version("0.12.3").unwrap() >= MIN_PIXELFED_VERSION);
        assert!(parse_version("0.12.2").unwrap() < MIN_PIXELFED_VERSION);
        assert!(parse_version("1.0.0").unwrap() > MIN_PIXELFED_VERSION);
    }
}