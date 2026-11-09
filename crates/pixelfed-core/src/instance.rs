use serde::{Deserialize, Serialize};

use crate::domain::Domain;
use crate::error::AppError;

/// Oldest Pellucid release whose API this client is written against.
/// pellucid-rn enforces the same floor.
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

/// GET /api/nodeinfo/2.0.json and refuse anything that is not a recent Pellucid.
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

    if !info.software.name.eq_ignore_ascii_case("pellucid") {
        return Err(AppError::unsupported(format!(
            "this server runs {}, not Pellucid",
            info.software.name
        )));
    }

    match parse_version(&info.software.version) {
        Some(v) if v >= MIN_PIXELFED_VERSION => {}
        Some(_) => {
            return Err(AppError::unsupported(format!(
                "Pellucid {} is older than the supported minimum {}.{}.{}",
                info.software.version, MIN_PIXELFED_VERSION.0, MIN_PIXELFED_VERSION.1, MIN_PIXELFED_VERSION.2
            )))
        }
        None => {
            return Err(AppError::unsupported(format!(
                "cannot read Pellucid version {}",
                info.software.version
            )))
        }
    }

    Ok(InstanceProbe {
        domain: domain.clone(),
        software: info.software.name,
        version: info.software.version,
    })
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
    fn compares_as_tuples() {
        assert!(parse_version("0.12.3").unwrap() >= MIN_PIXELFED_VERSION);
        assert!(parse_version("0.12.2").unwrap() < MIN_PIXELFED_VERSION);
        assert!(parse_version("1.0.0").unwrap() > MIN_PIXELFED_VERSION);
    }
}
