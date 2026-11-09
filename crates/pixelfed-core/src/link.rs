/// Cursors taken from an RFC 8288 `Link` response header.
/// Pellucid emits `<url>; rel="next",<url>; rel="prev"` and the URLs carry the
/// `max_id` / `min_id` the next request needs, so the client follows them
/// verbatim instead of building cursors itself.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct PageLinks {
    pub next: Option<String>,
    pub prev: Option<String>,
}

pub fn parse_link_header(value: &str) -> PageLinks {
    let mut links = PageLinks::default();
    for (target, params) in link_values(value) {
        let rel = params
            .iter()
            .find(|(k, _)| k.eq_ignore_ascii_case("rel"))
            .map(|(_, v)| v.trim_matches('"').to_ascii_lowercase());
        match rel.as_deref() {
            Some("next") => links.next = Some(target),
            Some("prev") | Some("previous") => links.prev = Some(target),
            _ => {}
        }
    }
    links
}

/// Splits a Link header into `(target, [(param, value)])` pairs. Commas inside
/// `<...>` or inside quoted parameter values do not split values.
fn link_values(header: &str) -> Vec<(String, Vec<(String, String)>)> {
    let mut out = Vec::new();
    let mut rest = header;
    while let Some(start) = rest.find('<') {
        let Some(end_rel) = rest[start..].find('>') else {
            break;
        };
        let target = rest[start + 1..start + end_rel].trim().to_string();
        rest = &rest[start + end_rel + 1..];

        let value_end = end_of_value(rest);
        let param_str = &rest[..value_end];
        rest = &rest[value_end..];
        if rest.starts_with(',') {
            rest = &rest[1..];
        }

        let params = param_str
            .split(';')
            .filter_map(|p| {
                let p = p.trim();
                if p.is_empty() {
                    return None;
                }
                let (k, v) = p.split_once('=')?;
                Some((k.trim().to_string(), v.trim().to_string()))
            })
            .collect();
        out.push((target, params));
    }
    out
}

/// Index of the comma that ends the current link-value, honouring quotes.
fn end_of_value(s: &str) -> usize {
    let mut in_quotes = false;
    for (i, c) in s.char_indices() {
        match c {
            '"' => in_quotes = !in_quotes,
            ',' if !in_quotes => return i,
            _ => {}
        }
    }
    s.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_pellucid_header() {
        let h = r#"<https://pellucid.social/api/v1/timelines/home?limit=20&_pe=1&max_id=100>; rel="next",<https://pellucid.social/api/v1/timelines/home?limit=20&_pe=1&min_id=120>; rel="prev""#;
        let links = parse_link_header(h);
        assert_eq!(
            links.next.as_deref(),
            Some("https://pellucid.social/api/v1/timelines/home?limit=20&_pe=1&max_id=100")
        );
        assert_eq!(
            links.prev.as_deref(),
            Some("https://pellucid.social/api/v1/timelines/home?limit=20&_pe=1&min_id=120")
        );
    }

    #[test]
    fn tolerates_spacing_and_unknown_rels() {
        let h =
            r#"<https://a.example/x?max_id=1> ;rel=next , <https://a.example/y>; rel="self"; title="a, b""#;
        let links = parse_link_header(h);
        assert_eq!(links.next.as_deref(), Some("https://a.example/x?max_id=1"));
        assert_eq!(links.prev, None);
    }

    #[test]
    fn empty_header_gives_no_cursors() {
        assert_eq!(parse_link_header(""), PageLinks::default());
    }
}
