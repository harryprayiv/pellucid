//! Pixelfed client core.
//!
//! Owns everything that must not live in a webview: the OAuth dance, the
//! access token, and the authenticated HTTP transport. The UI (PureScript in a
//! Tauri webview) only ever sees decoded JSON entities and opaque pagination
//! cursors. Because the requests are made from Rust, the instance's CORS
//! policy is irrelevant, which is what makes talking to stock Pixelfed servers
//! possible at all: their `config/cors.php` allows no origins by default.

pub mod api;
pub mod core;
pub mod domain;
pub mod error;
pub mod instance;
pub mod link;
pub mod oauth;
pub mod session;
pub mod store;

pub use crate::api::{ApiRequest, ApiResponse, MediaUpload, Method, MAX_UPLOAD_BYTES};
pub use crate::core::{Core, PendingLogin};
pub use crate::domain::Domain;
pub use crate::error::AppError;
pub use crate::instance::InstanceProbe;
pub use crate::oauth::Pkce;
pub use crate::session::{AccountRef, Session, SessionSummary};
pub use crate::store::{FileStore, SecretStore};
