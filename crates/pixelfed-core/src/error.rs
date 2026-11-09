use serde::Serialize;
use thiserror::Error;

/// Every failure the UI can observe. Serialised as `{ "kind": ..., ...fields }`
/// so the PureScript side decodes it into a closed sum type.
#[derive(Debug, Clone, PartialEq, Eq, Error, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum AppError {
    #[error("no active session")]
    NoSession,

    #[error("invalid domain: {message}")]
    InvalidDomain { message: String },

    #[error("network error: {message}")]
    Network { message: String },

    #[error("server returned HTTP {status}: {message}")]
    Http { status: u16, message: String },

    #[error("protocol error: {message}")]
    Protocol { message: String },

    #[error("unsupported server: {message}")]
    Unsupported { message: String },

    #[error("storage error: {message}")]
    Storage { message: String },

    #[error("login aborted: {message}")]
    LoginAborted { message: String },
}

impl AppError {
    pub fn protocol(message: impl Into<String>) -> Self {
        AppError::Protocol {
            message: message.into(),
        }
    }

    pub fn unsupported(message: impl Into<String>) -> Self {
        AppError::Unsupported {
            message: message.into(),
        }
    }

    pub fn storage(message: impl Into<String>) -> Self {
        AppError::Storage {
            message: message.into(),
        }
    }

    pub fn aborted(message: impl Into<String>) -> Self {
        AppError::LoginAborted {
            message: message.into(),
        }
    }
}

impl From<reqwest::Error> for AppError {
    fn from(err: reqwest::Error) -> Self {
        AppError::Network {
            message: err.to_string(),
        }
    }
}

impl From<std::io::Error> for AppError {
    fn from(err: std::io::Error) -> Self {
        AppError::Storage {
            message: err.to_string(),
        }
    }
}

impl From<serde_json::Error> for AppError {
    fn from(err: serde_json::Error) -> Self {
        AppError::Protocol {
            message: err.to_string(),
        }
    }
}

impl From<url::ParseError> for AppError {
    fn from(err: url::ParseError) -> Self {
        AppError::Protocol {
            message: err.to_string(),
        }
    }
}
