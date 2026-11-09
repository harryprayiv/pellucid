use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};

use crate::error::AppError;
use crate::session::Session;

/// Where the session (and therefore the OAuth token) is persisted.
/// The desktop shell uses `FileStore`. A mobile shell plugs in the platform
/// keystore behind the same trait.
pub trait SecretStore: Send + Sync {
    fn load(&self) -> Result<Option<Session>, AppError>;
    fn save(&self, session: &Session) -> Result<(), AppError>;
    fn clear(&self) -> Result<(), AppError>;
}

/// JSON file with mode 0600 inside the app's private data directory.
/// This is as good as desktop Linux gets without a running secret-service
/// daemon; it is also exactly what pellucid-rn's MMKV store amounts to.
pub struct FileStore {
    path: PathBuf,
}

impl FileStore {
    pub fn new(data_dir: &Path) -> FileStore {
        FileStore {
            path: data_dir.join("session.json"),
        }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl SecretStore for FileStore {
    fn load(&self) -> Result<Option<Session>, AppError> {
        match fs::read(&self.path) {
            Ok(bytes) => {
                let session = serde_json::from_slice(&bytes)
                    .map_err(|e| AppError::storage(format!("session file is corrupt: {e}")))?;
                Ok(Some(session))
            }
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
            Err(e) => Err(e.into()),
        }
    }

    fn save(&self, session: &Session) -> Result<(), AppError> {
        if let Some(parent) = self.path.parent() {
            fs::create_dir_all(parent)?;
        }
        let bytes = serde_json::to_vec_pretty(session)?;
        let tmp = self.path.with_extension("json.tmp");
        {
            let mut options = fs::OpenOptions::new();
            options.write(true).create(true).truncate(true);
            #[cfg(unix)]
            {
                use std::os::unix::fs::OpenOptionsExt;
                options.mode(0o600);
            }
            let mut file = options.open(&tmp)?;
            file.write_all(&bytes)?;
            file.sync_all()?;
        }
        fs::rename(&tmp, &self.path)?;
        Ok(())
    }

    fn clear(&self) -> Result<(), AppError> {
        match fs::remove_file(&self.path) {
            Ok(()) => Ok(()),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
            Err(e) => Err(e.into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::Domain;

    #[test]
    fn round_trips_and_clears() {
        let dir = std::env::temp_dir().join(format!("pellucid-core-test-{}", uuid::Uuid::new_v4()));
        let store = FileStore::new(&dir);
        assert!(store.load().unwrap().is_none());

        let session = Session {
            domain: Domain::parse("pellucid.social").unwrap(),
            client_id: "1".into(),
            client_secret: "s".into(),
            access_token: "t".into(),
            refresh_token: None,
            account: None,
        };
        store.save(&session).unwrap();
        let loaded = store.load().unwrap().unwrap();
        assert_eq!(loaded.access_token, "t");

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mode = fs::metadata(store.path()).unwrap().permissions().mode() & 0o777;
            assert_eq!(mode, 0o600);
        }

        store.clear().unwrap();
        assert!(store.load().unwrap().is_none());
        let _ = fs::remove_dir_all(dir);
    }
}
