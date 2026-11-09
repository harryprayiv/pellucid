//! Tauri shell. Every command here is a thin adapter over `pixelfed_core`;
//! no protocol logic lives in this crate.

use std::sync::Arc;

use percent_encoding::percent_decode_str;
use pixelfed_core::{
    ApiRequest, ApiResponse, AppError, Core, Domain, FileStore, InstanceProbe, MediaUpload, SessionSummary,
};
use serde::Serialize;
use tauri::ipc::{InvokeBody, Request};
use tauri::{AppHandle, Emitter, Manager, State};
use tauri_plugin_opener::OpenerExt;

/// Emitted with `Option<SessionSummary>` whenever the session appears or goes away.
pub const SESSION_CHANGED: &str = "session-changed";
/// Emitted with an `AppError` when a started login does not complete.
pub const LOGIN_FAILED: &str = "login-failed";

struct AppState {
    core: Arc<Core>,
}

#[derive(Serialize)]
struct LoginStarted {
    authorize_url: String,
}

#[tauri::command]
fn session_get(state: State<'_, AppState>) -> Option<SessionSummary> {
    state.core.session_summary()
}

#[tauri::command]
async fn instance_probe(state: State<'_, AppState>, domain: String) -> Result<InstanceProbe, AppError> {
    let domain = Domain::parse(&domain)?;
    state.core.probe(&domain).await
}

/// Registers the app, opens the consent page in the system browser and
/// returns immediately. Completion arrives as a `session-changed` event,
/// failure as `login-failed`.
#[tauri::command]
async fn login_start(
    app: AppHandle,
    state: State<'_, AppState>,
    domain: String,
) -> Result<LoginStarted, AppError> {
    let domain = Domain::parse(&domain)?;
    let pending = state.core.login_begin(domain).await?;
    let authorize_url = pending.authorize_url.to_string();

    app.opener()
        .open_url(&authorize_url, None::<&str>)
        .map_err(|e| AppError::protocol(format!("could not open the browser: {e}")))?;

    let core = Arc::clone(&state.core);
    let handle = app.clone();
    tauri::async_runtime::spawn(async move {
        match core.login_finish(pending).await {
            Ok(summary) => {
                let _ = handle.emit(SESSION_CHANGED, Some(summary));
            }
            Err(err) => {
                let _ = handle.emit(LOGIN_FAILED, err);
            }
        }
    });

    Ok(LoginStarted { authorize_url })
}

#[tauri::command]
fn logout(app: AppHandle, state: State<'_, AppState>) -> Result<(), AppError> {
    state.core.logout()?;
    let _ = app.emit(SESSION_CHANGED, None::<SessionSummary>);
    Ok(())
}

#[tauri::command]
async fn api_request(state: State<'_, AppState>, request: ApiRequest) -> Result<ApiResponse, AppError> {
    state.core.api_request(request).await
}

#[tauri::command]
async fn api_follow(state: State<'_, AppState>, url: String) -> Result<ApiResponse, AppError> {
    state.core.api_follow(&url).await
}

/// The file bytes travel as the raw IPC body, so nothing is base64 encoded
/// and nothing is written to disk. Metadata rides in percent-encoded headers.
#[tauri::command]
async fn media_upload(state: State<'_, AppState>, request: Request<'_>) -> Result<ApiResponse, AppError> {
    let bytes = match request.body() {
        InvokeBody::Raw(bytes) => bytes.clone(),
        InvokeBody::Json(_) => {
            return Err(AppError::protocol("media_upload expects a raw body, not JSON"));
        }
    };
    let header = |name: &str| -> Option<String> {
        request
            .headers()
            .get(name)
            .and_then(|v| v.to_str().ok())
            .map(|v| percent_decode_str(v).decode_utf8_lossy().into_owned())
            .filter(|v| !v.is_empty())
    };
    let upload = MediaUpload {
        bytes,
        filename: header("x-filename").unwrap_or_else(|| "upload".into()),
        mime: header("x-mime").unwrap_or_else(|| "application/octet-stream".into()),
        description: header("x-description"),
    };
    state.core.media_upload(upload).await
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    tauri::Builder::default()
        .plugin(tauri_plugin_opener::init())
        .setup(|app| {
            let data_dir = app.path().app_data_dir()?;
            let core = Core::new(Box::new(FileStore::new(&data_dir)))?;
            app.manage(AppState { core: Arc::new(core) });
            Ok(())
        })
        .invoke_handler(tauri::generate_handler![
            session_get,
            instance_probe,
            login_start,
            logout,
            api_request,
            api_follow,
            media_upload
        ])
        .run(tauri::generate_context!())
        .expect("error while running Pellucid");
}
