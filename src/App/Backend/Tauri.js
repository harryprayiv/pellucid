// FFI for App.Backend.Tauri. Uses the global injected by Tauri when
// `app.withGlobalTauri` is true in tauri.conf.json, so no npm packages are
// needed to build the frontend.

const tauri = () => {
  const t = globalThis.__TAURI__;
  if (!t) {
    throw new Error("window.__TAURI__ is not present; this build must run inside the Tauri shell");
  }
  return t;
};

// The promise always resolves. Rejections are folded into { ok: false, error }
// so the PureScript side decodes one shape instead of catching exceptions.
export const invokeImpl = (command) => (args) => () =>
  tauri()
    .core.invoke(command, args)
    .then(
      (value) => ({ ok: true, value: value === undefined ? null : value }),
      (error) => ({ ok: false, error: error === undefined ? null : error })
    );

// Returns the unlisten effect. Tauri's listen is itself asynchronous, so the
// unlisten waits for registration to finish before tearing it down.
export const listenImpl = (event) => (handler) => () => {
  const registration = tauri().event.listen(event, (e) => {
    handler(e.payload === undefined ? null : e.payload)();
  });
  return () => {
    registration.then((unlisten) => unlisten());
  };
};
