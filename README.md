# Pellucid

Pellucid is a Pixelfed client. The user interface is PureScript (Halogen) running in a Tauri 2 webview; the network and secret handling is Rust. It speaks the API that pixelfed-rn speaks to stock Pixelfed servers today, so it works against pixelfed.social and any other instance running Pixelfed 0.12.3 or newer. No Pellucid server exists yet; a server is a later project with its own specification.

The name means transparently clear. The name claims nothing by itself. What makes the claim is code you can read, a gate that refuses warnings, a specification the code is held to, and, later, reproducible builds.

Pellucid is not affiliated with the Pixelfed project.

## Status

This is early work and is not released. The desktop Linux build is the only one that has been exercised. What is built:

- Login to any Pixelfed instance through OAuth 2 in the system browser, with the session persisted locally.
- Home and public timelines with cursor pagination, likes, bookmarks, sensitive media covers, albums and video.
- Post detail with comments, replying, deleting your own comments, and likes on comments.
- Profiles with follow, unfollow and follow requests, a three-wide grid of posts with paging, and the Me tab.

What is not built: notifications, compose and media upload, search and discover, hashtags, direct messages, followers and following lists, stories, settings, moderation, infinite scroll (feeds end in a Load more button), blurhash placeholders, hashtag and mention links, token revocation on logout, the Android and iOS shells, and a Nix package for the bundle and the binary. The specification lists each of these as a surface or a known issue.

## Why this shape

Pixelfed's `config/cors.php` allows no origins by default. Only `/api/v1/apps` and `/api/v2/instance` set `Access-Control-Allow-Origin`. Timelines, `/oauth/token` and `verify_credentials` send no CORS header at all. A web page in a browser cannot log in to a stock instance, so every request has to come from a native HTTP client. Pellucid's requests come from Rust.

Since Rust has to make the requests anyway, the access token has no reason to exist in the webview. The webview receives the instance domain and the account id and handle, never the token, the client secret or the refresh token. The Rust side refuses to attach the token to any path outside `/api/` and to any pagination cursor that does not point at the logged-in instance over https.

## Architecture

```
PureScript (Halogen)          Rust (crates/pixelfed-core)   Tauri shell (src-tauri)
Component.Root                core: session, http client    six commands, two events
Component.Login   -invoke->   oauth: apps, authorize URL,   opens the system browser,
Component.Feed                loopback listener, token      emits session-changed
Component.StatusDetail        api: request_url, follow,     and login-failed,
Component.Profile             send                          FileStore with mode 0600
View.StatusCard   <-event-    link: Link header parser
Pixelfed.Client               domain: validated https host
Pixelfed.Decode               store: SecretStore trait
App.Backend (the seam)        error: closed AppError sum
```

`crates/pixelfed-core` has no UI and no Tauri dependency. It owns the OAuth dance, the token, the authenticated transport and the validation of every URL the token travels to. It has unit tests, including a loopback OAuth round trip.

`src-tauri` is an adapter. Each command calls one core method; the only extra behaviour is opening the browser and emitting the two events. It holds no protocol logic.

`src` is the PureScript application. `App.Backend` is a record of functions, one per shell command plus two subscriptions, and `App.Backend.Tauri` implements it over `window.__TAURI__`. Screens import the seam, never the Tauri module. Entities and decoders are written against Pixelfed's own transformers rather than the Mastodon specification, because Pixelfed omits some fields, nulls others and adds its own.

There is no npm, no package.json and no node_modules. The frontend is spago packages plus one hand-written foreign module, `src/App/Backend/Tauri.js`, which exists because Tauri's `withGlobalTauri` setting exposes `invoke` and `listen` on the window object. esbuild comes from nixpkgs and is driven by `spago bundle`.

## Security properties

These are invariants in `.inference/pellucid.allium` and the code is written to them.

- The token never crosses the bridge. No command result and no event payload contains `access_token`, `client_secret` or `refresh_token`.
- The token goes only to `/api/` paths on the logged-in origin over https. A pagination cursor is dereferenced only after Rust parses it and checks the scheme, the host, the port and the path prefix.
- No HTML is rendered as HTML. Captions are the server's `content_text`. When a server omits it, the HTML `content` is reduced to text and set as a text node. `innerHTML` is not used anywhere.
- The webview's Content Security Policy allows scripts and styles from the bundle only, images and media over https, and the Tauri IPC origin. The capability file grants `core:default` and nothing else.
- Redirects are limited to five and the HTTP client drops the Authorization header when the host changes.
- The desktop session file is written atomically with mode 0600 under the application's data directory.
- Builds are delivered with zero warnings. `scripts/check.fish` is the executable form of that rule.

## Requirements

Nix with flakes. The flake targets `x86_64-linux` and `aarch64-linux`. Everything the build needs, including the Rust toolchain, the PureScript toolchain, cargo-tauri and the WebKitGTK and GTK libraries, comes from the dev shell. Nothing is installed globally.

Toolchain as pinned by the inputs at the time of writing: purs 0.15.16 and spago 1.0.5 from purescript-overlay, registry package set 81.11.4 (Halogen 7.0.0), Rust stable from rust-overlay, Tauri 2.

macOS and Windows have not been tried. Android and iOS shells are not written; the `android` dev shell in the flake has never been entered and its SDK versions are a first guess. iOS builds will need a Mac with Xcode whatever the state of the code.

## Building and running

```
nix develop
cargo tauri icon path/to/a-square-png-at-least-1024px.png
cargo tauri dev
```

The icon step runs once. Tauri's code generator reads `src-tauri/icons/32x32.png` at compile time and fails without it; `cargo tauri icon` writes the whole set from one square PNG. Any PNG does for development.

`cargo tauri dev` runs the frontend build first (`spago bundle` into `dist/`, with `static/index.html` and `static/style.css` copied beside it) and then compiles and starts the shell. The first Rust compile takes several minutes; after that the Rust side is cached and each start re-runs only the PureScript bundle. To pick up a PureScript change, stop and start `cargo tauri dev` again.

In the app, type an instance domain and sign in. The consent page opens in the system browser; the app receives the redirect on a loopback port and switches to the home feed on its own. The session is stored at `~/.local/share/com.harryprayiv.pellucid/session.json`. Logging out deletes that file. The server-side token is not revoked yet; that is a known issue in the specification.

The dev shell exports `XDG_DATA_DIRS` with the GTK schemas, `GIO_MODULE_DIR` pointing at glib-networking so WebKitGTK can load https images, and `WEBKIT_DISABLE_DMABUF_RENDERER=1`, which avoids a blank window on some GPU drivers.

## The gate

```
fish scripts/check.fish
```

The gate runs, in order: `purs-tidy check`, `spago build --strict` (warnings are errors), `spago test`, `cargo fmt --check`, `cargo clippy -D warnings` and `cargo test` for `pixelfed-core`, and `cargo clippy -D warnings` for the shell. It stops at the first failure. Nothing is merged or delivered with a warning.

Everything up to the last step runs on a fresh clone. The last step needs the icons from `cargo tauri icon`.

## Tests

`cargo test -p pixelfed-core` covers domain parsing and origin checks, the version floor, the Link header parser, request URL building and the refusal of non-API paths and foreign cursors, error message extraction, the session store's permissions and round trip, and a real loopback OAuth round trip over a socket.

`spago test` decodes fixtures shaped like Pixelfed's `StatusTransformer`, `RelationshipService` and the context endpoint, including the lenient paths: string and null counters, a null `content_text`, the empty `liked_by` form, a share wrapper and a sparse remote account.

The bundled UI is also exercised end to end in a headless DOM against a mock of the shell's command and event contract. That harness is not part of this repository.

## Specification

`.inference/pellucid.allium` is the contract the code is held to. It records the facts about Pixelfed the client depends on, each with where it was checked in Pixelfed's source, the decisions behind the design, every rule a screen implements, the invariants above, the surfaces built and not built, and the open questions. Where the code and the specification disagree, the specification records the intended behaviour and its IMPLEMENTATION STATUS section says which parts are built.

Change the specification and the code together. A pull request that changes behaviour without touching the specification is incomplete.

## Protocol notes for contributors

Every read sends `_pe=1`. That flag selects Pixelfed's own entity, which carries `pf_type`, `content_text`, `liked_by`, `comments_disabled` and `place`. Without it the server returns the Mastodon-compatible entity and strips those keys.

The bookmark endpoints ignore `_pe` and always return the Mastodon entity. The client merges only the `bookmarked` flag from that answer. The favourite endpoints honour `_pe` and their answer replaces the local copy.

Timelines paginate by the `Link` header. The client stores the `next` URL as an opaque cursor and asks Rust to follow it after validation. The account statuses endpoint sends no `Link` header; the profile grid pages by `max_id` with the last id it holds, and an empty page ends the list. Pages are deduplicated by status id.

The context endpoint returns up to 100 descendants and no `Link` header, so comments are one request, shown flat in server order.

Login is OAuth 2 authorization code without PKCE. Each login registers a fresh client through `POST /api/v1/apps`, because the redirect URI carries an ephemeral loopback port and Pixelfed issues clients freely. The desktop shell binds the port, registers `http://127.0.0.1:PORT/callback`, opens the authorize URL with a random state, and accepts exactly one redirect carrying that state.

## Testing against an instance

pixelfed.social and any other public instance running 0.12.3 or newer work as they are. For destructive testing a throwaway instance is better. A NixOS module that stands one up with open registration, no email verification, every ActivityPub switch off and a Let's Encrypt certificate obtained by DNS-01 lives in the author's fleet configuration rather than in this repository; the specification's `LabInstance` surface describes it. The certificate matters because the client speaks https only and has no insecure mode.

## Repository layout

```
.inference/pellucid.allium     the specification
crates/pixelfed-core/          Rust: OAuth, session, transport (no UI)
src-tauri/                     Rust: Tauri shell, commands, events, config
src/                           PureScript: entities, decoders, client, screens
static/                        index.html and style.css, copied into dist/
test/                          PureScript decoder tests and JSON fixtures
scripts/check.fish             the gate
flake.nix                      dev shells
spago.yaml, Cargo.toml         package manifests
```

## Roadmap

In order: notifications and compose (which brings the first Rust-side multipart upload and post deletion), then search, discover and hashtags, then the mobile shells (deep link redirect, platform secret stores, the Android dev shell made real), then direct messages, followers and following lists, stories, settings and moderation. Infinite scroll, blurhash placeholders, hashtag and mention links and token revocation are smaller items that land alongside.

## License

AGPL-3.0-or-later. The license text belongs in `LICENSE` at the repository root.
