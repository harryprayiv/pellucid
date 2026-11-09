# Pellucid

Pellucid is a Pixelfed client. The user interface is PureScript (Halogen) running in a Tauri 2 webview; the network and secret handling is Rust. It speaks the API that pixelfed-rn speaks to stock Pixelfed servers today, so it works against pixelfed.social and any other instance running Pixelfed 0.12.3 or newer. No Pellucid server exists yet; a server is a later project with its own specification.

The name means transparently clear. The name claims nothing by itself. What makes the claim is code you can read, a gate that refuses warnings, a specification the code is held to, and, later, reproducible builds.

Pellucid is not affiliated with the Pixelfed project.

## Status

This is early work and is not released. The desktop Linux build is the only one that has been exercised. What is built:

- Login to any Pixelfed instance through OAuth 2 with PKCE in the system browser, with the session persisted locally.
- Home and public timelines with cursor pagination, likes, bookmarks, sensitive media covers, albums and video.
- Post detail with comments, replying, deleting your own comments, likes on comments, and deleting your own post after a confirmation.
- Profiles with follow, unfollow and follow requests, a three-wide grid of posts with paging, and the Me tab.
- Notifications: likes, comments, shares, follows, mentions and tags, newest first with paging, each opening the post or the person concerned.
- Compose: pick photos or videos, each uploaded as it is picked through the Rust side, alt text per file, caption, visibility, sensitive flag with content warning, comments off, with the instance's own limits applied before anything is sent.

What is not built: search and discover, hashtags, direct messages, followers and following lists, stories, settings, moderation, editing a published post, notification filters, infinite scroll (lists end in a Load more button), blurhash placeholders, hashtag and mention links, the Android and iOS shells, and a Nix package for the bundle and the binary. Token revocation on logout is not possible against stock Pixelfed; see the protocol notes. The specification lists each of these as a surface or a known issue.

## Why this shape

Pixelfed's `config/cors.php` allows no origins by default. Only `/api/v1/apps` and `/api/v2/instance` set `Access-Control-Allow-Origin`. Timelines, `/oauth/token` and `verify_credentials` send no CORS header at all. A web page in a browser cannot log in to a stock instance, so every request has to come from a native HTTP client. Pellucid's requests come from Rust.

Since Rust has to make the requests anyway, the access token has no reason to exist in the webview. The webview receives the instance domain and the account id and handle, never the token, the client secret or the refresh token. The Rust side refuses to attach the token to any path outside `/api/` and to any pagination cursor that does not point at the logged-in instance over https.

## Architecture

```
PureScript (Halogen)          Rust (crates/pixelfed-core)   Tauri shell (src-tauri)
Component.Root                core: session, http client    seven commands, two events
Component.Login   -invoke->   oauth: apps, PKCE, authorize  opens the system browser,
Component.Feed                URL, loopback listener,       emits session-changed
Component.StatusDetail        token exchange                and login-failed,
Component.Profile             api: request_url, follow,     unpacks the raw upload body,
Component.Notifications       send, multipart upload        FileStore with mode 0600
Component.Compose <-event-    link: Link header parser
View.StatusCard               domain: validated https host
Pixelfed.Client               store: SecretStore trait
Pixelfed.Decode               error: closed AppError sum
App.Backend (the seam)
```

`crates/pixelfed-core` has no UI and no Tauri dependency. It owns the OAuth dance, the token, the authenticated transport, the multipart upload and the validation of every URL the token travels to. It has unit tests, including a loopback OAuth round trip, the RFC 7636 PKCE test vector and the upload size bounds.

`src-tauri` is an adapter. Each command calls one core method; the only extra behaviour is opening the browser, emitting the two events, and unpacking the raw body and three headers of the upload command. It holds no protocol logic.

`src` is the PureScript application. `App.Backend` is a record of functions, one per shell command plus two subscriptions, and `App.Backend.Tauri` implements it over `window.__TAURI__`. Screens import the seam, never the Tauri module. Entities and decoders are written against Pixelfed's own transformers rather than the Mastodon specification, because Pixelfed omits some fields, nulls others and adds its own.

There is no npm, no package.json and no node_modules. The frontend is spago packages plus two hand-written foreign modules: `src/App/Backend/Tauri.js`, which exists because Tauri's `withGlobalTauri` setting exposes `invoke` and `listen` on the window object, and `src/Util/File.js`, which calls `file.arrayBuffer()` on a picked file. esbuild comes from nixpkgs and is driven by `spago bundle`.

## Security properties

These are invariants in `.inference/pellucid.allium` and the code is written to them.

- The token never crosses the bridge. No command result and no event payload contains `access_token`, `client_secret` or `refresh_token`. The PKCE verifier never leaves Rust either.
- The token goes only to `/api/` paths on the logged-in origin over https. A pagination cursor is dereferenced only after Rust parses it and checks the scheme, the host, the port and the path prefix. Uploads go to the fixed path `/api/v2/media` on that origin.
- Bytes leave the device only for a file the person picked. A picked file is read once in the webview, crosses the IPC once as a raw body, is never written to disk by the shell, and is refused by Rust before any network activity when empty or over 512 MiB.
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

In the app, type an instance domain and sign in. The consent page opens in the system browser; the app receives the redirect on a loopback port and switches to the home feed on its own. The session is stored at `~/.local/share/com.harryprayiv.pellucid/session.json`. Logging out deletes that file. The server-side token stays valid until the instance expires it, because stock Pixelfed gives a client no way to revoke the token it is using; revoke it from the instance's own settings page if that matters.

The dev shell exports `XDG_DATA_DIRS` with the GTK schemas, `GIO_MODULE_DIR` pointing at glib-networking so WebKitGTK can load https images, and `WEBKIT_DISABLE_DMABUF_RENDERER=1`, which avoids a blank window on some GPU drivers.

## The gate

```
fish scripts/check.fish
```

The gate runs, in order: `purs-tidy check`, `spago build --strict` (warnings are errors), `spago test`, `cargo fmt --check`, `cargo clippy -D warnings` and `cargo test` for `pixelfed-core`, and `cargo clippy -D warnings` for the shell. It stops at the first failure. Nothing is merged or delivered with a warning.

Everything up to the last step runs on a fresh clone. The last step needs the icons from `cargo tauri icon`.

## Tests

`cargo test -p pixelfed-core` covers domain parsing and origin checks, the version floor, the Link header parser, request URL building and the refusal of non-API paths and foreign cursors, error message extraction, the session store's permissions and round trip, PKCE against the RFC 7636 appendix B vector and the freshness of generated verifiers, the upload bounds, and a real loopback OAuth round trip over a socket.

`spago test` decodes fixtures shaped like Pixelfed's `StatusTransformer`, `RelationshipService`, `NotificationTransformer`, `MediaTransformer` as `POST /api/v2/media` returns it, the compose settings endpoint and the context endpoint, including the lenient paths: string and null counters, a null `content_text`, the empty `liked_by` form, a share wrapper, a sparse remote account, a notification with an unknown verb and an unreadable status, a comma-joined media type list and an upload with a null `url`.

The bundled UI is also exercised end to end in a headless DOM against a mock of the shell's command and event contract, including the raw-body upload. That harness is not part of this repository and the gate does not run it; the specification records this as a known issue.

## Specification

`.inference/pellucid.allium` is the contract the code is held to. It records the facts about Pixelfed the client depends on, each with where it was checked in Pixelfed's source, the decisions behind the design, every rule a screen implements, the invariants above, the surfaces built and not built, and the open questions. Where the code and the specification disagree, the specification records the intended behaviour and its IMPLEMENTATION STATUS section says which parts are built.

Change the specification and the code together. A pull request that changes behaviour without touching the specification is incomplete.

## Protocol notes for contributors

Every read sends `_pe=1`. That flag selects Pixelfed's own entity, which carries `pf_type`, `content_text`, `liked_by`, `comments_disabled` and `place`. Without it the server returns the Mastodon-compatible entity and strips those keys.

The bookmark endpoints ignore `_pe` and always return the Mastodon entity. The client merges only the `bookmarked` flag from that answer. The favourite endpoints honour `_pe` and their answer replaces the local copy.

Timelines paginate by the `Link` header. The client stores the `next` URL as an opaque cursor and asks Rust to follow it after validation. The account statuses endpoint sends no `Link` header; the profile grid pages by `max_id` with the last id it holds, and an empty page ends the list. Pages are deduplicated by status id.

The context endpoint returns up to 100 descendants and no `Link` header, so comments are one request, shown flat in server order.

Login is OAuth 2 authorization code with PKCE (S256). Pixelfed runs Laravel Passport on league/oauth2-server 9, which stores the challenge with the code and refuses the token request without the matching verifier. `POST /api/v1/apps` only issues confidential clients, so the client secret is still sent and still stored; PKCE protects the code on its way back through the loopback redirect, it does not remove the secret. Each login registers a fresh client, because the redirect URI carries an ephemeral loopback port and Pixelfed issues clients freely. The desktop shell binds the port, registers `http://127.0.0.1:PORT/callback`, opens the authorize URL with a random state, and accepts exactly one redirect carrying that state.

Stock Pixelfed has no route through which a client can revoke its own token: Passport ships no RFC 7009 `/oauth/revoke`, `DELETE /oauth/tokens/{id}` needs the browser session, and `POST /api/v1.1/accounts/apps-and-applications/{id}/revoke` answers 422 for the token making the request. Logout is therefore local.

Notifications paginate by the `Link` header like timelines; the server caps `limit` at 40. The `status` in a notification is the liked or shared post for a like or a share, the comment itself for a comment, and the mentioning post for a mention; a follow has none. The client opens a comment's parent so the comment is read in place, and a tag opens `tagged.status_id`.

Uploads are multipart `POST /api/v2/media` with `file`, built in Rust from bytes the webview sends as a raw IPC body. The server validates the MIME type against its `media_types` and the size against `max_photo_size` in kilobytes; the client applies both first using `GET /api/v1.1/compose/settings`, with Pixelfed's stock defaults when that call fails. The answer has `url: null` and the stored file in `preview_url`. Alt text is set afterwards with `PUT /api/v1/media/{id}`. `POST /api/v1/statuses` refuses a status with neither `media_ids` nor `in_reply_to_id`, so Post stays disabled until every upload is done; `spoiler_text` is limited to 140 characters and is sent only when the post is marked sensitive.

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

In order: the lab instance deployed and the first real login, feed, upload and post against it (everything so far has run against a mock of the shell and the reading of the server's source), then search, discover and hashtags, then the mobile shells (deep link redirect, platform secret stores, the Android dev shell made real), then direct messages, followers and following lists, stories, settings and moderation. Infinite scroll, blurhash placeholders, hashtag and mention links, notification filters and removing a deleted post from the feed behind it are smaller items that land alongside. Token revocation waits for a server that offers it.

## License

AGPL-3.0-or-later. The license text belongs in `LICENSE` at the repository root.
