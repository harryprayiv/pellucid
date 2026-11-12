# Pellucid

Pellucid is a Pixelfed client. The user interface is PureScript (Halogen) running in a Tauri 2 webview; the network and secret handling is Rust. It speaks the API that pixelfed-rn speaks to stock Pixelfed servers today, so it works against pixelfed.social and any other instance running Pixelfed 0.12.3 or newer. No Pellucid server exists yet; a server is a later project with its own specification.

The name means transparently clear. The name claims nothing by itself. What makes the claim is code you can read, a gate that refuses warnings, a specification the code is held to, and, later, reproducible builds.

Pellucid is not affiliated with the Pixelfed project.

## Status

This is early work and is not released. The desktop Linux build is the only one that has been exercised. What is built:

- Login to any Pixelfed instance through OAuth 2 with PKCE in the system browser, with the session persisted locally.
- Home and public timelines, likes, shares, bookmarks, sensitive media covers, albums and video, with hashtags and mentions in captions as links.
- Post detail with comments, replying, deleting your own comments, likes on comments, editing your own post, deleting it after a confirmation, and reporting someone else's.
- Explore: search over people and hashtags, trending hashtags, popular accounts and trending posts by day, month and year.
- Hashtag feeds with follow and unfollow, and the list of hashtags you follow.
- Profiles with follow, unfollow and follow requests, mute, block after a confirmation, report, a three-wide grid of posts, and the lists of followers and following.
- Who liked and who shared a post.
- Your bookmarks and your likes.
- Notifications: likes, comments, shares, follows, mentions, tags and direct messages, each opening the post, the conversation or the person concerned.
- Direct messages: your conversations, a thread with earlier messages on demand, sending text, deleting your own messages.
- Compose: pick photos or videos, each uploaded as it is picked through the Rust side, alt text per file, caption, visibility, sensitive flag with content warning, comments off, with the instance's own limits applied before anything is sent.
- Settings: the profile editor (display name, bio, website, private account), follow requests with accept and reject, muted and blocked accounts, and logging out after a confirmation.
- Every list loads further as you scroll, and going back returns to the screen you left, where you left it, without fetching it again.

What is not built: stories, collections, the admin screens, push notifications, avatar upload, a recipient picker for a new conversation (a conversation starts from a profile), sending media in a direct message, blocked domains, notification type filters, post edit history, cropping and location tagging in the composer, blurhash placeholders, the Android and iOS shells, and a Nix package for the bundle and the binary. Token revocation on logout is not possible against stock Pixelfed; see the protocol notes. The specification lists each of these as not built or as a known issue.

Nothing here has spoken to a running server yet. Every screen has run against a mock of the shell, and every fact about the server comes from reading Pixelfed's source at 0.15.2 and at 0.12.5. The first real login is the next thing to do.

## Why this shape

Pixelfed's `config/cors.php` allows no origins by default. Only `/api/v1/apps` and `/api/v2/instance` set `Access-Control-Allow-Origin`. Timelines, `/oauth/token` and `verify_credentials` send no CORS header at all. A web page in a browser cannot log in to a stock instance, so every request has to come from a native HTTP client. Pellucid's requests come from Rust.

Since Rust has to make the requests anyway, the access token has no reason to exist in the webview. The webview receives the instance domain and the account id and handle, never the token, the client secret or the refresh token. The Rust side refuses to attach the token to any path outside `/api/` and to any pagination cursor that does not point at the logged-in instance over https.

## Architecture

```
PureScript (Halogen)          Rust (crates/pixelfed-core)   Tauri shell (src-tauri)
Component.Root                core: session, http client    seven commands, two events
  App.Stack, App.Screen       oauth: apps, PKCE, authorize  opens the system browser,
fifteen screen components     URL, loopback listener,       emits session-changed
  -invoke->                   token exchange                and login-failed,
App.Paged                     api: request_url, follow,     unpacks the raw upload body,
View.StatusCard, View.Tile    send, multipart upload        FileStore with mode 0600
Util.Caption, Util.Scroll     link: Link header parser
Pixelfed.Client   <-event-    domain: validated https host
Pixelfed.Decode               store: SecretStore trait
App.Backend (the seam)        error: closed AppError sum
```

`crates/pixelfed-core` has no UI and no Tauri dependency. It owns the OAuth dance, the token, the authenticated transport, the multipart upload and the validation of every URL the token travels to. It has unit tests, including a loopback OAuth round trip, the RFC 7636 PKCE test vector and the upload size bounds.

`src-tauri` is an adapter. Each command calls one core method; the only extra behaviour is opening the browser, emitting the two events, and unpacking the raw body and three headers of the upload command. It holds no protocol logic.

`src` is the PureScript application. `App.Backend` is a record of functions, one per shell command plus two subscriptions, and `App.Backend.Tauri` implements it over `window.__TAURI__`. Screens import the seam, never the Tauri module. Entities and decoders are written against Pixelfed's own transformers rather than the Mastodon specification, because Pixelfed omits some fields, nulls others and adds its own. Only what identifies a thing can fail its decode: a post needs its id and its account, an account its id and username, an attachment its id. Every other field is read by a reader that accepts each encoding the server uses (a flag as `true`, `1` or `"1"`, a number as a number or a string of digits) and falls back to a default for anything else, because several of Pixelfed's transformers pass a database column through uncast.

Three pure modules carry the rules every screen shares. `Pixelfed.Client` exposes each list as a `Pager`, a function that fetches one chunk and the pager for the chunk after it, which hides the four ways Pixelfed paginates. `App.Paged` is the state machine every paginated screen runs: no chunk is requested twice, no item appears twice, a chunk with nothing new ends the list, and an answer nobody is waiting for is dropped. `App.Stack` decides whether a route change is a step back, a switch of tab or a step forward, so the screen underneath stays mounted with its state and scroll position. Because screens stay mounted, a change made on one (a like, an edit, a deletion) is raised to `Component.Root` and sent to every other screen that shows the same post; `App.Screen` holds that protocol.

There is no npm, no package.json and no node_modules. The frontend is spago packages plus two hand-written foreign modules: `src/App/Backend/Tauri.js`, which exists because Tauri's `withGlobalTauri` setting exposes `invoke` and `listen` on the window object, and `src/Util/File.js`, which calls `file.arrayBuffer()` on a picked file. esbuild comes from nixpkgs and is driven by `spago bundle`. Node comes from nixpkgs as well, only as the runtime `spago test` executes the compiled tests with.

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

Nix with flakes. The flake targets `x86_64-linux` and `aarch64-linux`. Everything the build needs, including the Rust toolchain, the PureScript toolchain, cargo-tauri, the WebKitGTK and GTK libraries and the GStreamer plugins the webview plays video with, comes from the dev shell. Nothing is installed globally.

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

The dev shell exports `XDG_DATA_DIRS` with the GTK schemas, `GIO_MODULE_DIR` pointing at glib-networking so WebKitGTK can load https images, and `WEBKIT_DISABLE_DMABUF_RENDERER=1`, which avoids a blank window on some GPU drivers. It also sets `GST_PLUGIN_SYSTEM_PATH_1_0` to the flake's own GStreamer, gst-plugins-base, gst-plugins-good, gst-plugins-bad and gst-libav. WebKitGTK plays video through GStreamer and does not bring its plugins with it, so without that path the webview can play only what the host session's plugins allow and logs a missing WebVTT encoder for every video in a feed.

## The gate

```
fish scripts/check.fish
```

The gate runs, in order: `purs-tidy check`, `spago build --strict` (warnings are errors), `spago test`, `cargo fmt --check`, `cargo clippy -D warnings` and `cargo test` for `pixelfed-core`, the frontend bundle into `dist/` (Tauri's `generate_context!` macro panics at compile time when `frontendDist` does not exist), and `cargo clippy -D warnings` for the shell. It stops at the first failure. Nothing is merged or delivered with a warning.

Everything up to the last step runs on a fresh clone. The last step needs the icons from `cargo tauri icon`.

## Tests

`cargo test -p pixelfed-core` covers domain parsing and origin checks, the server name and version floor a nodeinfo document must meet, the Link header parser, request URL building and the refusal of non-API paths and foreign cursors, error message extraction, the session store's permissions and round trip, PKCE against the RFC 7636 appendix B vector and the freshness of generated verifiers, the upload bounds, and a real loopback OAuth round trip over a socket.

`spago test` runs 184 checks. It decodes fixtures shaped like Pixelfed's `StatusTransformer`, `RelationshipService`, `NotificationTransformer`, `MediaTransformer` as `POST /api/v2/media` returns it, the compose settings, context, search, hashtag, trending and direct message endpoints, including the lenient paths: string and null counters, a null `content_text`, the empty `liked_by` form, a share wrapper, a sparse remote account, a notification with an unknown verb and an unreadable status, a comma-joined media type list, an upload with a null `url`, lists that carry `false` and `[]` in place of an entry, and direct messages as both 0.12 and 0.15 send them. The fixtures carry uncast fields the way the server sends them (`is_nsfw` as `0`, `1` and null, a tagged post's `status_id` as a bare number), and a set of loose payloads checks that a post reduced to its id and account still decodes while one that cannot be identified fails with an error naming the field. It also tests the pure machines: the paged list, the navigation stack, the round trip of every route, caption links, and the merge rules for like, bookmark and share.

The bundled UI is also exercised end to end in a headless DOM against a mock of the shell's command and event contract: 122 steps through every screen, including the raw-body upload. That harness is JavaScript on jsdom, is not part of this repository, and the gate does not run it; the specification records this as a known issue.

## Specification

`.inference/pellucid.allium` is the contract the client code is held to. It records the facts about Pixelfed the client depends on, each with where it was checked in Pixelfed's source, the decisions behind the design, every rule a screen implements, the invariants above, the surfaces, the known issues and the open questions. Where the code and the specification disagree, the specification records the intended behaviour and its IMPLEMENTATION STATUS section says which parts are built.

The server Pellucid will need is specified in `pellucid-server.allium`, which moved to the `pellucid-server` repository with the first server code and lives in that repository's `.inference` directory. It covers accounts, OAuth with public clients and token revocation, media, posts, follows, blocks, direct messages, reports and moderation with notices and appeals, federation deliveries, and export, and records which of those are built. The client accepts that server's nodeinfo name, `pellucid`, beside `pixelfed`, each with its own version floor. It has not signed in to one yet, because none is deployed and the client reaches only https servers with a certificate from a public authority (known issue `client_refuses_this_server`).

Both specifications pass `allium check` (allium-cli 3.6.1) with no errors and no warnings. Every lifecycle in them is a field named `status` with a transition graph, because that is the form the checker's state analysis covers: it rejects a rule that makes a transition the graph does not declare, a status no rule assigns and a status no rule leaves. The checker does not verify that the code matches the specification; that is done by reading, rule by rule.

Change the specification and the code together. A pull request that changes behaviour without touching the specification is incomplete.

## Protocol notes for contributors

Every read sends `_pe=1`. That flag selects Pixelfed's own entity, which carries `pf_type`, `content_text`, `liked_by`, `comments_disabled` and `place`. Without it the server returns the Mastodon-compatible entity and strips those keys.

The bookmark endpoints ignore `_pe` and always return the Mastodon entity. The client merges only the `bookmarked` flag from that answer. The favourite endpoints honour `_pe` and their answer replaces the local copy.

Pixelfed paginates in four ways. Timelines, notifications, bookmarks, favourites, mutes, blocks, followed hashtags and conversations send a `Link` header whose `rel="next"` leads further into the list. Followers, following, who liked and who shared are cursor-paginated and file the same direction under `rel="prev"`; who liked and who shared send the header to the post's author only, so everyone else sees one page. Account statuses and the direct message thread send no `Link` header and page by `max_id`. The hashtag timeline pages by `max_id` on 0.12 and by an opaque cursor in a `Link` header on 0.15, where a bare `max_id` answers an empty list; the client follows the header when there is one and falls back to `max_id` when there is not. In every case the client stores a cursor as an opaque value and asks Rust to follow it after validation, and lists are deduplicated by id.

The share endpoints accept public posts only and answer with the Mastodon entity, in which the share count is the count before the server's queue has counted the share; the client takes the flag and moves the count by one itself. `PUT /api/v1/statuses/{id}` deletes from storage every attachment its `media_ids` leaves out, so the post editor always sends the full list. Search never returns posts. A report's note is limited to 255 bytes, a direct message to 500 characters on 0.12 and 2000 on 0.15, and 0.12 omits `created_at` from the answer to a sent message.

The context endpoint returns up to 100 descendants and no `Link` header, so comments are one request, shown flat in server order.

Login is OAuth 2 authorization code with PKCE (S256). Pixelfed runs Laravel Passport on league/oauth2-server 9, which stores the challenge with the code and refuses the token request without the matching verifier. `POST /api/v1/apps` only issues confidential clients, so the client secret is still sent and still stored; PKCE protects the code on its way back through the loopback redirect, it does not remove the secret. Each login registers a fresh client, because the redirect URI carries an ephemeral loopback port and Pixelfed issues clients freely. The desktop shell binds the port, registers `http://127.0.0.1:PORT/callback`, opens the authorize URL with a random state, and accepts exactly one redirect carrying that state.

Stock Pixelfed has no route through which a client can revoke its own token: Passport ships no RFC 7009 `/oauth/revoke`, `DELETE /oauth/tokens/{id}` needs the browser session, and `POST /api/v1.1/accounts/apps-and-applications/{id}/revoke` answers 422 for the token making the request. Logout is therefore local.

Notifications paginate by the `Link` header like timelines; the server caps `limit` at 40. The `status` in a notification is the liked or shared post for a like or a share, the comment itself for a comment, and the mentioning post for a mention; a follow has none. The client opens a comment's parent so the comment is read in place, and a tag opens `tagged.status_id`.

Uploads are multipart `POST /api/v2/media` with `file`, built in Rust from bytes the webview sends as a raw IPC body. The server validates the MIME type against its `media_types` and the size against `max_photo_size` in kilobytes; the client applies both first using `GET /api/v1.1/compose/settings`, with Pixelfed's stock defaults when that call fails. The answer has `url: null` and the stored file in `preview_url`. Alt text is set afterwards with `PUT /api/v1/media/{id}`. `POST /api/v1/statuses` refuses a status with neither `media_ids` nor `in_reply_to_id`, so Post stays disabled until every upload is done; `spoiler_text` is limited to 140 characters and is sent only when the post is marked sensitive.

## Testing against an instance

pixelfed.social and any other public instance running 0.12.3 or newer work as they are. For destructive testing a throwaway instance is better. A NixOS module that stands one up with open registration, no email verification, every ActivityPub switch off and a Let's Encrypt certificate obtained by DNS-01 lives in the author's fleet configuration rather than in this repository; the specification's `LabInstance` surface describes it. The certificate matters because the client speaks https only and has no insecure mode.

## Repository layout

```
.inference/pellucid.allium          the client specification
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

In order: the lab instance deployed and the first real login, feed, upload, post, search and direct message against it (everything so far has run against a mock of the shell and the reading of the server's source), then the mobile shells (deep link redirect, platform secret stores, the Android dev shell made real), then stories, collections, avatar upload and the admin screens, then the server. Cancelling an abandoned login in Rust, a recipient picker for new conversations, blurhash placeholders and notification filters are smaller items that land alongside. Token revocation waits for a server that offers it.

## License

AGPL-3.0-or-later. The license text belongs in `LICENSE` at the repository root.

## 🤝 Contributing

Contributions are welcome. Please feel free to submit a Pull Request.

## Support


If you like my style, Cardano ADA donations fund continued work. 

[![Donate ADA](https://img.shields.io/badge/Donate-ADA-0033AD?style=for-the-badge&logo=cardano&logoColor=white)](https://cardanoscan.io/address/addr1qxankwszy6zaclef8vq5fjn3008689yswvangmz604npjhms7402kf0h964awtj8d6xz7lzlvnrq6hz6wu6k845pzkvspum4l7)