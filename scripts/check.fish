#!/usr/bin/env fish
# The Pellucid gate. Everything .inference/pellucid.allium's invariant
# WarningFreeBuild demands, in one command. Run from the repository root
# inside `nix develop`. Exits non-zero at the first failure.

function step --argument-names name
    set_color --bold
    echo "== $name"
    set_color normal
end

step "purs-tidy check"
purs-tidy check src test; or exit 1

step "spago build (warnings are errors)"
spago build --strict; or exit 1

step "spago test"
spago test; or exit 1

step "cargo fmt"
cargo fmt --all --check; or exit 1

step "cargo clippy pixelfed-core"
cargo clippy -p pixelfed-core --all-targets -- -D warnings; or exit 1

step "cargo test pixelfed-core"
cargo test -p pixelfed-core; or exit 1

step "cargo clippy pellucid (needs src-tauri/icons from cargo tauri icon)"
cargo clippy -p pellucid -- -D warnings; or exit 1

echo "all checks passed"
