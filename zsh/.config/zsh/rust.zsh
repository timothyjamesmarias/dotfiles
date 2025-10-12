# --- Rust dev aliases ---

alias rr="cargo run"
alias rc="cargo check"
alias rf="cargo fmt"
alias rt="cargo test"
alias rl="cargo clippy --all-targets --all-features -- -D warnings"
alias ru="cargo update"
alias rd="cargo doc --open"
alias rtf="cargo tree | fzf"
alias rdeps="cargo metadata --format-version=1 | jq '.packages[] | .name'"
alias rsmod="fd . rs | grep '\.rs$' | fzf | xargs nvim"
alias rb="cargo build"
alias rbe="cargo build --release"
alias rx="cargo expand | bat --paging=always"  # Requires `cargo-expand` and `bat`
alias rlog="RUST_LOG=debug cargo run"           # For debugging with env vars
alias rch="cargo check --all-targets --all-features"
alias rbench="cargo bench"
alias rclean="cargo clean"
