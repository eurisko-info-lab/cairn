#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUST_MANIFEST="$ROOT_DIR/verifier-rust/Cargo.toml"
LEAN_DIR="$ROOT_DIR/verifier-lean"

DIGEST64="deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"

pass_count=0
fail_count=0

run_rust() {
  (
    cd "$ROOT_DIR"
    cargo run --quiet --manifest-path "$RUST_MANIFEST" -- "$@" 2>&1
  )
}

run_lean() {
  (
    cd "$LEAN_DIR"
    lake exe verifier-lean "$@" 2>&1
  )
}

classify_rust() {
  local out="$1"
  if grep -q '^ok:' <<<"$out"; then
    printf 'valid'
  elif grep -q '^Error: missing closure:' <<<"$out"; then
    printf 'missing'
  elif grep -q '^Error: exhausted:' <<<"$out"; then
    printf 'exhausted'
  else
    printf 'invalid'
  fi
}

classify_lean() {
  local out="$1"
  case "$out" in
    valid:*) printf 'valid' ;;
    missing:*) printf 'missing' ;;
    exhausted:*) printf 'exhausted' ;;
    *) printf 'invalid' ;;
  esac
}

run_case() {
  local name="$1"
  local rust_cmd="$2"
  local lean_cmd="$3"

  local rust_out lean_out rust_kind lean_kind
  rust_out="$(eval "$rust_cmd" || true)"
  lean_out="$(eval "$lean_cmd" || true)"

  rust_kind="$(classify_rust "$rust_out")"
  lean_kind="$(classify_lean "$lean_out")"

  if [[ "$rust_kind" == "$lean_kind" ]]; then
    printf '[PASS] %s -> %s\n' "$name" "$rust_kind"
    pass_count=$((pass_count + 1))
  else
    printf '[FAIL] %s\n' "$name"
    printf '  rust_kind=%s\n' "$rust_kind"
    printf '  lean_kind=%s\n' "$lean_kind"
    printf '  rust_out=%s\n' "$rust_out"
    printf '  lean_out=%s\n' "$lean_out"
    fail_count=$((fail_count + 1))
  fi
}

main() {
  run_case \
    "resolve invalid digest" \
    "run_rust resolve --cas /tmp --digest abc" \
    "run_lean resolve /tmp abc"

  run_case \
    "verify-cert invalid cert digest" \
    "run_rust verify-cert --cas /tmp --cert abc --proposal $DIGEST64 --manifest $DIGEST64" \
    "run_lean verify-cert /tmp abc $DIGEST64 $DIGEST64"

  run_case \
    "replay-history missing chain" \
    "tmpdir=\$(mktemp -d); out=\$(run_rust verify-history --node-root \"\$tmpdir\" --federation-id $DIGEST64 --genesis-state $DIGEST64 || true); rm -rf \"\$tmpdir\"; printf '%s' \"\$out\"" \
    "tmpdir=\$(mktemp -d); out=\$(run_lean replay-history \"\$tmpdir\" $DIGEST64 $DIGEST64 || true); rm -rf \"\$tmpdir\"; printf '%s' \"\$out\""

  run_case \
    "replay-history empty chain" \
    "tmpdir=\$(mktemp -d); : > \"\$tmpdir/chain\"; out=\$(run_rust verify-history --node-root \"\$tmpdir\" --federation-id $DIGEST64 --genesis-state $DIGEST64 || true); rm -rf \"\$tmpdir\"; printf '%s' \"\$out\"" \
    "tmpdir=\$(mktemp -d); : > \"\$tmpdir/chain\"; out=\$(run_lean replay-history \"\$tmpdir\" $DIGEST64 $DIGEST64 || true); rm -rf \"\$tmpdir\"; printf '%s' \"\$out\""

  printf '\nSummary: %d passed, %d failed\n' "$pass_count" "$fail_count"
  if [[ "$fail_count" -ne 0 ]]; then
    exit 1
  fi
}

main "$@"
