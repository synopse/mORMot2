#!/usr/bin/env bash
# Bootstrap a Free Pascal + mORMot 2 toolchain for ephemeral Linux runtimes.
#
# Intended location in your repository:
#   tools/bootstrap-fpc.sh
#
# What it does:
#   1. Installs the Debian/Ubuntu build prerequisites (including a bootstrap FPC).
#   2. Builds FPC fixes_3_2 (currently reports 3.2.3) into an isolated prefix.
#   3. Finds an existing mORMot 2 checkout, or clones one into the user cache.
#   4. Downloads mORMot's POSIX static archive and verifies its SHA-256 checksum.
#   5. Generates tools/fpc, a wrapper with the right FPC config, mORMot unit paths,
#      and target-specific static-library path.
#   6. Smoke-compiles a tiny mORMot program.
#
# Useful overrides:
#   FPC_REF=fixes_3_2                 FPC branch or tag to build
#   FPC_PREFIX=/some/path             FPC installation prefix
#   FPC_CACHE_DIR=/some/path          download/source cache
#   FPC_REFRESH=1                     refresh/rebuild even if already installed
#   FPC_JOBS=4                        parallel build jobs
#   MORMOT2_DIR=/path/to/mORMot2      use this mORMot checkout
#   MORMOT2_REF=master                branch/tag used only when cloning mORMot
#   MORMOT2_STATIC_URL=https://...    matching mormot2static.tgz archive
#   MORMOT2_SKIP_STATIC=1             don't download static object files
#   MORMOT2_SKIP_SMOKE_TEST=1         don't run the final mORMot compile test
#
# This script is intentionally idempotent. In a fresh ChatGPT runtime it will do
# the full bootstrap; in a persistent environment it reuses the installed/cache
# artifacts unless FPC_REFRESH=1 is set.

set -Eeuo pipefail

log()  { printf '\n[bootstrap-fpc] %s\n' "$*" >&2; }
warn() { printf '\n[bootstrap-fpc] WARNING: %s\n' "$*" >&2; }
die()  { printf '\n[bootstrap-fpc] ERROR: %s\n' "$*" >&2; exit 1; }

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
if [[ "$(basename "$SCRIPT_DIR")" == "tools" ]]; then
  REPO_ROOT="$(cd -- "$SCRIPT_DIR/.." && pwd)"
else
  REPO_ROOT="$(pwd)"
fi

FPC_REF="${FPC_REF:-fixes_3_2}"
FPC_EXPECTED_VERSION="${FPC_EXPECTED_VERSION:-3.2.3}"
FPC_GIT_URL="${FPC_GIT_URL:-https://gitlab.com/freepascal.org/fpc/source.git}"
MORMOT2_GIT_URL="${MORMOT2_GIT_URL:-https://github.com/synopse/mORMot2.git}"
MORMOT2_REF="${MORMOT2_REF:-master}"
MORMOT2_STATIC_URL="${MORMOT2_STATIC_URL:-https://synopse.info/files/mormot2static.tgz}"

CACHE_BASE="${FPC_CACHE_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/chatgpt-fpc-mormot2}"
FPC_SRC_DIR="${FPC_SRC_DIR:-$CACHE_BASE/fpc-source}"
MORMOT2_CACHE_DIR="${MORMOT2_CACHE_DIR:-$CACHE_BASE/mormot2}"
DOWNLOAD_DIR="$CACHE_BASE/downloads"

if [[ "$(id -u)" -eq 0 ]]; then
  FPC_PREFIX="${FPC_PREFIX:-/opt/fpc-fixes-3.2}"
  LINK_DIR="${FPC_LINK_DIR:-/usr/local/bin}"
else
  FPC_PREFIX="${FPC_PREFIX:-$HOME/.local/fpc-fixes-3.2}"
  LINK_DIR="${FPC_LINK_DIR:-$HOME/.local/bin}"
fi

FPC_REFRESH="${FPC_REFRESH:-0}"
FPC_JOBS="${FPC_JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || printf '2')}"
MORMOT2_SKIP_STATIC="${MORMOT2_SKIP_STATIC:-0}"
MORMOT2_SKIP_SMOKE_TEST="${MORMOT2_SKIP_SMOKE_TEST:-0}"

mkdir -p "$CACHE_BASE" "$DOWNLOAD_DIR"

have() { command -v "$1" >/dev/null 2>&1; }

run_as_root() {
  if [[ "$(id -u)" -eq 0 ]]; then
    "$@"
  elif have sudo; then
    sudo "$@"
  else
    die "Root privileges are required for package installation, and sudo is unavailable."
  fi
}

install_prerequisites() {
  local missing=0
  for cmd in git make gcc ld curl tar sha256sum; do
    if ! have "$cmd"; then
      missing=1
      break
    fi
  done
  if have apt-get; then
    # Keep a distro bootstrap compiler available even if our custom FPC is
    # already linked as `fpc`; fixes_3_2 is normally bootstrapped by 3.2.2.
    [[ -x /usr/bin/fpc ]] || missing=1
  elif ! have fpc; then
    missing=1
  fi

  if [[ "$missing" -eq 0 ]]; then
    return
  fi

  if have apt-get; then
    log "Installing bootstrap compiler and build prerequisites"
    run_as_root apt-get -o Acquire::Retries=3 -o Acquire::http::Timeout=20 update
    run_as_root env DEBIAN_FRONTEND=noninteractive apt-get \
      -o Acquire::Retries=3 -o Acquire::http::Timeout=20 install -y \
      ca-certificates curl git make gcc binutils tar \
      fp-compiler
  else
    die "This bootstrap currently supports apt-based Linux runtimes. Install git, make, gcc, binutils, curl, tar, sha256sum and FPC 3.2.2 manually, then rerun it."
  fi
}

installed_fpc_ok() {
  local fpc_bin="$FPC_PREFIX/bin/fpc"
  [[ -x "$fpc_bin" ]] || return 1
  [[ "$("$fpc_bin" -iV 2>/dev/null || true)" == "$FPC_EXPECTED_VERSION" ]] || return 1
  [[ -f "$FPC_PREFIX/.source-ref" ]] || return 1
  [[ "$(cat "$FPC_PREFIX/.source-ref")" == "$FPC_REF" ]]
}

checkout_ref() {
  local url="$1"
  local ref="$2"
  local dir="$3"

  if [[ ! -d "$dir/.git" ]]; then
    rm -rf "$dir"
    log "Cloning $url ($ref)"
    git clone --depth=1 --branch "$ref" "$url" "$dir"
    return
  fi

  if [[ "$FPC_REFRESH" == "1" ]]; then
    log "Refreshing $dir to $ref"
    git -C "$dir" fetch --depth=1 origin "$ref"
    git -C "$dir" reset --hard FETCH_HEAD
  fi
}

build_fpc() {
  if installed_fpc_ok && [[ "$FPC_REFRESH" != "1" ]]; then
    log "Reusing FPC $FPC_EXPECTED_VERSION from $FPC_PREFIX"
    return
  fi

  install_prerequisites
  checkout_ref "$FPC_GIT_URL" "$FPC_REF" "$FPC_SRC_DIR"

  local bootstrap_fpc=""
  if [[ -x /usr/bin/fpc ]]; then
    bootstrap_fpc=/usr/bin/fpc
  elif have fpc; then
    bootstrap_fpc="$(command -v fpc)"
  fi
  [[ -n "$bootstrap_fpc" ]] || die "No bootstrap FPC compiler was found after prerequisite installation."

  log "Building FPC $FPC_REF with $bootstrap_fpc using $FPC_JOBS job(s)"
  (
    cd "$FPC_SRC_DIR"
    make distclean FPC="$bootstrap_fpc" >/dev/null 2>&1 || true
    make -j"$FPC_JOBS" all FPC="$bootstrap_fpc"
  )

  log "Installing FPC into $FPC_PREFIX"
  rm -rf "$FPC_PREFIX"
  mkdir -p "$FPC_PREFIX"
  (
    cd "$FPC_SRC_DIR"
    make install FPC="$bootstrap_fpc" INSTALL_PREFIX="$FPC_PREFIX"
  )

  local fpc_bin="$FPC_PREFIX/bin/fpc"
  [[ -x "$fpc_bin" ]] || die "FPC installation did not create $fpc_bin"

  local version
  version="$("$fpc_bin" -iV 2>/dev/null || true)"
  [[ "$version" == "$FPC_EXPECTED_VERSION" ]] || \
    die "Expected FPC $FPC_EXPECTED_VERSION from $FPC_REF, got '$version'."

  # Generate a private configuration next to the actual compiler tree. This
  # prevents a distro /etc/fpc.cfg (usually for 3.2.2) from leaking into the
  # fixes_3_2 compiler invocation.
  local libdir="$FPC_PREFIX/lib/fpc/$version"
  local cfgdir="$FPC_PREFIX/lib/fpc/etc"
  local samplecfg="$FPC_SRC_DIR/compiler/utils/samplecfg"
  mkdir -p "$cfgdir"
  if [[ -x "$samplecfg" ]]; then
    "$samplecfg" "$libdir" "$cfgdir" >/dev/null
  else
    warn "samplecfg was not found; tools/fpc will still force the private compiler paths."
  fi

  local revision
  revision="$(git -C "$FPC_SRC_DIR" rev-parse HEAD 2>/dev/null || true)"
  printf '%s\n' "$revision" > "$FPC_PREFIX/.source-revision"
  printf '%s\n' "$FPC_REF" > "$FPC_PREFIX/.source-ref"
}

link_fpc_commands() {
  local version="$1"
  mkdir -p "$LINK_DIR"

  ln -sfn "$FPC_PREFIX/bin/fpc" "$LINK_DIR/fpc"

  local ppc
  for ppc in "$FPC_PREFIX/lib/fpc/$version"/ppc*; do
    [[ -x "$ppc" ]] || continue
    ln -sfn "$ppc" "$LINK_DIR/$(basename "$ppc")"
  done

  if [[ ":$PATH:" != *":$LINK_DIR:"* ]]; then
    warn "$LINK_DIR is not currently on PATH. Use $SCRIPT_DIR/fpc, or add $LINK_DIR to PATH."
  fi
}

is_mormot2_dir() {
  local dir="$1"
  [[ -f "$dir/src/mormot.defines.inc" && -f "$dir/src/core/mormot.core.base.pas" ]]
}

find_or_clone_mormot2() {
  if [[ -n "${MORMOT2_DIR:-}" ]]; then
    is_mormot2_dir "$MORMOT2_DIR" || die "MORMOT2_DIR='$MORMOT2_DIR' is not a mORMot 2 checkout."
    printf '%s\n' "$(cd -- "$MORMOT2_DIR" && pwd)"
    return
  fi

  local candidate
  for candidate in \
    "$REPO_ROOT" \
    "$REPO_ROOT/mORMot2" \
    "$REPO_ROOT/mormot2" \
    "$REPO_ROOT/vendor/mORMot2" \
    "$REPO_ROOT/vendor/mormot2" \
    "$REPO_ROOT/lib/mORMot2" \
    "$REPO_ROOT/lib/mormot2" \
    "$REPO_ROOT/thirdparty/mormot2" \
    "$REPO_ROOT/external/mormot2"; do
    if is_mormot2_dir "$candidate"; then
      printf '%s\n' "$(cd -- "$candidate" && pwd)"
      return
    fi
  done

  checkout_ref "$MORMOT2_GIT_URL" "$MORMOT2_REF" "$MORMOT2_CACHE_DIR"
  is_mormot2_dir "$MORMOT2_CACHE_DIR" || die "Cloned repository does not look like mORMot 2."
  printf '%s\n' "$(cd -- "$MORMOT2_CACHE_DIR" && pwd)"
}

prepare_mormot_static() {
  local mormot_dir="$1"
  local target_cpu="$2"
  local target_os="$3"

  if [[ "$MORMOT2_SKIP_STATIC" == "1" ]]; then
    warn "Skipping mORMot static libraries (MORMOT2_SKIP_STATIC=1)."
    printf '%s\n' ""
    return
  fi

  local checksums="$mormot_dir/static/dev.sha256"
  [[ -f "$checksums" ]] || die "mORMot checksum file not found: $checksums"

  local expected
  expected="$(awk '{
    for (i = 1; i <= NF; i++)
      if ($i ~ /\*?mormot2static\.tgz$/ && i > 1) {
        print $(i - 1)
        exit
      }
  }' "$checksums")"
  [[ -n "$expected" ]] || die "Could not find the mormot2static.tgz checksum in $checksums"

  local static_root="$CACHE_BASE/mormot2-static/$expected"
  local target_dir="$static_root/$target_cpu-$target_os"
  if [[ -d "$target_dir" ]]; then
    log "Reusing verified mORMot static libraries for $target_cpu-$target_os"
    printf '%s\n' "$target_dir"
    return
  fi

  local archive="$DOWNLOAD_DIR/mormot2static-$expected.tgz"
  if [[ -f "$archive" ]]; then
    if ! printf '%s  %s\n' "$expected" "$archive" | sha256sum -c - >/dev/null 2>&1; then
      rm -f "$archive"
    fi
  fi

  if [[ ! -f "$archive" ]]; then
    log "Downloading mORMot static libraries"
    curl --fail --location --retry 3 --output "$archive" "$MORMOT2_STATIC_URL"
  fi

  if ! printf '%s  %s\n' "$expected" "$archive" | sha256sum -c - >/dev/null; then
    rm -f "$archive"
    die "mormot2static.tgz checksum does not match this mORMot checkout. If your checkout is pinned, set MORMOT2_STATIC_URL to its matching release archive, or use MORMOT2_SKIP_STATIC=1."
  fi

  local tmp="$static_root.tmp.$$"
  rm -rf "$tmp"
  mkdir -p "$tmp"
  tar -xzf "$archive" -C "$tmp"

  rm -rf "$static_root"
  mkdir -p "$(dirname "$static_root")"
  if [[ -d "$tmp/static" ]]; then
    mv "$tmp/static" "$static_root"
    rm -rf "$tmp"
  else
    mv "$tmp" "$static_root"
  fi

  if [[ ! -d "$target_dir" ]]; then
    warn "The verified static archive has no $target_cpu-$target_os directory. mORMot can still use its Pascal fallbacks."
    printf '%s\n' ""
    return
  fi

  printf '%s\n' "$target_dir"
}

shell_quote() {
  printf '%q' "$1"
}

generate_wrapper() {
  local version="$1"
  local mormot_dir="$2"
  local static_target_dir="$3"
  local wrapper="$SCRIPT_DIR/fpc"
  local fpc_bin="$FPC_PREFIX/bin/fpc"
  local cfg_dir="$FPC_PREFIX/lib/fpc/etc"
  local exec_dir="$FPC_PREFIX/lib/fpc/$version"

  mkdir -p "$SCRIPT_DIR"

  {
    printf '%s\n' '#!/usr/bin/env bash'
    printf '%s\n' 'set -Eeuo pipefail'
    printf 'FPC_BIN='; shell_quote "$fpc_bin"; printf '\n'
    printf 'FPC_CONFIG_DIR='; shell_quote "$cfg_dir"; printf '\n'
    printf 'FPC_EXEC_DIR='; shell_quote "$exec_dir"; printf '\n'
    printf 'MORMOT2_DIR='; shell_quote "$mormot_dir"; printf '\n'
    printf 'MORMOT_STATIC_TARGET='; shell_quote "$static_target_dir"; printf '\n'
    cat <<'WRAPPER'

args=()

# mORMot's public units live in src plus its first-level subdirectories.
while IFS= read -r -d '' dir; do
  args+=("-Fu$dir")
done < <(find "$MORMOT2_DIR/src" -maxdepth 1 -type d -print0 | sort -z)

if [[ -n "$MORMOT_STATIC_TARGET" && -d "$MORMOT_STATIC_TARGET" ]]; then
  args+=("-Fl$MORMOT_STATIC_TARGET")
fi

# Force this FPC installation's private config and compiler directory so a
# distro-wide FPC 3.2.2 configuration cannot be picked up accidentally.
export PPC_CONFIG_PATH="$FPC_CONFIG_DIR"
export PPC_EXEC_PATH="$FPC_EXEC_DIR"

exec "$FPC_BIN" "${args[@]}" "$@"
WRAPPER
  } > "$wrapper"

  chmod +x "$wrapper"
  log "Generated compiler wrapper: $wrapper"
}

smoke_test() {
  local wrapper="$1"
  if [[ "$MORMOT2_SKIP_SMOKE_TEST" == "1" ]]; then
    warn "Skipping smoke test (MORMOT2_SKIP_SMOKE_TEST=1)."
    return
  fi

  local tmp
  tmp="$(mktemp -d)"
  trap 'rm -rf "${tmp:-}"' RETURN
  mkdir -p "$tmp/units" "$tmp/bin"

  cat > "$tmp/bootstrap_smoke.pas" <<'PAS'
program BootstrapSmoke;

{$mode objfpc}{$H+}

uses
  mormot.core.base;

begin
  WriteLn('FPC + mORMot bootstrap OK');
end.
PAS

  log "Smoke-compiling mORMot with tools/fpc"
  "$wrapper" -FU"$tmp/units" -FE"$tmp/bin" "$tmp/bootstrap_smoke.pas"
  "$tmp/bin/bootstrap_smoke" >/dev/null
  rm -rf "$tmp"
  trap - RETURN
}

main() {
  install_prerequisites
  build_fpc

  local fpc_bin="$FPC_PREFIX/bin/fpc"
  local version target_cpu target_os
  version="$("$fpc_bin" -iV)"
  target_cpu="$("$fpc_bin" -iTP)"
  target_os="$("$fpc_bin" -iTO)"

  link_fpc_commands "$version"

  local mormot_dir
  mormot_dir="$(find_or_clone_mormot2)"
  log "Using mORMot 2 from $mormot_dir"

  local static_target_dir
  static_target_dir="$(prepare_mormot_static "$mormot_dir" "$target_cpu" "$target_os")"

  generate_wrapper "$version" "$mormot_dir" "$static_target_dir"
  smoke_test "$SCRIPT_DIR/fpc"

  local revision="unknown"
  if [[ -f "$FPC_PREFIX/.source-revision" ]]; then
    revision="$(cat "$FPC_PREFIX/.source-revision")"
  fi

  cat >&2 <<EOF

[bootstrap-fpc] Ready.
  FPC:       $version ($target_cpu-$target_os)
  FPC ref:   $FPC_REF @ $revision
  Prefix:    $FPC_PREFIX
  mORMot 2:  $mormot_dir
  Wrapper:   $SCRIPT_DIR/fpc

Use:
  $SCRIPT_DIR/fpc -iV
  $SCRIPT_DIR/fpc path/to/project.lpr

For ChatGPT project instructions, use:
  Before working on Pascal/mORMot 2 code, run tools/bootstrap-fpc.sh. Compile
  relevant changes with tools/fpc and do not rely only on static analysis.
EOF
}

main "$@"
