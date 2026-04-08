#!/usr/bin/env bash
set -euo pipefail

apt_updated=0
package_manifest=
pkg_manager=
script_dir=
target=
user_home=
os_name=

have_cmd() {
  command -v "$1" >/dev/null 2>&1
}

run_as_user() {
  sudo -Hu "$SUDO_USER" -- "$@"
}

brew_path="/home/linuxbrew/.linuxbrew/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin"

have_user_cmd() {
  run_as_user env PATH="$brew_path" \
    sh -lc 'command -v "$1" >/dev/null 2>&1' sh "$1"
}

brew_as_user() {
  run_as_user env PATH="$brew_path" brew "$@"
}

install_with_apt() {
  if [ "$apt_updated" -eq 0 ]; then
    apt-get update
    apt_updated=1
  fi

  apt-get install -y "$@"
}

install_with_brew() {
  brew_as_user install "$@"
}

# Install a package using whatever system package manager is available.
# Used only for pre-brew bootstrap prerequisites (e.g. curl).
install_with_system_pm() {
  if have_cmd apt-get; then
    install_with_apt "$@"
  elif have_cmd dnf; then
    dnf install -y "$@"
  elif have_cmd yum; then
    yum install -y "$@"
  fi
}

install_homebrew_if_needed() {
  if ! have_user_cmd brew; then
    # Homebrew on Linux requires a C toolchain and a handful of utilities.
    # Install them via the system package manager before handing off.
    if [ "$os_name" = "Linux" ]; then
      install_with_system_pm curl file gcc gcc-c++ make procps-ng
    fi
    run_as_user env NONINTERACTIVE=1 /bin/bash -c \
      "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  fi
}

resolve_os() {
  os_name="$(uname -s)"
}

resolve_pkg_manager() {
  case "$os_name" in
    Darwin)
      pkg_manager="brew"
      ;;
    Linux)
      if have_cmd apt-get; then
        pkg_manager="apt"
      else
        pkg_manager="brew"
      fi
      ;;
    *)
      echo "Unsupported OS: $os_name" >&2
      exit 1
      ;;
  esac
}

resolve_script_dir() {
  script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  package_manifest="${script_dir}/packages.tsv"
}

managed_cmd_exists() {
  local apt_check_cmd="$1"
  local brew_check_cmd="$2"

  case "$pkg_manager" in
    apt)
      [ -n "$apt_check_cmd" ] && have_cmd "$apt_check_cmd"
      ;;
    brew)
      [ -n "$brew_check_cmd" ] && have_user_cmd "$brew_check_cmd"
      ;;
  esac
}

package_spec() {
  local apt_packages="$1"
  local brew_packages="$2"

  case "$pkg_manager" in
    apt)  printf '%s\n' "$apt_packages" ;;
    brew) printf '%s\n' "$brew_packages" ;;
  esac
}

require_sudo_context() {
  if [ -z "${SUDO_USER:-}" ]; then
    echo "bootstrap.sh must be run via sudo so SUDO_USER is available" >&2
    exit 1
  fi
}

resolve_target() {
  user_home=$(sudo -Hiu "$SUDO_USER" sh -lc 'printf %s "$HOME"')
  target="${user_home}/dotfiles"
}

ensure_bootstrap_deps() {
  case "$pkg_manager" in
    apt)
      if ! have_cmd git; then
        install_with_apt git
      fi
      ;;
    brew)
      install_homebrew_if_needed
      if ! have_user_cmd git; then
        install_with_brew git
      fi
      ;;
  esac
}

ensure_checkout_present() {
  if [ ! -d "$target" ]; then
    run_as_user git clone https://github.com/frrad/dotfiles.git "$target"
  fi
}

ensure_packages() {
  local name apt_check_cmd brew_check_cmd apt_packages brew_packages
  local -a package_args

  while IFS='|' read -r name apt_check_cmd brew_check_cmd apt_packages brew_packages; do
    if [ -z "$name" ] || [[ "$name" == \#* ]]; then
      continue
    fi

    if managed_cmd_exists "$apt_check_cmd" "$brew_check_cmd"; then
      continue
    fi

    pkg=$(package_spec "$apt_packages" "$brew_packages")
    if [ -z "$pkg" ]; then
      echo "Skipping $name on $os_name ($pkg_manager): no package mapping" >&2
      continue
    fi

    read -r -a package_args <<<"$pkg"
    case "$pkg_manager" in
      apt)  install_with_apt  "${package_args[@]}" </dev/null ;;
      brew) install_with_brew "${package_args[@]}" </dev/null ;;
    esac
  done < "$package_manifest"
}

ensure_ssh_key() {
  local ssh_dir
  local key_path

  ssh_dir="${user_home}/.ssh"
  key_path="${ssh_dir}/id_rsa"

  run_as_user mkdir -p "$ssh_dir"
  if [ ! -f "$key_path" ]; then
    run_as_user sh -lc 'ssh-keygen -t rsa -b 4096 -N "" -f "$1"' sh "$key_path"
  fi
}

run_stow() {
  cd "$target"
  case "$pkg_manager" in
    brew) run_as_user env PATH="$brew_path" ./stow.sh ;;
    *)    run_as_user ./stow.sh ;;
  esac
}

main() {
  require_sudo_context
  resolve_os
  resolve_pkg_manager
  resolve_script_dir
  resolve_target
  ensure_bootstrap_deps
  ensure_checkout_present
  ensure_packages
  ensure_ssh_key
  run_stow
}

main "$@"
