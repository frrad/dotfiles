#!/usr/bin/env bash
set -euo pipefail

apt_updated=0
package_manifest=
script_dir=
target=
user_home=
os_name=
linux_package_manager=

have_cmd() {
  command -v "$1" >/dev/null 2>&1
}

run_as_user() {
  sudo -Hu "$SUDO_USER" -- "$@"
}

have_user_cmd() {
  run_as_user env PATH="/opt/homebrew/bin:/home/linuxbrew/.linuxbrew/bin:/usr/local/bin:/usr/bin:/bin" \
    sh -lc 'command -v "$1" >/dev/null 2>&1' sh "$1"
}

brew_as_user() {
  run_as_user env PATH="/opt/homebrew/bin:/home/linuxbrew/.linuxbrew/bin:/usr/local/bin:/usr/bin:/bin" brew "$@"
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

install_homebrew_if_needed() {
  if ! have_user_cmd brew; then
    run_as_user env NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  fi
}

resolve_os() {
  os_name="$(uname -s)"

  if [ "$os_name" = "Linux" ]; then
    if have_cmd apt-get; then
      linux_package_manager=apt
    else
      linux_package_manager=brew
    fi
  fi
}

resolve_script_dir() {
  script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  package_manifest="${script_dir}/packages.tsv"
}

managed_cmd_exists() {
  local linux_check_cmd="$1"
  local darwin_check_cmd="$2"

  case "$os_name" in
    Linux)
      case "$linux_package_manager" in
        apt)
          [ -n "$linux_check_cmd" ] && have_cmd "$linux_check_cmd"
          ;;
        brew)
          [ -n "$darwin_check_cmd" ] && have_user_cmd "$darwin_check_cmd"
          ;;
        *)
          return 1
          ;;
      esac
      ;;
    Darwin)
      [ -n "$darwin_check_cmd" ] && have_user_cmd "$darwin_check_cmd"
      ;;
    *)
      return 1
      ;;
  esac
}

package_spec_for_current_os() {
  local apt_packages="$1"
  local brew_packages="$2"

  case "$os_name" in
    Linux)
      case "$linux_package_manager" in
        apt)
          printf '%s\n' "$apt_packages"
          ;;
        brew)
          printf '%s\n' "$brew_packages"
          ;;
        *)
          return 1
          ;;
      esac
      ;;
    Darwin)
      printf '%s\n' "$brew_packages"
      ;;
    *)
      return 1
      ;;
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
  case "$os_name" in
    Linux)
      case "$linux_package_manager" in
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
        *)
          echo "Unsupported Linux package manager" >&2
          exit 1
          ;;
      esac
      ;;
    Darwin)
      install_homebrew_if_needed
      if ! have_user_cmd git; then
        install_with_brew git
      fi
      ;;
    *)
      echo "Unsupported OS: $os_name" >&2
      exit 1
      ;;
  esac
}

ensure_checkout_present() {
  if [ ! -d "$target" ]; then
    run_as_user git clone https://github.com/frrad/dotfiles.git "$target"
  fi
}

ensure_packages() {
  local name linux_check_cmd darwin_check_cmd apt_packages brew_packages package_spec
  local -a package_args

  while IFS='|' read -r name linux_check_cmd darwin_check_cmd apt_packages brew_packages; do
    if [ -z "$name" ] || [[ "$name" == \#* ]]; then
      continue
    fi

    if managed_cmd_exists "$linux_check_cmd" "$darwin_check_cmd"; then
      continue
    fi

    package_spec=$(package_spec_for_current_os "$apt_packages" "$brew_packages")
    if [ -z "$package_spec" ]; then
      echo "Skipping $name on $os_name: no package mapping" >&2
      continue
    fi

    read -r -a package_args <<<"$package_spec"
    case "$os_name:$linux_package_manager" in
      Linux:apt)
        install_with_apt "${package_args[@]}" </dev/null
        ;;
      Linux:brew|Darwin:*)
        install_with_brew "${package_args[@]}" </dev/null
        ;;
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
  run_as_user ./stow.sh
}

main() {
  require_sudo_context
  resolve_os
  resolve_script_dir
  resolve_target
  ensure_bootstrap_deps
  ensure_checkout_present
  ensure_packages
  ensure_ssh_key
  run_stow
}

main "$@"
