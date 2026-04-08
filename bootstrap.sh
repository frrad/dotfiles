#!/usr/bin/env bash
set -euo pipefail

apt_updated=0

have_cmd() {
  command -v "$1" >/dev/null 2>&1
}

run_as_user() {
  sudo -Hiu "$SUDO_USER" -- "$@"
}

have_user_cmd() {
  run_as_user sh -lc 'command -v "$1" >/dev/null 2>&1' sh "$1"
}

brew_as_user() {
  run_as_user env PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin" brew "$@"
}

install_with_apt() {
  if [ "$apt_updated" -eq 0 ]; then
    apt-get update
    apt_updated=1
  fi

  apt-get install -y "$@"
}

install_homebrew_if_needed() {
  if ! have_user_cmd brew; then
    run_as_user /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  fi
}

ensure_linux_deps() {
  if ! have_cmd git; then
    install_with_apt git
  fi

  if ! have_cmd puppet; then
    install_with_apt puppet r10k
  fi
}

ensure_darwin_deps() {
  install_homebrew_if_needed

  if ! have_user_cmd git; then
    brew_as_user install git
  fi

  if ! have_user_cmd puppet; then
    brew_as_user install puppet
  fi

  if ! have_user_cmd r10k; then
    brew_as_user install r10k
  fi
}

ensure_deps() {
  case "$(uname -s)" in
    Linux)
      ensure_linux_deps
      ;;
    Darwin)
      ensure_darwin_deps
      ;;
    *)
      echo "Unsupported OS: $(uname -s)" >&2
      exit 1
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
  local user_home

  user_home=$(sudo -Hiu "$SUDO_USER" sh -lc 'printf %s "$HOME"')
  target="${user_home}/dotfiles"
}

ensure_checkout_present() {
  if [ ! -d "$target" ]; then
    run_as_user git clone https://github.com/frrad/dotfiles.git "$target"
  fi
}

apply_puppet() {
  cd "$target/puppet"
  r10k puppetfile install
  export FACTER_sudo_user="$SUDO_USER"
  puppet apply --test --verbose "$target/puppet/main.pp" --modulepath="$target/puppet/modules"
}

run_stow() {
  cd "$target"
  run_as_user ./stow.sh
}

main() {
  require_sudo_context
  resolve_target
  ensure_deps
  ensure_checkout_present
  apply_puppet
  run_stow
}

main "$@"
