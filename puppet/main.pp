$packages = [
  'apt-file',
  'binutils',
  'black',
  'colordiff',
  'emacs-nox',
  'fzf',
  'git',
  'graphviz',
  'htop',
  'ispell',
  'jq',
  'kpcli',
  'make',
  'nano',
  'pv',
  'python3-pip',
  'python3-venv',
  'ripgrep',
  'screen',
  'tree',
  'xstow',
  'zsh',
]

package { $packages: ensure => 'latest' }

user { $::sudo_user:
  ensure => present,
  shell  => "/bin/zsh",
}

node  default{
  $arch = $facts['architecture'] ? {
    'aarch64' => 'linux-arm64',
    default   => 'linux-amd64',
  }

  class { 'golang':
    arch => $arch,
    version   => '1.16',
    workspace => '/usr/local/src/go',
  }
}

ssh_keygen { $::sudo_user:
  bits => 4096
}
