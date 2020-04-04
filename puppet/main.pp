$packages = [
  'apt-file',
  'binutils',
  'colordiff',
  'emacs-nox',
  'htop',
  'kpcli',
  'make',
  'python3-pip',
  'ripgrep',
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
class { 'golang':
  version   => '1.14.1',
  workspace => '/usr/local/src/go',
}}
