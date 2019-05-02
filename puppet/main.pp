package { 'emacs25-nox':
  ensure => 'latest',
}

package { 'xstow':
  ensure => 'latest',
}

package { 'kpcli':
  ensure => 'latest',
}

package { 'zsh':
  ensure => 'latest',
}

user { $id:
  ensure => present,
  shell  => "/bin/zsh",
}


node  default{
class { 'golang':
  version   => '1.12.4',
  workspace => '/usr/local/src/go',
}}
