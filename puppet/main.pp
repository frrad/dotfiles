package { 'emacs-nox':
  ensure => 'latest',
}

package { 'htop':
  ensure => 'latest',
}

package { 'python3-pip':
  ensure => 'latest',
}

package { 'ripgrep':
  ensure => 'latest',
}

package { 'binutils':
  ensure => 'latest',
}

package { 'make':
  ensure => 'latest',
}

package { 'apt-file':
  ensure => 'latest',
}

package { 'xstow':
  ensure => 'latest',
}

package { 'keychain':
  ensure => 'latest',
}

package { 'colordiff':
  ensure => 'latest',
}

package { 'tree':
  ensure => 'latest',
}

package { 'kpcli':
  ensure => 'latest',
}

package { 'zsh':
  ensure => 'latest',
}

user { $::sudo_user:
  ensure => present,
  shell  => "/bin/zsh",
}


node  default{
class { 'golang':
  version   => '1.14.1',
  workspace => '/usr/local/src/go',
}}
