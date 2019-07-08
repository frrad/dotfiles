package { 'emacs25-nox':
  ensure => 'latest',
}

package { 'htop':
  ensure => 'latest',
}

package { 'python3-pip':
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

user { $id:
  ensure => present,
  shell  => "/bin/zsh",
}


node  default{
class { 'golang':
  version   => '1.12.4',
  workspace => '/usr/local/src/go',
}}
