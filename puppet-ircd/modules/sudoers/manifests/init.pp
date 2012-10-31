class sudoers {
  file { 'sudoers':
    ensure => present,
    path   => '/etc/sudoers',
    mode   => '0440',
    owner  => 'root',
    group  => 'root',
    source => "puppet:///modules/sudoers/sudoers",
  }
}

