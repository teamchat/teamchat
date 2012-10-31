class ircd {
  include epel

  package {'ircd-hybrid':
    ensure => latest,
    require => Package["epel-release"],
  }

  file {'etc_ircd_conf':
    ensure  => present,
    path    => '/etc/ircd/ircd.conf',
    owner   => 'ircd',
    group   => 'ircd',
    mode    => '0640',
    source  => "puppet:///modules/ircd/ircd.conf",
    require => Package['ircd-hybrid'],
    notify  => Service['ircd'],
  }

  service {'ircd':
    ensure     => 'running',
    enable     => true,
    # Use reload so hopefully not everyone gets dropped.
    restart    => '/sbin/service ircd reload',
    hasrestart => true,
    hasstatus  => true,
    require    => Package['ircd-hybrid'],
  }
}

