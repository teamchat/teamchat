class epel {

  package { 'epel-release':
    ensure   => present,
    provider => 'rpm',
    source   => 'http://dl.fedoraproject.org/pub/epel/6/x86_64/epel-release-6-7.noarch.rpm',
  }
}

