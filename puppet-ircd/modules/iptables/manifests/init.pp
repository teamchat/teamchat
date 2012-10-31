class iptables {
  service {'iptables':
    restart    => '/etc/init.d/iptables reload',
    hasstatus  => true,
  }
}
