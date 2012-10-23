# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant::Config.run do |config|
  # Do the ircd first, so it's up when the bouncer starts
  config.vm.define :ircd do |ircd_config|
    ircd_config.vm.box = "Centos63"
    ircd_config.vm.box_url = "https://dl.dropbox.com/u/7225008/Vagrant/CentOS-6.3-x86_64-minimal.box"
    ircd_config.vm.provision :puppet do |ircd_puppet|
      ircd_puppet.manifests_path = "~/work/SpikyIRC/puppet/manifests"
      ircd_puppet.module_path = "~/work/SpikyIRC/puppet/modules"
      ircd_puppet.manifest_file  = "site.pp"
    end
    ircd_config.vm.network :hostonly, "192.168.1.11"
  end

  # Now the bouncer box
  config.vm.define :bouncer do |bouncer_config|
    bouncer_config.vm.box = "ubuntu-12_04"

    # Puppet provision
    bouncer_config.vm.provision :puppet do |bouncer_puppet|
      # puppet-emacs should come from git@github.com:nicferrier/emacs-puppet.git
      bouncer_puppet.module_path = ["puppet/modules", "puppet-emacs"]
      bouncer_puppet.manifests_path = "puppet/manifests"
      bouncer_puppet.manifest_file  = "shoesoff.pp"
    end

    # The elnode app
    bouncer_config.vm.forward_port 8001, 8100
    # The bouncer port
    bouncer_config.vm.forward_port 6901, 6667
    # The internal network for the ircd
    bouncer_config.vm.network :hostonly, "192.168.1.10"

    # This is needed because of ubuntu
    # see this fix https://github.com/mitchellh/vagrant/pull/909
    bouncer_config.vm.customize ["modifyvm", :id, "--natdnshostresolver1", "on"]
  end

end
