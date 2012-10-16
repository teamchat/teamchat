# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant::Config.run do |config|
  # All Vagrant configuration is done here. The most common configuration
  # options are documented and commented below. For a complete reference,
  # please see the online documentation at vagrantup.com.

  # Every Vagrant virtual environment requires a box to build off of.
  config.vm.box = "ubuntu-12_04"

  # Puppet provision
  config.vm.provision :puppet do |puppet|
    # puppet-emacs should come from git@github.com:nicferrier/emacs-puppet.git
    puppet.module_path = ["puppet/modules", "puppet-emacs"]
    puppet.manifests_path = "puppet/manifests"
    puppet.manifest_file  = "shoesoff.pp"
  end
  
  # This is needed because of ubuntu
  # see this fix https://github.com/mitchellh/vagrant/pull/909
  config.vm.customize ["modifyvm", :id, "--natdnshostresolver1", "on"]
end
