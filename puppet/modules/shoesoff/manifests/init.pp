class shoesoff {
      include emacs

      $local_archive = "/vagrant/shoesoff-elpa"

      exec { "install-talk-app":
         require => Service["emacs"],
         #command => "/etc/init.d/emacs install talkapp $local_archive",
         command => "true", 
         path => "/bin:/usr/bin",
      }



      # Make a key file for the vagrant user
      exec { "make-local-key":
         command => "ssh-keygen -t rsa -N '' -f /home/vagrant/.ssh/id_rsa",
         creates => "/home/vagrant/.ssh/id_rsa",
         logoutput => true,
         path => "/usr/bin",
         user => "vagrant",
      }

      # This is so the vagrant user can ssh to the emacs user for emacsclient
      exec { "access-emacs":
         command => "cat /home/vagrant/.ssh/id_rsa.pub | sudo -u emacs tee -a /home/emacs/.ssh/authorized_keys",
         logoutput => true,
         path => "/usr/bin:/bin",
         user => "vagrant",
         require => [Exec["install-talk-app"], Exec["make-local-key"]],
         unless => "sudo grep vagrant@ /home/emacs/.ssh/authorized_keys"
      }
}
