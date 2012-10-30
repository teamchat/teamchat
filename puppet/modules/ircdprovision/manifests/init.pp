class ircdprovision {

      # host { "ircd":
      #    ip => "192.168.1.11", # from vagrant multi config
      #    name => "irc.jbx.cc",
      # }

      file { ".ssh":
         require => User["emacs"],
         ensure => directory,
         path => "/home/emacs/.ssh",
         owner => "emacs",
         group => "emacs",
         mode => "u=rwx,g=,o=",
      }
      
      # Private key side of a keypair, the ircd has the other side
      file { "privatekey":
         require => File[".ssh"],
         path => "/home/emacs/.ssh/ircdprovision",
         source => "puppet:///modules/ircdprovision/privatekey",
         owner => "emacs",
         group => "emacs",
         mode => "u=rx,g=,o=",
      }

      # The script to create ircd users
      file { "ircdmakeuser":
         require => User["emacs"],
         path => "/home/emacs/ircdmakeuser",
         source => "puppet:///modules/ircdprovision/ircdmakeuser",
         owner => "emacs",
         group => "emacs",
         mode => "u=rwx,g=,o=",
      }

}
