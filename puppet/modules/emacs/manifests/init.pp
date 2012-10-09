class emacs {

      group { "emacs":
         ensure => "present"
      }

      user { "emacs":
         require => Group["emacs"],
         gid => "emacs",
         home => "/home/emacs",
      }

      file { "/home/emacs":
         require => User["emacs"],
         ensure => "directory",
         owner => "emacs",
         group => "emacs",
         mode => "u=rwx,g=rx,o=rx",
      }

      file { "emacs-init":
         require => User["emacs"],
         source => "puppet:///modules/emacs/emacs.el",
         path => "/home/emacs/.emacs.el",
         ensure => "present",
         owner => "emacs",
         group => "emacs",
      }

      package { "curl":
      }

      $emacs_dist_url = "https://github.com/downloads/nicferrier/heroku-buildpack-emacs/emacs.tgz"

      exec { "emacs-tarball":
         require => [Package["curl"], User["emacs"]],
         path => "/usr/bin:/bin",
         command => "curl -Ls $emacs_dist_url -o /tmp/emacs-24.tgz",
         creates => "/tmp/emacs-24.tgz",
         user => "emacs",
         group => "emacs",
         logoutput => true
      }        

      exec { "emacs-dist":
         require => [File["/home/emacs"], Exec["emacs-tarball"]],
         path => "/usr/bin:/bin",
         cwd => "/home/emacs",
         command => "tar xvzf /tmp/emacs-24.tgz",
         creates => "/home/emacs/emacs/bin/emacs",
         refresh => "rm -rf /home/emacs/emacs",
         logoutput => true,
         user => "emacs",
         group => "emacs",
      }

      file { "initd":
         require => Exec["emacs-dist"],
         source => "puppet:///modules/emacs/initd",
         path => "/etc/init.d/emacs",
         owner => "root",
         group => "root",
         mode => "u=rx,g=rx",
      }

      service { "emacs":
         require => [File["initd"], File["emacs-init"]],
         ensure => "running",
         enable => "true",
         hasstatus => "true",
      }
}
