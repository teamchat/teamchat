class shoesoff {
      include emacs

      $local_archive = "/vagrant/shoesoff-elpa"

      exec { "install-talk-app":
         require => Service["emacs"],
         command => "/etc/init.d/emacs install talk $local_archive",
      }
}
