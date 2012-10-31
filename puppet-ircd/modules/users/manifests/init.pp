class users {

  # The user used to provision the users from the bouncer app
  user { 'bouncerprovisioner':
    ensure     => present,
    managehome => true,
    groups     => 'wheel',
  }

  ssh_authorized_key { "bouncer_provisioner_key":
    ensure => present,
    key    => "AAAAB3NzaC1yc2EAAAADAQABAAABAQDLT2Z1kNmeNbfXWDGtwei4TOl/tgW8RuyEp8FVsjkoVNfqNSUOEFhyYekjh/y5TYC95i6kZrBvKIsXO9TCmQ0kxRrhLwvwMMXLAF8QTs60bote6ExRL1pSNwmYP92wUpnJ7o5zMSUH9Pm3HKeAMSQ6sLZYNZ9VKtU07/zFbQfYKVBVd1pRjr/atpJ0Z9qkiYQbzqLyQUoKCQvdastsk2VHzgXdYnErhYH0E+Bg/1MqEVUZ/VpYirRe0FiKXzdtRq1O/cYzgOHtq1rNCcr/jzOGqHD4FsCJ29Jamksk7jfNC0wvUT0uPdkO0gDm3gMU3gCVTO3BJn0kTSFNkBNm9qC7",
    type   => "rsa",
    user   => "bouncerprovisioner",
    require => User["bouncerprovisioner"]
  }
}
