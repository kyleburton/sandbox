#!/usr/bin/env ruby

puts `rabbitmqctl add_vhost /test/exchange`
puts `rabbitmqctl add_user test-user testing`
puts `rabbitmqctl set_permissions -p /test/exchange test-user ".*" ".*" ".*"`

