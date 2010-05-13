#!/usr/bin/env ruby -w
# list_services.rb
# List all of the services the RingServer knows about

require 'rinda/ring'

$stderr.puts "starting DRb"
DRb.start_service

$stderr.puts "obtaining Rinda::RingFinder.primary..."
ring_server = Rinda::RingFinger.primary
$stderr.puts "Rinda::RingFinder.primary obtained."

[[nil],[nil,nil],[nil,nil,nil]].each do |tup|
  tuples = ring_server.read_all tup

end
