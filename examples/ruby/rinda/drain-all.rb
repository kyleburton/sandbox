#!/usr/bin/env ruby -w
# list_services.rb
# List all of the services the RingServer knows about

require 'rinda/ring'

$stderr.puts "starting DRb"
DRb.start_service

$stderr.puts "obtaining Rinda::RingFinder.primary..."
ring_server = Rinda::RingFinger.primary
$stderr.puts "Rinda::RingFinder.primary obtained."

tuple_timeout = 0
[[nil],[nil,nil],[nil,nil,nil]].each do |tup|
  while true
    items_found = false
    begin
      tup = ring_server.take tup, tuple_timeout
      puts "popped: #{tup.inspect}"
      items_found = true
    rescue Rinda::RequestExpiredError => e
      puts "timed out after #{tuple_timeout}, while waiting for :thing"
    end
    break unless items_found
  end

end
