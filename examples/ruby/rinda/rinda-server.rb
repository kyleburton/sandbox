#!/usr/bin/env ruby -w
# ringserver.rb
# Rinda RingServer

require 'rinda/ring'
require 'rinda/tuplespace'

# start DRb
DRb.start_service
$stderr.puts "DRb service started..."

# Create a TupleSpace to hold named services, and start running
Rinda::RingServer.new Rinda::TupleSpace.new
$stderr.puts "Rinda::RingServer and TupleSpace registered"

# Wait until the user explicitly kills the server.
$stderr.puts "Awaiting on DRb.thread: #{DRb.thread}"
DRb.thread.join
$stderr.puts "DRb exited"
