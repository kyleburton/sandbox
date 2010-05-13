
#!/usr/bin/env ruby -w
# list_services.rb
# List all of the services the RingServer knows about

require 'rinda/ring'

$stderr.puts "starting DRb"
DRb.start_service

$stderr.puts "obtaining Rinda::RingFinder.primary..."
ring_server = Rinda::RingFinger.primary
$stderr.puts "Rinda::RingFinder.primary obtained."

$stderr.puts "reading all tuples"
tuples = ring_server.read_all [nil,nil,nil]
puts "Tuples already present: #{tuples.inspect}"

my_token    = ARGV[0] || 'this'
their_token = ARGV[1] || 'that'
tup = [:to, their_token, rand ]
puts "placing tup=#{tup.inspect}"
ring_server.write [:to, their_token, rand ]

tuple_timeout = 1
while true
  begin
    tup = ring_server.take [:to, my_token, nil], tuple_timeout
    puts "received: #{tup.inspect}"
    #puts "writing 2 more"
    #ring_server.write [:to, their_token, rand ]
    ring_server.write [:to, their_token, rand ]
  rescue Rinda::RequestExpiredError => e
    puts "timed out after #{tuple_timeout}, while waiting for :thing"
    #tuple_timeout += 1
    retry
  end
end

puts "next item: #{tup}"
