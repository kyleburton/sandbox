require 'rubygems'
require 'mq'

AMQP.start(:host => 'localhost') do
  test_exchange = MQ.queue '/rn/test-exchange', :durable => true
  (1..10).each do |counter|
    body = {:from => 'c1', :body => "[#{$$}] this is the body #{counter}"}
    message = Marshal.dump body
    res = test_exchange.publish(message, :mandatory => true, :persistent => true)
    puts "queued, result=#{res} for #{body.inspect}"
  end
  puts "messages queued stopping..."
  AMQP.stop { EM.stop }
end
puts ".stopped"
