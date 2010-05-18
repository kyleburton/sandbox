require 'rubygems'
require 'nanite'
require 'mq'

class Thing
  include Nanite::AMQPHelper

  def options
    {
      :user => 'mapper',
      :pass => 'testing',
      :host => 'localhost',
      :vhost => '/test/exchange'
    }
  end

  def start_amqp
    connection = AMQP.connect({
      :user   =>  options[:user],
      :pass   =>  options[:pass],
      :vhost  => (options[:vhost]  || '/'),
      :host   => (options[:host]   || '127.0.0.1'),
      :port   => (options[:port]   || ::AMQP::PORT).to_i,
      :insist =>  options[:insist] || false,
      :retry  =>  options[:retry]  || 5,
      :connection_status => options[:connection_callback] || proc {|event| 
        $stderr.puts ":connection_callback: event=#{event}"
      }
    })
    amq = MQ.new(connection)
    amq.prefetch 1
    amq
  end

  def run
    # AMQP.start(:host => 'localhost', :prefetch => 1) do
    EM.run do
      amq = start_amqp 
      puts "amq: #{amq}"

      #Signal.trap('INT')  { $stderr.puts "[#{$$}] INT:  AMQP.stop"; AMQP.stop{ EM.stop } }
      #Signal.trap('TERM') { $stderr.puts "[#{$$}] TERM: AMQP.stop"; AMQP.stop{ EM.stop } }

      # test_exchange = MQ.queue '/rn/test-exchange', :durable => true
      queue = amq.queue 'messages', :durable => true
      $stderr.puts "queue: #{queue.class}"
      queue.subscribe(:ack => true) do |hdr, msg|
        $stderr.puts "[#{$$}] hdr: #{hdr}"
        body = Marshal.load msg
        $stderr.puts "[#{$$}] body: #{body.inspect}"
        res = hdr.ack
        $stderr.puts "[#{$$}] hdr.ack => #{res.inspect}"
        sleep 3
      end

      $stderr.puts "[#{$$}] subscribed and waiting, press CTRL+C to stop the event loop..."
    end
    $stderr.puts ".stopped"

  end
end

Thing.new.run
