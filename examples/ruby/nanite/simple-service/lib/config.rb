require 'socket'
module ExampleConfig
  def self.hostname
    ::Socket.gethostname
  end

  def self.base_config 
    { :host                         => '127.0.0.1', # 'localhost',
      :port                         => 5672,
      :daemonize                    => false,
      :vhost                        => '/nanite',
      :log_level                    => 'debug',
      :timeout                      => 2,
      :prefetch                     => 1,
      :persistent                   => true,
      :ping_time                    => 2,
      :retry                        => lambda { |*args|
        $stderr.puts "retry handler (connection broken?): args=#{args.inspect}"
        2
      },
      # doesn't seem to have an effect...
      :connection_callback          => lambda { |event|
        $stderr.puts ":connection_callback, event=#{event}"
        Nanite::Log.debug("Connected to MQ")      if event == :connected
        Nanite::Log.debug("Disconnected from MQ") if event == :disconnected
      }
    }
  end

  def self.mapper_config
    base_config.merge({
      :identity                     => "simple-service-mapper",
      :user                         => 'mapper',
      :pass                         => 'testing',
      #:offline_failsafe             => true,
      #:offline_redelivery_frequency => 30,
      :callbacks => {
        :register => lambda { |*args|
          $stderr.puts ":register handler: args=#{args}"
        },
        :unregister => lambda { |*args|
          $stderr.puts ":unregister handler: args=#{args}"
        },
        :timeout => lambda { |*args|
          $stderr.puts ":timeout handler: args=#{args}"
        },
      }
    })
  end

  def self.agent_config
    base_config.merge({
      :identity                     => "simple-service-agent",
      :user                         => 'nanite',
      :pass                         => 'testing',
      :single_threaded              => true,
      :actors_dir                   => File.dirname(__FILE__) + '/actors',
      :initrb                       => File.dirname(__FILE__) + '/init.rb',
    })
  end
end

