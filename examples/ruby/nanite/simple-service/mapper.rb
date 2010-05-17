require 'rubygems'
require 'nanite'
require './lib/config.rb'

EM.run do
  service = "/example/addition"

  puts "starting mapper."

  config = ExampleConfig.mapper_config
  config[:callbacks] ||= {}
  config[:callbacks][:register] = lambda { |*args|
    $stderr.puts ":register handler: args.length=#{args.length}"
    arg = [1,2]
    submit_result = Nanite.request( service, arg, :persistent => true ) do |result|
      puts "Result of [#{arg}] = #{result.inspect}"
    end
    puts "Submitted, result=#{submit_result}"
  }


  Nanite.start_mapper config
  puts "..started mapper."


  if true
    arg = [1,2]
    submit_result = Nanite.request( service, arg, :persistent => true ) do |result|
      puts "Result of [#{arg}] = #{result.inspect}"
    end
    puts "Submitted, result=#{submit_result}"
  else
    (4..5).each do |x|
      (11..15).each do |y|
        arg = [x,y]
        submit_result = Nanite.request( service, arg, :persistent => true ) do |result|
          puts "Result of [#{arg}] = #{result.inspect}"
        end
        puts "Submitted: #{arg.inspect} => '#{submit_result}'"
      end
    end
  end

  puts "Request(s) submitted, wait for them to be serviced, or press CTRL+C to stop the mapper immediately."
end


