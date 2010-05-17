require 'rubygems'
require File.dirname(__FILE__) + '/config'
require 'nanite'
require 'pp'

class Agent

  def run
    EM.run do
      puts "ExampleConfig.agent_config="
      pp ExampleConfig.agent_config
      @agent = Nanite::Agent.new(ExampleConfig.agent_config)
      @agent.run
    end
  end

  def self.main
    self.new.run
  end
end
