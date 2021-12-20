require 'rubygems'
require 'uri'
require 'base_app'
require 'net/http'
require 'hpricot'

# http://github.com/whymirror/hpricot

class JarvanaCli < BaseApp
  def initialize
    super
    @jarvana_url = "http://www.jarvana.com/jarvana/search-xml/1.0"
    @commands = {
      :search  => :search_content,
      :class   => :search_class,
      :project => :search_project,
      :content => :search_content,
    }
    # api url : http://www.jarvana.com/jarvana/api
    # http://www.jarvana.com/jarvana/search-xml/1.0?search_type=class&java_class=org.apache.commons.lang.StringUtils&start=0
  end

  def command_line_arguments
    super.concat []
  end

  def search_by_url(term, qs)
    url = sprintf "%s?%s",
            @jarvana_url,
            qs
    puts "Searching for #{term.inspect} : #{url}"
    resp = Net::HTTP.get_response URI.parse(url)
    puts "resp: #{resp.body}"

    doc = Hpricot::XML(resp.body)
    num_results = (doc/:resultSize)
    puts "Results for #{term} => '#{num_results}"
    if 0 == num_results.to_i
      puts "No results found for #{term}"
    end
  end

  def search_content(args)
    search_by_url args[0], sprintf("search_type=content&content=%s&start=0",URI.escape(args[0]))
  end

  def search_project(args)
    search_by_url args[0], sprintf("search_type=project&project=%s&start=0",URI.escape(args[0]))
  end

  def search_class(args)
    search_by_url args[0], sprintf("search_type=class&java_class=%s&start=0",URI.escape(args[0]))
  end

  def run
    unless ARGV.size > 0
      raise "Error: you must supply a command (#{@commands.keys.join(", ")})"
    end

    command = ARGV.shift
    fn = @commands[command.to_sym]
    unless fn
      raise "Error: invalid command '#{command}' (#{@commands.keys.join(", ")})"
    end

    self.send(fn,ARGV)
  end
end

