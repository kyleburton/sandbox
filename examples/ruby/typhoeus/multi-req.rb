require 'rubygems'
require 'typhoeus'
require 'json'
require 'lib/util'
require 'rexml/document'
require 'stringio'
include REXML

url = 'http://asymmetrical-view.com/rss.xml'

hydra = Typhoeus::Hydra.new

req = Typhoeus::Request.new(url)

req.on_complete do |response|
  print_response response
  doc = Document.new(response.body)
  XPath.each(doc,"//link") do |item|
    puts "item: #{item}"
    next_req = Typhoeus::Request.new(item.text)
    next_req.on_complete do |resp|
      puts "got back: #{item.text}"
      print_response resp
    end
    hydra.queue next_req
  end
  "done"
end

puts "queueing request"
hydra.queue req
puts "calling hydra.run"
hydra.run
puts "first req.handled_response: #{req.handled_response}"


