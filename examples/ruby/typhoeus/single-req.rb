require 'rubygems'
require 'typhoeus'
require 'json'
require 'lib/util'

url = 'http://asymmetrical-view.com/rss.xml'

response = Typhoeus::Request.get(
  url,
  :headers       => { :Accepts => "text/html" }
)

print_response response
