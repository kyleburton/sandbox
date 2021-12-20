require 'rubygems'
require 'open-uri'
require 'dbm'
require 'pp'

class Swear
  def initialize
    @word_cache_file = ".swear.db"
    @words           = load_cache @word_cache_file, []
    @url_cache       = DBM.open('.url.dbm',0666)
  end

  def store_results
    store_cache @word_cache_file, @words
  end

  # TODO: move out into a util mixin or helper
  def store_cache file, datum
    Marshal.dump datum, File.new(file,'w')
  end

  # TODO: move out into a util mixin or helper
  def load_cache file, default=nil
    if File.exists? @word_cache_file
      return Marshal.load File.new(file,'r')
    end
    default
  end

  # TODO: move into a util class
  def fetch url
    if @url_cache.key?(url)
      $stderr.puts "Returning from cache: #{url}"
      return @url_cache[url]
    end
    $stderr.puts "Fetching URL: #{url}"
    @url_cache[url] = open(url).read
  end

  def get_words_block_from_page page
    start = page.index "user submitted swear words."
    start = page.index "<table", start
    start = page.index "<tr", start
    start = page.index "<td", start
    end_pos = page.index "</td>", start
    puts "extracting from #{start} to #{end_pos}"
    page[start,end_pos-start]
  end

  def load_words
    if @words.empty?
      alpha = "abcdefghijklmnopqrstuvwxyz"
      alpha.split(//).each do |letter|
        url = "http://www.noswearing.com/dictionary/#{letter}"
        puts "fetch url: #{url}"
        page = fetch url
        puts "Page: #{page.size} #{page.class}"
        num_words = page.match(/Below is a list of.+?(\d+).+?user submitted/)[1].to_i
        puts "  need to find #{num_words} in the page..."
        if 0 == num_words
          puts "No Results for #{letter}, skipping"
          next
        end
        block = get_words_block_from_page page
        lines = block.split(/<br><br>/).map {|line|
          unless line.match(/<b>(.+?)<\/b>/)
            puts "DNE: #{letter} #{line}"
          end
          line.match(/<b>(.+?)<\/b>/)[1]
        }
        puts "lines=#{lines.inspect}"
        @words.concat lines
      end
    end
    @words
  end


  def run
    load_words
    puts "Swear Words:"
    @words.each {|w| puts "  #{w}"}
  end
end

sw = Swear.new
sw.run
sw.store_results

# TODO: expirement with a bloom filter
# to test if a word is a swear word
