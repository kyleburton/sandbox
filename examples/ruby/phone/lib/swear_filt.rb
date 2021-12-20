require 'rubygems'
require 'bloom_filter'

class SwearFilt
  def initialize
    #@filter = BloomFilter.new(:size => 5*1024*1024, :hashes => 10, :seed => 97, :bucket => 3, :raise => false )
    @filter = BloomFilter.new(5*1024*1024,10)
    @proj_dir = File.dirname(__FILE__) + "/.."
    @digits = ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
    @leet = {
      '1' => 'l',
      '3' => 'e',
      '4' => 'a',
      '5' => 's',
      '6' => 'g',
      '7' => 't',
      '0' => 'o'
    }
    @badwords = {}
  end

  def num_to_code num
    code = ''

    while num >= @digits.size
      val = num % @digits.size

      code = @digits[val] + code
      num = (num / @digits.size).to_i
    end
    code = @digits[num] + code

    while code.size < 6
      code = "0" + code
    end
    if code.size > 6
      raise "Error: exceeded max len (6): #{num} => #{code} / #{code.size}"
    end
    code
  end

  def de_leet word
    word = word.split(//).map { |ch|
      @leet[ch] || ch
    }.join('')

  end

  def word_to_alternates word
    cand = word.downcase
    while cand.start_with? '0'
      cand = cand.gsub(/^0/,'')
    end

    cand2 = word.downcase
    while cand2.end_with? '0'
      cand2 = cand2.gsub(/0$/,'')
    end

    [de_leet(cand),de_leet(cand2)]
  end

  def bad_word? word
    res = word_to_alternates(word).any? do |word|
      @badwords[word] || false # || @filter.include?(word)
    end

    print "," if res
    filt_indication = @filter.include?(word)
    if res && !filt_indication
      puts "FN: hash indicated, bloom filt did not: #{word}"
    end

    if !res && filt_indication
      puts "FP: bloom filt indicated, hash did not: #{word}"
    end

    res
  end

  def run
    puts "building bloom filter.."
    File.readlines(@proj_dir + "/all-badwords.txt").each do |w|
      w.chomp!
      @filter.add(w.downcase)
      @badwords[w.downcase] = true
    end
    puts ".done"

    accept = File.new('accepted.txt','w')
    reject = File.new('rejected.txt','w')
    idx = 0
    while true
      code = num_to_code idx

      if bad_word? code
        reject.puts code
      else
        accept.puts code
      end

      if 0 == (idx % 1000)
        $stdout.print '.'
        $stdout.flush
        print "\n" if 0 == (idx % 100_000)
        accept.flush
        reject.flush
      end
      idx = idx + 1
    end
  rescue
    accept.close if accept
    reject.close if reject
  end
end

SwearFilt.new.run
