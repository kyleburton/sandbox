require 'rubygems'
require 'pp'

class PhoneUtil
  attr_accessor :phone_keypad, :num_to_letters, :letters_to_nums

  def initialize
    @phone_keypad =  \
      [ [  1,  ''     ], [  2,  'ABC' ], [  3,  'DEF' ],
        [  4,  'GHI'  ], [  5,  'JKL' ], [  6,  'MNO' ],
        [  7,  'PQRS' ], [  8,  'TUV' ], [  9,  'WXYZ'],
        [ '*', ''     ], [  0,  ''    ], [ '#', '' ] ]

    @num_to_letters = phone_keypad.inject({}) do |m,tup|
      num, ltrs = tup
      m[num] = ltrs.split //
        m
    end

    @letters_to_nums = phone_keypad.inject({}) do |m,tup|
      num, ltrs = tup
      ltrs.split(//).each do |ltr|
        m[ltr] = num
        m[ltr.downcase] = num
      end
      m
    end
  end

  def word_to_phone word
    word.split(//).inject('') do |s,ltr|
      puts "s=#{s.inspect} #{s.class}"
      s += @letters_to_nums[ltr].to_s
    end
  end

  def self.load_dict file="/usr/share/dict/words"
    words = File.new(file).readlines
    #words = words.select {|w| w.length > 3 }
    words.map { |w| w.upcase }
  end

  def phone_to_words
    # TODO: precompute the phone number version of each of the
    # dictionary words stick this into a multimap
    load_dict.inject({}) do |word|

    end

    # TODO: cache the results of this into a marshall object

  end

end

util = PhoneUtil.new

# puts "util.phone_keypad:"
# pp util.phone_keypad
# 
# puts "util.num_to_letters:"
# pp util.num_to_letters
# 
# puts "util.letters_to_nums:"
# pp util.letters_to_nums

puts "util.word_to_phone('mattress') = #{util.word_to_phone('mattress')}"

words = PhoneUtil.load_dict
puts "words=#{words[0,10]}"
