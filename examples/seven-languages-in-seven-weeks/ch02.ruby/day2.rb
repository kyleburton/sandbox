# • Find out how to access files with and without code blocks. What is the benefit of the code block?
fh = File.open("day1.rb","r")
lines = fh.readlines()
fh.close()
puts "lines=#{lines}"

# • How would you translate a hash to an array? Can you translate arrays to hashes?
h1 = {:a => 1, :b => 2, :c => 3, :d => 4, :e => 5}
tuples = []
h1.each do |k,v|
  tuples << [k,v]
end

puts "h1=#{h1}"
puts "tuples=#{tuples}"

# • Can you iterate through a hash?
# yes, see above, order is not guaranteed

# • You can use Ruby arrays as stacks. What other common data structures do arrays support?
#   * FIFO (queues)
#   * using indexes and some simple math it can also easily by a binary tree
#   * linked lists / dynamic lists (though I'm so used to python, ruby, perl, 
#     Clojure, etc that this seems like a given, though I know 'native arrays'
#     in C, C++ and Java don't allow for growth)

# • Print the contents of an array of sixteen numbers, four numbers at a time,
# using just each. Now, do the same with each_slice in Enumerable.
nums = []
16.times do |ii|
  nums << rand(100).to_i
end

nums.each do |num|
  puts "num=#{num}"
end

nums.each_slice(2) do |slice|
  puts "slice=#{slice}"
end

nums.each_slice(3) do |slice|
  puts "slice=#{slice}"
end

# • The Tree class was interesting, but it did not allow you to specify a new
# tree with a clean user interface. Let the initializer accept a nested
# structure of hashes. You should be able to specify a tree like this:
# {'grandpa' => { 'dad' => {'child 1' => {}, 'child 2' => {} }, 'uncle' =>
# {'child 3' => {}, 'child 4' => {} } } }.
load "tree.rb"
tree_example2()

# • Write a simple grep that will print the lines of a file having any occur-
# rences of a phrase anywhere in that line. You will need to do a simple
# regular expression match and read lines from a file. (This is surprisingly
# simple in Ruby.) If you want, include line numbers.

def grep_file(pat, fname="/dev/stdin")
  re = Regexp.new pat
  lnum = 0
  File.open(fname, "r") do |f|
    f.each_line do |line|
      matches = line.scan(re)
      if ! matches.empty?
        print "#{fname}:#{lnum} #{line}"
      end
      lnum = lnum + 1
    end
  end
end
fname = ARGV[0] || 'day2.rb'
grep_file("reg", fname)

