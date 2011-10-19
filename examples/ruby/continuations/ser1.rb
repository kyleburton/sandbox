class Foo
  attr_accessor :prompt, :fname, :lname

  def initialize
    self.prompt = 'First Name: '
  end

  def first data
    #puts "first: #{data.inspect}"
    self.prompt = 'Last Name: '
    self.fname = data.chomp
  end

  def second data
    #puts "second: #{data.inspect}"
    self.lname = data.chomp
  end

  def full_name
    self.fname + " " + self.lname
  end
end

def controler
  f = Foo.new

  print f.prompt; $stdout.flush; data = $stdin.readline
  f.first data

  print f.prompt; $stdout.flush; data = $stdin.readline
  f.second data
  puts f.full_name
end

controler
