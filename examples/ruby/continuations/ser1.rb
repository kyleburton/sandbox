class Foo
  attr_accessor :prompt, :fname, :lname

  def initialize
    self.prompt = 'First Name: '
  end

  def first data
    self.prompt = 'Last Name: '
    self.fname = data.chomp
  end

  def second data
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






