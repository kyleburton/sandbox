
def prompt s
  print s
  $stdout.flush
  readline
end

class Example
  def run
    first = prompt "First Name: "
    last  = prompt "Last Name: "
    full_name = first + " " + last
    puts full_name
  end
end

Example.new.run
