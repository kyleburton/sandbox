$verbose = false

class Baseball < Exception
end

class Base
  attr_accessor :k, :p, :resp

  def prompt s
    self.k ||= []
    self.resp = nil
    self.p = s
    kont = nil
    callcc do |k|
      kont = k
    end

    if self.k.empty? || kont != self.k.last[:k]
      self.k.push({:k=>kont,:s=>s})
    end

    if self.resp.nil?
      raise Baseball.new
    end
    self.resp
  end

  def run_app
    run = true
    while run
      begin
        self.run
        run = false
      rescue Baseball => b
        $verbose and puts "kont stack: #{self.k.inspect}"
        print self.k.last[:s]
        $stdout.flush
        resp = $stdin.readline.chomp
        if resp =~ /^\d+$/
          k = self.k[resp.to_i]
          $verbose and puts "Going back to #{k} at #{resp} of #{self.k.size} save spots"
          k[:k].call
        else
          self.resp = resp
          self.k.last[:k].call(self.resp)
        end
      end
    end
  end
end

class Foo < Base
  attr_accessor :full_name
  def run
    first = prompt "First Name: "
    last  = prompt "Last Name: "
    prompt "Are you done?: "
    self.full_name = first + " " + last
  end
end

f = Foo.new
f.run_app
puts f.full_name
