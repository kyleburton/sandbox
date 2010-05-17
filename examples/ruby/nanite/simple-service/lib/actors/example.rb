class Example
  include Nanite::Actor
  expose :addition

  def addition payload
    puts "payload = #{payload.inspect}"
    a,b = payload
    a + b
  end
end
