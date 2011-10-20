$k1 = nil
$count = 4

def show_stack
  begin
    raise "showme"
  rescue => e
    puts e.inspect
    puts e.backtrace
  end
end

def foo
  puts "foo: enter"

  the_fn = lambda do
    #puts "foo: count=#{$count} callcc res:'#{res}'"
    puts "foo: count=#{$count}"
    show_stack
    $count -= 1
    bar
  end

  res = callcc do |k|
    $k1 = k
  end

  the_fn.call
end

def bar
  puts "bar: enter"
  if $count > 1
    val = $k1.call
  end
  puts "bar: val='#{val}'"
end

foo

