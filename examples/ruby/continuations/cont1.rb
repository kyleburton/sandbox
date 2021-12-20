def main
  i = 0
  callcc do |k|
    puts i
    k.call
    i = 1
  end
  puts i
end


main
