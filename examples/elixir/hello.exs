IO.puts "Hello, World!"


:io.format "Regex.run=~p~n", [Regex.run(~r{[aeiou]}, "caterpillar")]

:io.format "Regex.scan=~p~n", [Regex.scan(~r{[aeiou]}, "caterpillar")]



:io.format "Regex.split=~p~n", [Regex.split(~r{[aeiou]}, "caterpillar")]

:io.format "Regex.replace=~p~n", [Regex.replace(~r{[aeiou]}, "caterpillar", "*")]


:io.format "1 in ~p=~p~n", [[1,2,3], 1 in [1,2,3]]

:io.format "\"thing\" in ~p=~p~n", [[1,2,3], "thing" in [1,2,3]]


# auto promotion to a proplist
kwlist = [foo: "bar", qux: "baz"]
:io.format "kwlist=~p~n", [kwlist]



content = "Now is the time"
lp = with {:ok, file} = File.open("/etc/passwd"),
          content     = IO.read(file, :all),
            :ok         = File.close(file),
            [_, uid, gid ] = Regex.run(~r/^lp:.*?:(\d+):(\d+)/m, content) do
       "group: #{gid}, user: #{uid}"
     end

IO.puts lp
IO.puts content



# content = "Now is the time"
# lp = with {:ok, file} = File.open("/etc/passwd"),
#           content     = IO.read(file, :all),
#             :ok         = File.close(file),
#             [_, uid, gid ] = Regex.run(~r/^lpXX:.*?:(\d+):(\d+)/m, content) do
#        "group: #{gid}, user: #{uid}"
#      end

# IO.puts lp
# IO.puts content


content = "Now is the time"
lp = with {:ok, file} = File.open("/etc/passwd"),
          content     = IO.read(file, :all),
            :ok         = File.close(file),
            [_, uid, gid ] <- Regex.run(~r/^lpXX:.*?:(\d+):(\d+)/m, content) do
       "group: #{gid}, user: #{uid}"
     end

IO.puts lp
IO.puts content


sum = fn (a, b) -> a + b end
sum.(1, 2)


greet = fn -> IO.puts "Hello" end
greet.()


sum = fn a, b -> a + b end
IO.puts "sum(99, 99) = #{sum.(99, 99)}"


swap = fn {a, b} -> {b, a} end
tup1 = {:a, :b}
:io.format "swap(~p) = ~p~n", [tup1, swap.(tup1)]

list_concat = fn
  a, b -> a ++ b
end

list_concat.([:a, :b], [:c, :d])

sum = fn a, b, c -> a+b+c end
sum.(1,2,3)


pair_typle_to_list = fn
  {a, b} -> [a,b]
end
pair_typle_to_list.({1234, 5678})


handle_open = fn
  {:ok, file} -> "Read data: #{IO.read(file, :line)}"
  {_, error}  -> "Error: #{:file.format_error(error)}"
end

handle_open.(File.open("/etc/passwd"))

handle_open.(File.open("/does/not/exist"))


3 + 99


# Exercise: Functions-2

thing = fn (a, b, c) ->
  case {a, b, c} do
    {0, 0, _} -> "FizzBuzz"
    {0, _, _} -> "Fizz"
    {_, 0, _} -> "Buzz"
    _         -> c
  end
end
  
thing.(0, 0, :none)
thing.(1, 0, :none)
thing.(0, 1, :none)
thing.(1, 1, :none)


thing2 = fn n -> thing.(rem(n, 3), rem(n, 5), n) end


thing2.(10)
thing2.(11)
thing2.(12)
thing2.(13)
thing2.(14)
thing2.(15)
thing2.(16)
thing2.(17)

for n <- 1..27 do
    thing2.(n)
end

greeter = fn name -> (fn -> "Hello #{name}" end) end
greeter.("Robert Ross").()

add_n = fn n -> (fn m -> n + m end) end
add_two = add_n.(2)
add_two.(3)

prefix = fn s -> (fn s2 -> "#{s} #{s2}" end) end
prefix.("Mrs").("Smith")

list = [1, 3, 5, 7, 9]
Enum.map list, fn elem -> elem * 2 end
Enum.map list, fn elem -> elem * elem end
Enum.map list, fn elem -> elem > 6 end


defmodule Greeter do
  def for(name, greeting) do
          fn
            (^name) -> "#{greeting} #{name}"
            (_)     -> "I don't know you"
          end
      end
end


mr_valim = Greeter.for("Jose", "Oi!")


IO.puts mr_valim.("Jose")
IO.puts mr_valim.("Dave")


add_one = &(&1 + 1)
add_one.(44)

list = [1, 3, 5, 7, 9]
Enum.map list, &(&1 * 2)
Enum.map list, &(&1 * &1)
Enum.map list, &(&1 > 6)


divrem = &{ div(&1, &2), rem(&1, &2) }
divrem.(13, 5)

s = &"bacon & #{&1}"
s.("custard")

match_end = &~r/.*#{&1}$/

"cat" =~ match_end.("t")
"cat" =~ match_end.("!")

l = &length/1
% &:erlang.length/1


Enum.map [1,2,3,4], &(&1 + 2)
Enum.map [1,2,3,4], &IO.inspect/1
