defmodule Fact do
  def factorial(n) when is_integer(n) and n > 0 do
    factorial(n, 1)
  end

  def factorial(0, acc) do
    acc
  end

  def factorial(1, acc) do
    acc
  end

  def factorial(n, acc) do
    factorial(n-1, acc*n)
  end

  def thing(x) do
    x
  end

  def thing2(x) do
    x
  end
end


# Fact.thing2 "three"
defmodule ModulesAndFunctions4 do
  def sum(0, acc) do
    acc
  end

  def sum(ctr, acc) do
    sum(ctr - 1, ctr + acc)
  end

  def sum(upto) do
    sum(upto, 0)
  end


  def gcd(x, 0) do
    x
  end

  def gcd(x, x) do
    x
  end
  
  def gcd(x, y) do
    gcd(y, rem(x, y))
  end
end


defmodule Guard do
  def what_is(x) when is_number(x) do
    :number
  end

  def what_is(x) when is_list(x) do
    :list
  end
  
  def what_is(x) when is_atom(x) do
    :atom
  end
  
end


# defmodule Defaults do
#   def func(p1, p2 \\ 2, p3 \\ 3, p4) do
#     [p1, p2, p3, p4]
#   end

#   def func(p1, p2) do
#     [p1, p2]
#   end

#   # def f2(a, b \\ :defualt) do
#   #   {:first, a, b}
#   # end

#   # def f2(a, 99) do
#   #   {:second, a, 99}
#   # end
# end


defmodule Chop do
  def guess(actual, lower..upper) do
    mid = lower + div(upper - lower, 2)
    IO.puts "Is it #{mid}"
    Chop.check(actual, mid, lower, upper)
  end

  def check(_, _, x, x) do
    IO.puts "Error: range is empty!"
    :failed
  end
  
  def check(_, x, x, _) do
    IO.puts "Error: at lower!"
    :failed
  end
  
  def check(_, x, _, x) do
    IO.puts "Error: at upper!"
    :failed
  end
  
  def check(actual, mid, _lower, _uppper) when actual == mid do
    actual
  end

  def check(actual, mid, _lower, upper) when actual > mid do
    Chop.guess(actual, mid..upper)
  end

  def check(actual, mid, lower, _upper) when actual < mid do
    Chop.guess(actual, lower..mid)
  end
end
