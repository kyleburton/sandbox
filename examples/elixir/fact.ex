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


defmodule Defaults do
  def func(p1, p2 \\ 2, p3 \\ 3, p4) do
    [p1, p2, p3, p4]
  end

  def func(p1, p2) do
    [p1, p2]
  end

  # def f2(a, b \\ :defualt) do
  #   {:first, a, b}
  # end

  # def f2(a, 99) do
  #   {:second, a, 99}
  # end
end


defmodule Guess do
  def guess(actual, a..a) do
    IO.puts "It is: #{actual}"
    {:answer: actual}
  end

  def guess(actual, range) do
    minval..maxval = range
  end
end
