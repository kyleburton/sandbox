defmodule Times do
  def double(n) do
    n * 2
  end

  def double2(n), do: n * 2

  def triple(n) do
    n * 3
  end

  def quadruple(n) do
    double(double(n))
  end
  
end



defmodule Factorial do
  def of(0), do: 1
  def of(n), do: n * of(n-1)
end
