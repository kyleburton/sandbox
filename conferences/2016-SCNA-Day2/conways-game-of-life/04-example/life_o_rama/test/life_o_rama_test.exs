defmodule LifeORamaTest do
  use ExUnit.Case
  doctest LifeORama

  # init the world
  #  - ref to process that represents the world
  #  - the world is a 'grid' of cells
  #  - a cell is a process
  #     recv
  #      . alive / dead
  #      . list / set of neighbors
  #      ! add neighbor     -> ok & recur
  #      ! get state        -> ! state & recur
  #      ! calc-next-state  -> ok & recur
  #      ! tick             -> ok & recur

  test "create & destroy cell" do
    cpid = LifeORama.create_cell(:alive)
    send(cpid, {:get_state, self})
    assert_receive :alive
    LifeORama.destroy_cell(cpid)
  end

  test "can update and track state" do
    cpid = LifeORama.create_cell(:alive)
    send(cpid, {:set_state, self, :dead})
    send(cpid, {:get_state, self})
    assert_receive :alive
    LifeORama.destroy_cell(cpid)
  end
end
