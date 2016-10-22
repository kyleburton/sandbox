defmodule LifeORama do
    def create_cell(state) do
        spawn fn -> LifeORama.Cell.loop({state, nil, []}) end
    end

    def destroy_cell(cpid) do
        send(cpid, {:stop, self})
    end
end

defmodule LifeORama.Cell do
    def loop({curr_state, next_state, neighbors}) do
        receive do
            {:set_state, caller, newval} ->
              send(caller, :ok)
              loop({newval, next_state, neighbors})
            {:get_state, caller} ->
              send(caller, curr_state)
              loop({curr_state, next_state, neighbors})
            {:add_neighbor, caller, neighbor} ->
              send(caller, :ok)
              loop({curr_state, next_state, [neighbor]++neighbors})
            {:num_neighbors, caller, neighbor} ->
              send(caller, Enum.count(neighbors))
              loop({curr_state, next_state, neighbors})
            # {:num_live_neighbors, caller, neighbor} ->
            #     send(caller, Enum.reduce(neighbors, fn(acc, item) -> acc + 1 end))
            #   loop({curr_state, next_state, [neighbor]++neighbors]})
            # {:num_dead_neighbors, caller, neighbor} ->
            #   send(caller, :ok)
            #   loop({curr_state, next_state, [neighbor]++neighbors]})
            {:stop, caller} ->
              send(caller, :ok)
        end
    end
end
