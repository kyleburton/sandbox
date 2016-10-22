defmodule LifeORama do
    def create_cell(state) do
        spawn LifeORama.Cell.loop({state, []})
    end

    def destroy_cell(cpid) do
        send(cpid, {:stop, self})
    end
end

defmodule LifeORama.Cell do
    def loop({curr_state, neighbors}) do
        receive do
            {:set_state, caller, newval} ->
              send(caller, :ok)
              loop({newval, neighbors})
            {:get_state, caller} ->
              send(caller, curr_state)
              loop({curr_state, neighbors})
            {:stop, caller} ->
              send(caller, :ok)
        end
    end
end
