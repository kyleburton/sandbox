-record(heap, {
          cmp_fn :: fun(),
          buff = array:new() :: arrays:array()
}).


