(**[compress equal a] compresses the array [a], in place, so as to remove
   adjacent duplicate elements. It returns the number of unique elements that
   were found; these elements now form an initial segment of the array [a].
   The function [equal] is used to test elements for equality. *)
val compress : ('a -> 'a -> bool) -> 'a array -> int

type run =
  int * int

(**[foreach_increasing_run compare yield accu a] iterates over the consecutive
   increasing runs of elements of the array [a]. The total ordering function
   [compare] is used to identify increasing runs. Each run is described as a
   semi-open interval [(i, j)] of indices into the array [a]. Each run is
   passed in turn to the function [yield]. An accumulator, whose initial value
   is [accu], is threaded through the sequence of calls to [yield]. *)
val foreach_increasing_run :
  ('a -> 'a -> int) ->
  ('b -> run -> 'b) ->
  'b -> 'a array -> 'b

(**[increasing_run compare a] returns a list of the consecutive increasing
   runs of elements of the array [a]. The total ordering function [compare] is
   used to identify increasing runs. Each run is described as a semi-open
   interval [(i, j)] of indices into the array [a]. *)
val increasing_runs :
  ('a -> 'a -> int) ->
  'a array -> run list
