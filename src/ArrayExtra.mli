(**[compress equal a] compresses the array [a], in place, so as to remove
   adjacent duplicate elements. It returns the number of unique elements that
   were found; these elements now form an initial segment of the array [a].
   The function [equal] is used to test elements for equality. *)
val compress : ('a -> 'a -> bool) -> 'a array -> int
