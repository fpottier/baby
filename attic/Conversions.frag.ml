(* -------------------------------------------------------------------------- *)

(* [of_array_destructive a] converts the array, in time O(n.log n), to
   a set. The array is modified (it is sorted and compressed). *)

let of_array_destructive a =
  (* Sort the array. *)
  Array.sort E.compare a;
  (* Remove duplicate elements. The unique elements remain in the
     slice of index 0 to index [n]. *)
  let equal x1 x2 = E.compare x1 x2 = 0 in
  let n = ArrayExtra.compress equal a in
  (* Convert this array slice to a tree. *)
  of_sorted_unique_array_slice a 0 n

(* -------------------------------------------------------------------------- *)

(* [of_array] converts an array, in time O(n.log n), to a set. *)

let of_array a =
  a |> Array.copy |> of_array_destructive
