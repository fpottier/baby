(* [of_sorted_unique_array_slice a i j] requires the array slice defined by
   array [a], start index [i], and end index [j] to be sorted and to contain
   no duplicate elements. It converts this array slice, in linear time, to a
   tree. *)

(* Making this function part of the signatures [BASE_SET] and [BASE_MAP]
   removes the need to export [doubleton], [tripleton], etc. *)

let rec of_sorted_unique_array_slice a i j =
  assert (0 <= i && i <= j && j <= Array.length a);
  let n = j - i in
  match n with
  | 0 ->
      TLeaf
  | 1 ->
      let x = a.(i) in
      singleton x
  | 2 ->
      let x = a.(i)
      and y = a.(i+1) in
      doubleton x y
  | 3 ->
      let x = a.(i)
      and y = a.(i+1)
      and z = a.(i+2) in
      tripleton x y z
  | _ ->
      let k = i + n/2 in
      let l = of_sorted_unique_array_slice a i k
      and v = a.(k)
      and r = of_sorted_unique_array_slice a (k+1) j in
      (* Here, we know that the trees [l] and [r] have balanced weights,
         and we assume that this implies [siblings l r]. *)
      join_siblings l v r
