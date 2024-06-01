(* TODO
let of_list xs =
  (* Convert the list to an array. *)
  let a = Array.of_list xs in
  (* Sort the array. *)
  Array.sort E.compare a;
  (* Remove duplicate elements. The unique elements remain in the
     slice of index 0 to index [n]. *)
  let equal x1 x2 = E.compare x1 x2 = 0 in
  let n = ArrayExtra.compress equal a in
  (* Convert this array slice to a tree. *)
  of_sorted_unique_array_slice n a
 *)

(* A naïve generic [of_list], whose complexity is O(n.log n). *)

let of_list xs =
  List.fold_left (fun s x -> add x s) empty xs
