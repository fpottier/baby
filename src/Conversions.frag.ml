(******************************************************************************)
(*                                                                            *)
(*                                    Baby                                    *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

(* -------------------------------------------------------------------------- *)

(* [elements] converts a set, in linear time, to a sorted list. *)

let rec to_list (t : TREE) (k : ELT list) : ELT list =
  match VIEW(t) with
  | LEAF ->
      k
  | NODE(l, v, r)
      to_list l (v :: to_list r k)

let[@inline] to_list (t : TREE) : ELT list =
  to_list t []

(* -------------------------------------------------------------------------- *)

(* [to_seq] constructs the increasing sequence of the elements of the
   tree [t]. *)

let to_seq (t : TREE) : ELT Seq.t =
  fun () -> Enum.(to_seq_node (enum t))

(* [to_seq_from low t] constructs the increasing sequence of the
   elements [x] of the tree [t] such that [low <= x] holds. *)

let to_seq_from (low : key) (t : TREE) : ELT Seq.t =
  fun () -> Enum.(to_seq_node (from_enum low t))

(* [to_rev_seq] constructs the decreasing sequence of the elements of
   the tree [t]. *)

let to_rev_seq (t : TREE) : ELT Seq.t =
  fun () -> RevEnum.(to_seq_node (enum t))

(* -------------------------------------------------------------------------- *)

(* [to_array_slice t a i] writes the elements of the tree [t] to the
   array slice determined by the array [a] and the start index [i].
   It returns the end index of this slice. *)

let rec to_array_slice (t : TREE) a i : int =
  assert (0 <= i && i + cardinal t <= Array.length a);
  match VIEW(t) with
  | LEAF ->
      i
  | NODE(l, v, r)
      let i = to_array_slice l a i in
      a.(i) <- v;
      let i = i + 1 in
      to_array_slice r a i

(* -------------------------------------------------------------------------- *)

(* [to_array] converts a set, in linear time, to a sorted array. *)

let to_array (t : TREE) : ELT array =
  match VIEW(t) with
  | LEAF ->
      [||]
  | NODE(_, dummy, _)
      let n = cardinal t in
      let a = Array.make n dummy in
      let j = to_array_slice t a 0 in
      assert (n = j);
      a

(* -------------------------------------------------------------------------- *)

(* [of_sorted_unique_array_slice] is available as part of the [BASE_SET]
   or [BASE_MAP] signature. *)

(* -------------------------------------------------------------------------- *)

(* [of_sorted_unique_array a] requires the array [a] to be sorted and to
   contain no duplicate elements. It converts this array, in linear time,
   to a set. *)

(* Because this function is unsafe (the user can provide an array that
   is not sorted and/or that has duplicate elements), it is disabled.
   [to_array] (below) is safe and is almost just as fast.

let[@inline] of_sorted_unique_array a =
  of_sorted_unique_array_slice a 0 (Array.length a)

 *)

(* -------------------------------------------------------------------------- *)

(* [of_array] converts an array to a set. This algorithm is adaptive. If the
   array is sorted, then its time complexity is O(n). If the array is not
   sorted, then its time complexity gradually degenerates to O(n.log n). *)

(* Each run of consecutive increasing elements is converted to a set, in
   linear time in the length of this run. Then, the union of these sets
   is computed. *)

(* [override] is [union] with priority to the right-hand argument. So,
   in the map variant, if a key appears twice in the list, the rightmost
   binding takes over. *)

let of_array a =
  let yield accu i j = override accu (of_sorted_unique_array_slice a i j) in
  ArrayExtra.foreach_increasing_run compare_elts yield empty a

(* -------------------------------------------------------------------------- *)

(* [of_list] converts a list to a set. It is adaptive. *)

(* OCaml's Set library constructs a sorted list (using [List.sort_uniq]) and
   converts it directly to a tree. Instead, we convert the list to an array
   and use [of_array]. On random data, our approach seems slower by about 50%.
   On sorted data, our approach can be 2x or 3x faster. One drawback of our
   approach is that it requires linear auxiliary storage. *)

let of_list xs =
  xs |> Array.of_list |> of_array

(* -------------------------------------------------------------------------- *)

(* [of_seq] converts a sequence to a set. It is adaptive. *)

(* [of_seq] in OCaml's Set library is implemented using [add_seq], which
   itself is naively implemented by iterated insertions, so its complexity
   is O(n.log n), whereas it could be O(n). *)

let of_seq xs =
  xs |> Array.of_seq |> of_array

(* [add_seq] inserts a sequence into a set. *)

let add_seq xs t =
  override t (of_seq xs)
