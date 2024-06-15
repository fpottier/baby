(**************************************************************************)
(*                                                                        *)
(*                                  Bistro                                *)
(*                                                                        *)
(*                      François Pottier, Inria Paris                     *)
(*                                                                        *)
(*      Copyright 2024--2024 Inria. All rights reserved. This file is     *)
(*      distributed under the terms of the GNU Library General Public     *)
(*      License, with an exception, as described in the file LICENSE.     *)
(*                                                                        *)
(**************************************************************************)

open Monolith

(* This is the reference implementation. *)
module R = Reference.Make(Int)

(* The candidate implementation is supplied by a separate library,
   which is either [Weight_candidate] or [Height_candidate]. Both
   of these libraries offer a module named [Candidate]. *)
module C = Candidate

(* -------------------------------------------------------------------------- *)

(* The abstract type [set]. *)

(* It is equipped with a well-formedness check,
   which ignores the model (the reference side). *)

let check _model =
  C.check,
  constant "check"

let set =
  declare_abstract_type ~check ()

(* The abstract type [enum] *)

let enum =
  declare_abstract_type ()

(* We draw random integer keys. *)

let range =
  1 lsl 8

let elt =
  semi_open_interval (-range) (range-1)

(* We can also draw an inhabitant out of a set. *)

let inhabits s =
  int_within @@ fun () ->
    let open R in
    let open Gen in
    if is_empty s then reject() else
    let x = min_elt s
    and y = max_elt s in
    let k = x + Random.int (y - x + 1) in
    let _, b, r = split k s in
    let z = if b then k else min_elt r in
    assert (mem z s);
    z

(* Deconstructing a triple. *)

let nest (x, y, z) =
  (x, (y, z))

let triple spec1 spec2 spec3 =
  map_into
    nest
    (nest, constant "nest")
    (spec1 *** (spec2 *** spec3))

(* Generating arrays. *)

let array_elt =
  easily_constructible
    Gen.(array (int range) (semi_open_interval (-range) (range-1)))
    Print.(array int)

let sorted_unique_array compare n element () =
  Gen.list n element ()
  |> List.sort_uniq compare
  |> Array.of_list

let sorted_unique_array_elt =
  easily_constructible
    Gen.(sorted_unique_array Int.compare (int 16) (semi_open_interval (-16) (15)))
    Print.(array int)

(* Generating or consuming sequences of elts. *)

let seq_elt =
  declare_seq elt

(* Exchanging two arguments. *)

let flip f x y =
  f y x

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = set in
  declare "empty" spec R.empty C.empty;

  let spec = elt ^> set in
  declare "singleton" spec R.singleton C.singleton;

  (* not tested: [is_empty] *)

  let spec = set ^!> elt in
  declare "min_elt" spec R.min_elt C.min_elt;

  let spec = set ^!> option elt in
  declare "min_elt_opt" spec R.min_elt_opt C.min_elt_opt;

  let spec = set ^!> elt in
  declare "max_elt" spec R.max_elt C.max_elt;

  let spec = set ^!> option elt in
  declare "max_elt_opt" spec R.max_elt_opt C.max_elt_opt;

  (* not tested: [choose], [choose_opt] *)
  (* not tested: [find_first], [find_first_opt] *)
  (* not tested: [find_last], [find_last_opt] *)

  let spec = elt ^> set ^> bool in
  declare "mem" spec R.mem C.mem;

  let spec = elt ^> set ^!> elt in
  declare "find" spec R.find C.find;

  let spec = elt ^> set ^> option elt in
  declare "find_opt" spec R.find_opt C.find_opt;

  let spec = elt ^> set ^> set in
  declare "add" spec R.add C.add;

  let spec = elt ^> set ^> set in
  declare "remove" spec R.remove C.remove;

  (* TODO do the same for [mem], [find], [add], etc. *)
  (* Specifically remove a elt that is in the set. *)
  let spec = set ^>> fun s -> (inhabits s) ^> set in
  declare "flip remove" spec (flip R.remove) (flip C.remove);

  let spec = set ^!> set in
  declare "remove_min_elt" spec R.remove_min_elt C.remove_min_elt;

  let spec = set ^!> set in
  declare "remove_max_elt" spec R.remove_max_elt C.remove_max_elt;

  let spec = set ^> set ^> set in
  declare "union" spec R.union C.union;

  let spec = set ^> set ^> set in
  declare "inter" spec R.inter C.inter;

  let spec = set ^> set ^> bool in
  declare "disjoint" spec R.disjoint C.disjoint;

  let spec = set ^> set ^> set in
  declare "diff" spec R.diff C.diff;

  let spec = set ^> set ^> bool in
  declare "subset" spec R.subset C.subset;

  let spec = set ^> set ^> set in
  declare "xor" spec R.xor C.xor;

  let spec = set ^> set ^> int in
  declare "compare" spec R.compare C.compare;

  (* [split] is not tested. *)

  let spec = set ^> list elt in
  declare "elements" spec R.elements C.elements;

  let spec = seq_elt ^> set in
  declare "of_seq" spec R.of_seq C.of_seq;

  let spec = set ^> seq_elt in
  declare "to_seq" spec R.to_seq C.to_seq;

  let spec = elt ^> set ^> seq_elt in
  declare "to_seq_from" spec R.to_seq_from C.to_seq_from;

  let spec = set ^> seq_elt in
  declare "to_rev_seq" spec R.to_rev_seq C.to_rev_seq;

  let spec = seq_elt ^> set ^> set in
  declare "add_seq" spec R.add_seq C.add_seq;

  (* [of_list] is important in this test because it offers a cheap way
     of creating nontrivial sets. It consumes just one unit of fuel. *)
  let spec = list elt ^> set in
  declare "of_list" spec R.of_list C.of_list;

  let spec = array_elt ^> set in
  declare "of_array" spec R.of_array C.of_array;

  let spec = sorted_unique_array_elt ^> set in
  declare "of_sorted_unique_array" spec R.of_array C.of_array;

  let spec = set ^> list elt in
  declare "(fun s -> Array.to_list (to_array s))" spec
    R.elements
    (fun s -> Array.to_list (C.to_array s));

  let spec = set ^> int in
  declare "cardinal" spec R.cardinal C.cardinal;

  if C.has_random_access_functions then begin

    let spec = set ^>> fun s -> lt (R.cardinal s) ^> elt in
    declare "get" spec R.get C.get;

    let spec = elt ^> set ^!> int in
    declare "index" spec R.index C.index;

    (* Specifically query a value that is in the set. *)
    let spec = set ^>> fun s -> (inhabits s) ^> int in
    declare "flip index" spec (flip R.index) (flip C.index);

    let spec = set ^>> fun s -> le (R.cardinal s) ^> set *** set in
    declare "cut" spec R.cut C.cut;

    let spec = set ^>> fun s -> lt (R.cardinal s) ^> triple set elt set in
    declare "cut_and_get" spec R.cut_and_get C.cut_and_get;

  end;

  (* not tested: [map] *)
  (* TODO test [filter_map] with identity (test physical equality),
       with monotone function, with non-monotone function *)
  (* not tested: [filter] *)
  (* TODO test [partition] *)

  (* not tested: [iter] *)
  (* not tested: [fold] *)
  (* not tested: [for_all] *)
  (* not tested: [exists] *)

  let spec = enum in
  declare "Enum.empty" spec R.Enum.empty C.Enum.empty;

  let spec = enum ^> bool in
  declare "Enum.is_empty" spec R.Enum.is_empty C.Enum.is_empty;

  let spec = set ^> enum in
  declare "Enum.enum" spec R.Enum.enum C.Enum.enum;

  let spec = elt ^> set ^> enum in
  declare "Enum.from_enum" spec R.Enum.from_enum C.Enum.from_enum;

  let spec = enum ^!> elt in
  declare "Enum.head" spec R.Enum.head C.Enum.head;

  let spec = enum ^!> enum in
  declare "Enum.tail" spec R.Enum.tail C.Enum.tail;

  let spec = enum ^> option elt in
  declare "Enum.head_opt" spec R.Enum.head_opt C.Enum.head_opt;

  let spec = enum ^> option enum in
  declare "Enum.tail_opt" spec R.Enum.tail_opt C.Enum.tail_opt;

  let spec = elt ^> enum ^> enum in
  declare "Enum.from" spec R.Enum.from C.Enum.from;

  let spec = enum ^> seq_elt in
  declare "Enum.to_seq" spec R.Enum.to_seq C.Enum.to_seq;

  let spec = enum ^> set in
  declare "Enum.elements" spec R.Enum.elements C.Enum.elements;

  let spec = enum ^> int in
  declare "Enum.length" spec R.Enum.length C.Enum.length;

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let prologue () =
    dprintf "          open %s;;\n" C.name;
    dprintf "          let flip f x y = f y x;;\n";
    dprintf "          let nest (x, y, z) = (x, (y, z));;\n";
    ()
  in
  let fuel = 16 in
  main ~prologue fuel
