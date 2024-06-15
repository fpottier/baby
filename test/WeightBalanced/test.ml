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

(* A Monolith combinator for arrays. *)

let constructible_array spec =
  map_outof
    Array.of_list
    (Array.of_list, constant "Array.of_list")
    (list spec)

let deconstructible_array spec =
  map_into
    Array.to_list
    (Array.to_list, constant "Array.to_list")
    (list spec)

let array spec =
  ifpol
    (constructible_array spec)
    (deconstructible_array spec)

(* -------------------------------------------------------------------------- *)

(* A Monolith combinator for triples. *)

let unnest (x, (y, z)) =
  (x, y, z)

let nest (x, y, z) =
  (x, (y, z))

let constructible_triple spec1 spec2 spec3 =
  map_outof
    unnest
    (unnest, constant "unnest")
    (spec1 *** (spec2 *** spec3))

let deconstructible_triple spec1 spec2 spec3 =
  map_into
    nest
    (nest, constant "nest")
    (spec1 *** (spec2 *** spec3))

let triple spec1 spec2 spec3 =
  ifpol
    (constructible_triple spec1 spec2 spec3)
    (deconstructible_triple spec1 spec2 spec3)

(* -------------------------------------------------------------------------- *)

(* The abstract type [set]. *)

(* This type is equipped with a well-formedness check,
   which ignores the model (the reference side). *)

let check _model =
  C.check,
  constant "check"

let set =
  declare_abstract_type ~check ()

(* -------------------------------------------------------------------------- *)

(* The abstract type [enum] *)

let enum =
  declare_abstract_type ()

(* -------------------------------------------------------------------------- *)

(* The concrete type [elt]. *)

(* Our elements are integer values, drawn from a fixed interval. *)

let range =
  1 lsl 8

let elt =
  semi_open_interval (-range) (range-1)

(* -------------------------------------------------------------------------- *)

(* An element can also be drawn out of a set. *)

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

(* -------------------------------------------------------------------------- *)

(* The abstract type of sequences of elements. *)

let seq_elt =
  declare_seq elt

(* -------------------------------------------------------------------------- *)

(* An operation of type [elt -> set -> _], such as [add], [remove], [mem],
   [find], etc., is tested both with arbitrary elements and specifically
   with elements that are members of the set. *)

(* [result] is the result type; [name], [r], [c] are the name of the function,
   the reference implementation, and the candidate implementation. *)

let flip f x y =
  f y x

let declare_elt_set_function result name r c =
  let spec = elt ^> set ^> result in
  declare name spec r c;
  let spec = set ^>> fun s -> (inhabits s) ^> result in
  declare ("flip " ^ name) spec (flip r) (flip c)

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  (* Section 1: constructing sets. *)

  let spec = set in
  declare "empty" spec R.empty C.empty;

  let spec = elt ^> set in
  declare "singleton" spec R.singleton C.singleton;

  declare_elt_set_function set "add" R.add C.add;

  declare_elt_set_function set "remove" R.remove C.remove;

  let spec = set ^!> set in
  declare "remove_min_elt" spec R.remove_min_elt C.remove_min_elt;

  let spec = set ^!> set in
  declare "remove_max_elt" spec R.remove_max_elt C.remove_max_elt;

  let spec = set ^> set ^> set in
  declare "union" spec R.union C.union;

  let spec = set ^> set ^> set in
  declare "inter" spec R.inter C.inter;

  let spec = set ^> set ^> set in
  declare "diff" spec R.diff C.diff;

  let spec = set ^> set ^> set in
  declare "xor" spec R.xor C.xor;

  declare_elt_set_function (triple set bool set) "split" R.split C.split;

  (* Section 2: querying sets. *)

  let spec = set ^> bool in
  declare "is_empty" spec R.is_empty C.is_empty;

  let spec = set ^!> elt in
  declare "min_elt" spec R.min_elt C.min_elt;

  let spec = set ^!> option elt in
  declare "min_elt_opt" spec R.min_elt_opt C.min_elt_opt;

  let spec = set ^!> elt in
  declare "max_elt" spec R.max_elt C.max_elt;

  let spec = set ^!> option elt in
  declare "max_elt_opt" spec R.max_elt_opt C.max_elt_opt;

  (* not tested: [choose], [choose_opt] *)

  declare_elt_set_function bool "mem" R.mem C.mem;

  (* [find] can raise an exception. *)
  let spec = elt ^> set ^!> elt in
  declare "find" spec R.find C.find;

  declare_elt_set_function (option elt) "find_opt" R.find_opt C.find_opt;

  let spec = set ^> set ^> bool in
  declare "disjoint" spec R.disjoint C.disjoint;

  let spec = set ^> set ^> bool in
  declare "subset" spec R.subset C.subset;

  let spec = set ^> set ^> bool in
  declare "equal" spec R.equal C.equal;

  let spec = set ^> set ^> int in
  declare "compare" spec R.compare C.compare;

  let spec = set ^> int in
  declare "cardinal" spec R.cardinal C.cardinal;

  (* Section 3: conversions to and from sets. *)

  (* [of_list] is important in this test because it offers a cheap way
     of creating nontrivial sets. It consumes just one unit of fuel. *)
  let spec = list elt ^> set in
  declare "of_list" spec R.of_list C.of_list;

  let spec = set ^> list elt in
  declare "elements" spec R.elements C.elements;

  (* [to_list] is a synonym for [elements]. *)

  let spec = array elt ^> set in
  declare "of_array" spec R.of_array C.of_array;

  let spec = set ^> array elt in
  declare "to_array" spec R.to_array C.to_array;

  let spec = seq_elt ^> set in
  declare "of_seq" spec R.of_seq C.of_seq;

  let spec = seq_elt ^> set ^> set in
  declare "add_seq" spec R.add_seq C.add_seq;

  let spec = set ^> seq_elt in
  declare "to_seq" spec R.to_seq C.to_seq;

  let spec = elt ^> set ^> seq_elt in
  declare "to_seq_from" spec R.to_seq_from C.to_seq_from;

  let spec = set ^> seq_elt in
  declare "to_rev_seq" spec R.to_rev_seq C.to_rev_seq;

  (* Section 4: iterating, searching, transforming sets. *)

  (* not tested: [find_first], [find_first_opt] *)
  (* not tested: [find_last], [find_last_opt] *)





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
    dprintf "          let unnest (x, (y, z)) = (x, y, z);;\n";
    dprintf "          let nest (x, y, z) = (x, (y, z));;\n";
    ()
  in
  let fuel = 16 in
  main ~prologue fuel
