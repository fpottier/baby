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

open Monolith

(* This is the reference implementation. *)
module R = Reference.Make(Int)

(* This is the candidate implementation. *)
module C = struct

  include Candidate

  (* Wrap some of the candidate functions with extra runtime checks. *)

  (* [add] guarantees that if the result is logically equal to [m]
     then it is physically equal to [m]. *)

  let[@inline] add k v m =
    let result = add k v m in
    (* TODO if equal (==) result m then assert (result == m); *)
    if has_binding k v m then assert (result == m);
    result

end

(* -------------------------------------------------------------------------- *)

(* The abstract type [map]. *)

(* This type is equipped with a well-formedness check,
   which ignores the model (the reference side). *)

let check _model =
  C.check,
  constant "check"

let map =
  declare_abstract_type ~check ()

(* -------------------------------------------------------------------------- *)

(* The concrete type [key]. *)

(* Our keys are integer values, drawn from a fixed interval. *)

let range =
  1 lsl 8

let key =
  semi_open_interval (-range) (range-1)

(* -------------------------------------------------------------------------- *)

(* The concrete type [value]. *)

(* Our values are integer values, drawn from a fixed interval. *)

let range =
  1 lsl 8

let value =
  semi_open_interval (-range) (range-1)

(* -------------------------------------------------------------------------- *)

(* An operation whose arguments include a key and a map, such as [add],
   [remove], [mem], [find], etc., is tested both with arbitrary keys and
   specifically with keys that are in the map. *)

let flip f x y =
  f y x

let flip3 f z x y =
  f x y z

(* A key can be drawn out of a map. *)

let inhabits m =
  int_within @@ fun () ->
    let open R in
    let open Gen in
    if is_empty m then reject() else
    let x, _ = min_binding m
    and y, _ = max_binding m in
    let k = x + Random.int (y - x + 1) in
    let _, ov, r = split k m in
    let z = if ov <> None then k else fst (min_binding r) in
    assert (mem z m);
    z

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  (* Section 1: constructing maps. *)

  let spec = map in
  declare "empty" spec R.empty C.empty;

  let spec = key ^> value ^> map in
  declare "singleton" spec R.singleton C.singleton;

  let spec = key ^> value ^> map ^> map in
  declare "add" spec R.add C.add;
  let spec = map ^>> fun m -> (inhabits m) ^> value ^> map in
  declare "flip3 add" spec (flip3 R.add) (flip3 C.add);

  let spec = key ^> map ^> map in
  declare "remove" spec R.remove C.remove;
  let spec = map ^>> fun m -> (inhabits m) ^> map in
  declare "flip remove" spec (flip R.remove) (flip C.remove);

(* TODO

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

*)

  (* Section 2: querying maps. *)

  let spec = map ^> bool in
  declare "is_empty" spec R.is_empty C.is_empty;

(* TODO

  let spec = set ^!> elt in
  declare "min_elt" spec R.min_elt C.min_elt;

  let spec = set ^!> option elt in
  declare "min_elt_opt" spec R.min_elt_opt C.min_elt_opt;

  let spec = set ^!> elt in
  declare "max_elt" spec R.max_elt C.max_elt;

  let spec = set ^!> option elt in
  declare "max_elt_opt" spec R.max_elt_opt C.max_elt_opt;

  (* not tested: [choose], [choose_opt] *)

*)

  let spec = key ^> map ^> bool in
  declare "mem" spec R.mem C.mem;
  let spec = map ^>> fun m -> (inhabits m) ^> bool in
  declare "flip mem" spec (flip R.mem) (flip C.mem);

  let spec = key ^> map ^!> value in
  declare "find" spec R.find C.find;
  let spec = map ^>> fun m -> (inhabits m) ^!> value in
  declare "flip find" spec (flip R.find) (flip C.find);

(* TODO

  declare_elt_set_function (option elt) "find_opt" R.find_opt C.find_opt;

  let spec = set ^> set ^> bool in
  declare "disjoint" spec R.disjoint C.disjoint;

  let spec = set ^> set ^> bool in
  declare "subset" spec R.subset C.subset;

  let spec = set ^> set ^> bool in
  declare "equal" spec R.equal C.equal;

  let spec = set ^> set ^> int in
  declare "compare" spec R.compare C.compare;

*)

  let spec = map ^> int in
  declare "cardinal" spec R.cardinal C.cardinal;

(* TODO

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

  (* not tested: [iter] *)
  (* not tested: [fold] *)
  (* not tested: [for_all] *)
  (* not tested: [exists] *)

  (* [find_first] is applied specifically to the function [(<=) 0]. *)
  let spec = set ^!> elt in
  declare "find_first ((<=) 0)" spec
    (R.find_first ((<=) 0))
    (C.find_first ((<=) 0));

  let spec = set ^> option elt in
  declare "find_first_opt ((<=) 0)" spec
    (R.find_first_opt ((<=) 0))
    (C.find_first_opt ((<=) 0));

  let spec = set ^!> elt in
  declare "find_last ((>=) 0)" spec
    (R.find_last ((>=) 0))
    (C.find_last ((>=) 0));

  let spec = set ^> option elt in
  declare "find_last_opt ((>=) 0)" spec
    (R.find_last_opt ((>=) 0))
    (C.find_last_opt ((>=) 0));

  (* [map] is tested in three different scenarios: applied to the identity;
     applied to a monotonically increasing function; applied to an arbitrary
     function. In the first scenario, we check that sharing is preserved. *)

  let spec = set ^> set in
  let f = Fun.id in
  declare "map Fun.id" spec (R.map f) (must_preserve_sharing (C.map f));

  let spec = set ^> set in
  let f = succ in
  declare "map succ" spec (R.map f) (C.map f);

  let spec = set ^> set in
  let f = Int.neg in
  declare "map Int.neg" spec (R.map f) (C.map f);

  let spec = set ^> set in
  let p _ = true in
  declare "filter (fun _ -> true)" spec
    (R.filter p) (must_preserve_sharing (C.filter p));

  let spec = set ^> set in
  let p x = x mod 2 = 0 in
  declare "filter (fun x -> x mod 2 = 0)" spec
    (R.filter p) (C.filter p);

  let spec = set ^> set in
  let f x = Some x in
  declare "filter_map (fun x -> Some x)"
    spec (R.filter_map f) (must_preserve_sharing (C.filter_map f));

  let spec = set ^> set in
  let f x = if x mod 2 = 0 then Some (x + 1) else None in
  declare
    "filter_map (fun x -> if x mod 2 = 0 then Some (x + 1) else None) "
    spec (R.filter_map f) (C.filter_map f);

  let spec = set ^> set in
  let f x = if x mod 2 = 0 then Some (-x) else None in
  declare
    "filter_map (fun x -> if x mod 2 = 0 then Some (-x) else None) "
    spec (R.filter_map f) (C.filter_map f);

  let spec = set ^> set *** set in
  let p x = x mod 2 = 0 in
  declare "partition (fun x -> x mod 2 = 0)" spec
    (R.partition p) (C.partition p);

  (* Section 5: random access. *)

  if C.has_random_access_functions then begin

    let spec = set ^>> fun s -> lt (R.cardinal s) ^> elt in
    declare "get" spec R.get C.get;

    declare_elt_set_function_may_fail_if_absent int "index" R.index C.index;

    let spec = set ^>> fun s -> le (R.cardinal s) ^> set *** set in
    declare "cut" spec R.cut C.cut;

    let spec = set ^>> fun s -> lt (R.cardinal s) ^> triple set elt set in
    declare "cut_and_get" spec R.cut_and_get C.cut_and_get;

  end;

  (* Section 6: enumerations. *)

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

*)

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let prologue () =
    (* dprintf "          open %s;;\n" C.name; TODO *)
    dprintf "          let flip f x y = f y x;;\n";
    dprintf "          let flip3 f z x y = f x y z;;\n";
    dprintf "          let unnest (x, (y, z)) = (x, y, z);;\n";
    dprintf "          let nest (x, y, z) = (x, (y, z));;\n";
    ()
  in
  let fuel = 16 in
  main ~prologue fuel
