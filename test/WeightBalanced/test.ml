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
open Helpers

(* -------------------------------------------------------------------------- *)

(* In order to detect an unintentional use of [Stdlib.compare], we use
   a nonstandard ordering on keys: the reverse of the usual ordering. *)

(* This decision appears here and is repeated in the two candidate
   implementations (ugh). It also appears in the implementation of
   the functions [inhabits_set] and [inhabits_map]. *)

let reversed = true

let exchange (x, y) =
  if reversed then (y, x) else (x, y)

module Key = struct
  type t = int
  let compare x y = - (Int.compare x y)
end

let increasing, decreasing =
  exchange (increasing, decreasing)

(* -------------------------------------------------------------------------- *)

(* This is the reference implementation. *)
module R = Reference.Make(Key)

(* The candidate implementation is supplied by a separate library,
   which is either [Weight_candidate] or [Height_candidate]. Both
   of these libraries offer a module named [Candidate]. *)
module C = struct

  module Set = struct

    include Candidate.Set

    (* Wrap some of the candidate functions with extra runtime checks. *)

    (* [diff] guarantees that if the result is logically equal to [t1]
       then it is physically equal to [t1]. This holds regardless of
       which balancing criterion is used. *)

    let diff t1 t2 =
      let result = diff t1 t2 in
      if equal result t1 then assert (result == t1);
      result

    (* [add] and [remove] offers similar guarantees. *)

    let add x t =
      let result = add x t in
      if mem x t then assert (result == t);
      result

    let remove x t =
      let result = remove x t in
      if not (mem x t) then assert (result == t);
      result

    (* [union] and [inter] guarantee that if the result is logically equal to
       one of the arguments then it is physically equal to one of the
       arguments. *)

    (* This guarantee holds for weight-balanced trees, but not for
       height-balanced trees; indeed, a reliable way of comparing the
       cardinals of the two sets is needed. *)

    let union t1 t2 =
      let result = union t1 t2 in
      if Candidate.weight_balanced && (equal result t1 || equal result t2) then
        assert (result == t1 || result == t2);
      result

    let inter t1 t2 =
      let result = inter t1 t2 in
      if Candidate.weight_balanced && (equal result t1 || equal result t2) then
        assert (result == t1 || result == t2);
      result

  end

  module Map = struct

    include Candidate.Map

    let oeqeq (od : 'a option) (v' : 'a) =
      match od with
      | None ->
          false
      | Some v ->
          v == v'

    (* Wrap some of the candidate functions with extra runtime checks. *)

    (* [add] guarantees that if the new value is physically equal to the
       previous value then the new map is physically equal to the old map. *)

    let add k v m =
      let od = find_opt k m in
      let result = add k v m in
      if oeqeq od v then assert (result == m);
      result

    (* [remove x m] guarantees that if the result is equal to [m]
       then it is physically equal to [m]. *)

    let remove x m =
      let result = remove x m in
      if equal (=) result m then assert (result == m);
      result

    (* [diff] guarantees that if the result is logically equal to [t1]
       then it is physically equal to [t1]. *)

    let diff t1 t2 =
      let result = diff t1 t2 in
      if equal (=) result t1 then assert (result == t1);
      result

    (* [union] claims to be a special case of [merge]. *)

    let union_via_merge f m1 m2 =
      let f' key ov1 ov2 =
        match ov1, ov2 with
        | None, None ->
            None
        | Some v, None
        | None, Some v ->
            Some v
        | Some v1, Some v2 ->
            f key v1 v2
      in
      merge f' m1 m2

    let union f m1 m2 =
      let obtained = union f m1 m2 in
      let expected = union_via_merge f m1 m2 in
      assert (equal (=) obtained expected);
      obtained

    (* [inter] claims to be a special case of [merge]. *)

    let inter_via_merge f m1 m2 =
      let f' key ov1 ov2 =
        match ov1, ov2 with
        | Some v1, Some v2 ->
            f key v1 v2
        | _, _ ->
            None
      in
      merge f' m1 m2

    let inter f m1 m2 =
      let obtained = inter f m1 m2 in
      let expected = inter_via_merge f m1 m2 in
      assert (equal (=) obtained expected);
      obtained

    (* [diff] claims to be a special case of [merge]. *)

    let diff_via_merge m1 m2 =
      let f _key ov1 ov2 =
        match ov2 with
        | None ->
            ov1
        | Some _ ->
            None
      in
      merge f m1 m2

    let diff m1 m2 =
      let obtained = diff m1 m2 in
      let expected = diff_via_merge m1 m2 in
      assert (equal (=) obtained expected);
      obtained

    (* [xor] claims to be a special case of [merge]. *)

    let xor_via_merge m1 m2 =
      let f _key ov1 ov2 =
        match ov1, ov2 with
        | Some v, None
        | None, Some v ->
            Some v
        | Some _, Some _
        | None, None ->
            None
      in
      merge f m1 m2

    let xor m1 m2 =
      let obtained = xor m1 m2 in
      let expected = xor_via_merge m1 m2 in
      assert (equal (=) obtained expected);
      obtained

  end

  let domain = Candidate.domain
  let lift = Candidate.lift

end

(* -------------------------------------------------------------------------- *)

(* The abstract type [set]. *)

(* This type is equipped with a well-formedness check,
   which ignores the model (the reference side). *)

let set =
  let check _model = C.Set.check, constant "Set.check" in
  declare_abstract_type ~check ()

(* -------------------------------------------------------------------------- *)

(* The abstract type [map]. *)

(* This type is equipped with a well-formedness check,
   which ignores the model (the reference side). *)

let map =
  let check _model = C.Map.check, constant "Map.check" in
  declare_abstract_type ~check ()

(* -------------------------------------------------------------------------- *)

(* The concrete type [elt]. *)

(* Our elements are integer values, drawn from a fixed interval. *)

let range =
  1 lsl 8

let elt =
  semi_open_interval (-range) (range-1)

let key =
  elt

(* -------------------------------------------------------------------------- *)

(* The concrete type [value]. *)

(* Our values are integer values, drawn from a fixed interval. *)

let range =
  1 lsl 8

let value =
  semi_open_interval (-range) (range-1)

(* -------------------------------------------------------------------------- *)

(* The concrete type [binding] is defined as a key-value pair. *)

(* It is both constructible and deconstructible. *)

let binding =
  key *** value

(* -------------------------------------------------------------------------- *)

(* An element can be drawn out of a set. *)

(* This code is a hack. It is based on the reference implementation, which
   does not support random access. We pick a random key [k] between the
   minimum and maximum keys. If [k] exists in the tree, we return [k].
   Otherwise, we return the next key; this is done by splitting the tree
   based on [k] and returning the minimum key of the right-hand subtree,
   which in this case cannot be empty. *)

let inhabits_set s =
  int_within @@ fun () ->
    let open R.Set in
    let open Gen in
    if is_empty s then reject() else
    let x, y = exchange (min_elt s, max_elt s) in
    let k = x + Random.int (y - x + 1) in
    let _, b, r = split k s in
    let z = if b then k else min_elt r in
    assert (mem z s);
    z

let inhabits_map m =
  int_within @@ fun () ->
    let open R.Map in
    let open Gen in
    if is_empty m then reject() else
    let (x, _), (y, _) = exchange (min_binding m, max_binding m) in
    let k = x + Random.int (y - x + 1) in
    if mem k m then k else
    let _, ov, r = split k m in
    assert (ov = None);
    let z = fst (min_binding r) in
    assert (mem z m);
    z

(* -------------------------------------------------------------------------- *)

(* An operation of type [elt -> set -> result], such as [add], [remove],
   [mem], [find], etc., is tested both with arbitrary elements and
   specifically with elements of the set. *)

(* [result] is the result type; [name], [r], [c] are the name of the function,
   the reference implementation, and the candidate implementation. *)

(* [flag] indicates whether this operation:
   - may fail when the key is not a member of the set, or
   - cannot fail. *)

let declare_elt_set_function flag result name r c =

  (* First, test with arbitrary keys. *)
  let spec =
    match flag with
    | `MayFailWhenAbsentKey ->
        elt ^> set ^!> result
    | `CannotFail ->
        elt ^> set ^> result
          (* One might wish to use [^!>] here as well,
             as it is an over-approximation of the true spec.
             However a limitation in Monolith (20230604) prevents
             the use of [^!>] with complex result types. *)
  in
  declare name spec r c;

  (* Second, test with keys that are drawn from the set. Exceptions are not
     allowed in this case. *)
  let spec = flip (set ^>> fun s -> (inhabits_set s) ^> result) in
  declare name spec r c

(* Like above, for operations of type [key -> map -> result]. *)

let declare_key_map_function flag result name r c =
  let spec =
    match flag with
    | `MayFailWhenAbsentKey ->
        key ^> map ^!> result
    | `CannotFail ->
        key ^> map ^> result
  in
  declare name spec r c;
  let spec = flip (map ^>> fun m -> (inhabits_map m) ^> result) in
  declare name spec r c

(* Like above, for operations of type [key -> foo -> map -> result]. *)

let declare_key_foo_map_function flag foo result name r c =
  let spec =
    match flag with
    | `MayFailWhenAbsentKey ->
        key ^> foo ^> map ^!> result
    | `CannotFail ->
        key ^> foo ^> map ^> result
  in
  declare name spec r c;
  let spec = rot3 (map ^>> fun m -> (inhabits_map m) ^> foo ^> result) in
  declare name spec r c

(* -------------------------------------------------------------------------- *)

(* Declare the operations on sets. *)

let () =

  (* Section 1: constructing sets. *)

  let spec = set in
  declare "Set.empty" spec R.Set.empty C.Set.empty;

  let spec = elt ^> set in
  declare "Set.singleton" spec R.Set.singleton C.Set.singleton;

  declare_elt_set_function `CannotFail set
    "Set.add" R.Set.add C.Set.add;

  declare_elt_set_function `CannotFail set
    "Set.remove" R.Set.remove C.Set.remove;

  let spec = set ^!> set in
  declare "Set.remove_min_elt" spec R.Set.remove_min_elt C.Set.remove_min_elt;

  let spec = set ^!> set in
  declare "Set.remove_max_elt" spec R.Set.remove_max_elt C.Set.remove_max_elt;

  let spec = set ^> set ^> set in
  declare "Set.union" spec R.Set.union C.Set.union;

  let spec = set ^> set ^> set in
  declare "Set.inter" spec R.Set.inter C.Set.inter;

  let spec = set ^> set ^> set in
  declare "Set.diff" spec R.Set.diff C.Set.diff;

  let spec = set ^> set ^> set in
  declare "Set.xor" spec R.Set.xor C.Set.xor;

  declare_elt_set_function `CannotFail (triple set bool set)
    "Set.split" R.Set.split C.Set.split;

  (* Section 2: querying sets. *)

  let spec = set ^> bool in
  declare "Set.is_empty" spec R.Set.is_empty C.Set.is_empty;

  let spec = set ^!> elt in
  declare "Set.min_elt" spec R.Set.min_elt C.Set.min_elt;

  let spec = set ^!> option elt in
  declare "Set.min_elt_opt" spec R.Set.min_elt_opt C.Set.min_elt_opt;

  let spec = set ^!> elt in
  declare "Set.max_elt" spec R.Set.max_elt C.Set.max_elt;

  let spec = set ^!> option elt in
  declare "Set.max_elt_opt" spec R.Set.max_elt_opt C.Set.max_elt_opt;

  (* not tested: [choose], [choose_opt] *)

  declare_elt_set_function `CannotFail bool
    "Set.mem" R.Set.mem C.Set.mem;

  declare_elt_set_function `MayFailWhenAbsentKey elt
    "Set.find" R.Set.find C.Set.find;

  declare_elt_set_function `CannotFail (option elt)
    "Set.find_opt" R.Set.find_opt C.Set.find_opt;

  let spec = set ^> set ^> bool in
  declare "Set.disjoint" spec R.Set.disjoint C.Set.disjoint;

  let spec = set ^> set ^> bool in
  declare "Set.subset" spec R.Set.subset C.Set.subset;

  let spec = set ^> set ^> bool in
  declare "Set.equal" spec R.Set.equal C.Set.equal;

  let spec = set ^> set ^> int in
  declare "Set.compare" spec R.Set.compare C.Set.compare;

  let spec = set ^> int in
  declare "Set.cardinal" spec R.Set.cardinal C.Set.cardinal;

  (* Section 3: conversions to and from sets. *)

  let seq_elt = declare_seq elt in

  (* [of_list] is important in this test because it offers a cheap way
     of creating nontrivial sets. It consumes just one unit of fuel. *)
  let spec = list elt ^> set in
  declare "Set.of_list" spec R.Set.of_list C.Set.of_list;

  (* [to_list] is a synonym for [elements]. *)

  let spec = set ^> list elt in
  declare "Set.elements" spec R.Set.elements C.Set.elements;

  let spec = array elt ^> set in
  declare "Set.of_array" spec R.Set.of_array C.Set.of_array;

  let spec = set ^> array elt in
  declare "Set.to_array" spec R.Set.to_array C.Set.to_array;

  let spec = seq_elt ^> set in
  declare "Set.of_seq" spec R.Set.of_seq C.Set.of_seq;

  let spec = seq_elt ^> set ^> set in
  declare "Set.add_seq" spec R.Set.add_seq C.Set.add_seq;

  let spec = set ^> seq_elt in
  declare "Set.to_seq" spec R.Set.to_seq C.Set.to_seq;

  let spec = elt ^> set ^> seq_elt in
  declare "Set.to_seq_from" spec R.Set.to_seq_from C.Set.to_seq_from;

  let spec = set ^> seq_elt in
  declare "Set.to_rev_seq" spec R.Set.to_rev_seq C.Set.to_rev_seq;

  (* Section 4: iterating, searching, transforming sets. *)

  let spec = iter (set ^> list elt) in
  declare "Set.iter" spec R.Set.iter C.Set.iter;

  let spec = fold (set ^> list elt) in
  declare "Set.fold" spec R.Set.fold C.Set.fold;

  let spec = predicate ^> set ^> bool in
  declare "Set.for_all" spec R.Set.for_all C.Set.for_all;

  let spec = predicate ^> set ^> bool in
  declare "Set.exists" spec R.Set.exists C.Set.exists;

  let spec = increasing ^> set ^!> elt in
  declare "Set.find_first" spec R.Set.find_first C.Set.find_first;

  let spec = increasing ^> set ^> option elt in
  declare "Set.find_first_opt" spec R.Set.find_first_opt C.Set.find_first_opt;

  let spec = decreasing ^> set ^!> elt in
  declare "Set.find_last" spec R.Set.find_last C.Set.find_last;

  let spec = decreasing ^> set ^> option elt in
  declare "Set.find_last_opt" spec R.Set.find_last_opt C.Set.find_last_opt;

  (* When applied to the identity function, [map] must preserve sharing. *)
  let spec = sharing (set ^> set) in
  declare "(Set.map Fun.id)" spec (R.Set.map Fun.id) (C.Set.map Fun.id);

  let spec = transformation ^> set ^> set in
  declare "Set.map" spec R.Set.map C.Set.map;

  (* When applied to [always], [filter] must preserve sharing. *)
  let spec = sharing (set ^> set) in
  declare "(Set.filter always)" spec (R.Set.filter always) (C.Set.filter always);

  let spec = predicate ^> set ^> set in
  declare "Set.filter" spec R.Set.filter C.Set.filter;

  (* When applied to [keep], [filter_map] must preserve sharing. *)
  let spec = sharing (set ^> set) in
  declare "(Set.filter_map keep)" spec (R.Set.filter_map keep) (C.Set.filter_map keep);

  let spec = otransformation ^> set ^> set in
  declare "Set.filter_map" spec R.Set.filter_map C.Set.filter_map;

  let spec = predicate ^> set ^> set *** set in
  declare "Set.partition" spec R.Set.partition C.Set.partition;

  (* Section 5: random access. *)

  if Candidate.has_random_access_functions then begin

    let spec = set ^>> fun s -> lt (R.Set.cardinal s) ^> elt in
    declare "Set.get" spec R.Set.get C.Set.get;

    declare_elt_set_function `MayFailWhenAbsentKey int
      "Set.index" R.Set.index C.Set.index;

    let spec = set ^>> fun s -> le (R.Set.cardinal s) ^> set *** set in
    declare "Set.cut" spec R.Set.cut C.Set.cut;

    let spec = set ^>> fun s -> lt (R.Set.cardinal s) ^> triple set elt set in
    declare "Set.cut_and_get" spec R.Set.cut_and_get C.Set.cut_and_get;

  end;

  (* Section 6: enumerations. *)

  let enum = declare_abstract_type () in

  let spec = enum in
  declare "Set.Enum.empty" spec R.Set.Enum.empty C.Set.Enum.empty;

  let spec = enum ^> bool in
  declare "Set.Enum.is_empty" spec R.Set.Enum.is_empty C.Set.Enum.is_empty;

  let spec = set ^> enum in
  declare "Set.Enum.enum" spec R.Set.Enum.enum C.Set.Enum.enum;

  let spec = elt ^> set ^> enum in
  declare "Set.Enum.from_enum" spec R.Set.Enum.from_enum C.Set.Enum.from_enum;

  let spec = enum ^!> elt in
  declare "Set.Enum.head" spec R.Set.Enum.head C.Set.Enum.head;

  let spec = enum ^!> enum in
  declare "Set.Enum.tail" spec R.Set.Enum.tail C.Set.Enum.tail;

  let spec = enum ^> option elt in
  declare "Set.Enum.head_opt" spec R.Set.Enum.head_opt C.Set.Enum.head_opt;

  let spec = enum ^> option enum in
  declare "Set.Enum.tail_opt" spec R.Set.Enum.tail_opt C.Set.Enum.tail_opt;

  let spec = elt ^> enum ^> enum in
  declare "Set.Enum.from" spec R.Set.Enum.from C.Set.Enum.from;

  let spec = enum ^> seq_elt in
  declare "Set.Enum.to_seq" spec R.Set.Enum.to_seq C.Set.Enum.to_seq;

  let spec = enum ^> set in
  declare "Set.Enum.elements" spec R.Set.Enum.elements C.Set.Enum.elements;

  let spec = enum ^> int in
  declare "Set.Enum.length" spec R.Set.Enum.length C.Set.Enum.length;

  ()

(* -------------------------------------------------------------------------- *)

(* Declare the operations on maps. *)

let () =

  (* Section 1: constructing maps. *)

  let spec = map in
  declare "Map.empty" spec R.Map.empty C.Map.empty;

  let spec = key ^> value ^> map in
  declare "Map.singleton" spec R.Map.singleton C.Map.singleton;

  declare_key_foo_map_function `CannotFail value map
    "Map.add" R.Map.add C.Map.add;

  declare_key_map_function `CannotFail map
    "Map.remove" R.Map.remove C.Map.remove;

  (* When applied to the identity, [update] must preserve sharing. *)
  let spec = key ^> sharing (map ^> map) in
  declare "(Fun.flip Map.update Fun.id)" spec
    (Fun.flip R.Map.update Fun.id)
    (Fun.flip C.Map.update Fun.id);

  declare_key_foo_map_function `CannotFail ootransformation map
    "Map.update" R.Map.update C.Map.update;

  (* [add_to_list] cannot be tested because it operates on int list maps,
     whereas this test code works with int maps. *)

  let spec = map ^!> map in
  declare "Map.remove_min_binding" spec R.Map.remove_min_binding C.Map.remove_min_binding;

  let spec = map ^!> map in
  declare "Map.remove_max_binding" spec R.Map.remove_max_binding C.Map.remove_max_binding;

  let spec = combination ^> map ^> map ^> map in
  declare "Map.union" spec R.Map.union C.Map.union;

  let spec = combination ^> map ^> map ^> map in
  declare "Map.inter" spec R.Map.inter C.Map.inter;

  let spec = map ^> map ^> map in
  declare "Map.diff" spec R.Map.diff C.Map.diff;

  let spec = map ^> map ^> map in
  declare "Map.xor" spec R.Map.xor C.Map.xor;

  let spec = complex_combination ^> map ^> map ^> map in
  declare "Map.merge" spec R.Map.merge C.Map.merge;

  declare_key_map_function `CannotFail (triple map (option int) map)
    "Map.split" R.Map.split C.Map.split;

  (* Section 2: querying maps. *)

  let spec = map ^> bool in
  declare "Map.is_empty" spec R.Map.is_empty C.Map.is_empty;

  let spec = map ^!> binding in
  declare "Map.min_binding" spec R.Map.min_binding C.Map.min_binding;

  let spec = map ^!> option binding in
  declare "Map.min_binding_opt" spec R.Map.min_binding_opt C.Map.min_binding_opt;

  let spec = map ^!> binding in
  declare "Map.max_binding" spec R.Map.max_binding C.Map.max_binding;

  let spec = map ^!> option binding in
  declare "Map.max_binding_opt" spec R.Map.max_binding_opt C.Map.max_binding_opt;

  (* not tested: [choose], [choose_opt] *)

  declare_key_map_function `CannotFail bool
    "Map.mem" R.Map.mem C.Map.mem;

  declare_key_map_function `MayFailWhenAbsentKey value
    "Map.find" R.Map.find C.Map.find;

  declare_key_map_function `CannotFail (option value)
    "Map.find_opt" R.Map.find_opt C.Map.find_opt;

  let spec = map ^> map ^> bool in
  declare "Map.disjoint" spec R.Map.disjoint C.Map.disjoint;

  let spec = bordering ^> map ^> map ^> bool in
  declare "Map.sub" spec R.Map.sub C.Map.sub;

  let spec = bequality ^> map ^> map ^> bool in
  declare "Map.equal" spec R.Map.equal C.Map.equal;

  let spec = iordering ^> map ^> map ^> int in
  declare "Map.compare" spec R.Map.compare C.Map.compare;

  let spec = map ^> int in
  declare "Map.cardinal" spec R.Map.cardinal C.Map.cardinal;

  (* Section 3: conversions to and from maps. *)

  let seq_binding = declare_seq binding in

  (* [of_list] is important in this test because it offers a cheap way
     of creating nontrivial maps. It consumes just one unit of fuel. *)
  let spec = list binding ^> map in
  declare "Map.of_list" spec R.Map.of_list C.Map.of_list;

  let spec = map ^> list binding in
  declare "Map.bindings" spec R.Map.bindings C.Map.bindings;

  (* [to_list] is a synonym for [bindings]. *)

  let spec = array binding ^> map in
  declare "Map.of_array" spec R.Map.of_array C.Map.of_array;

  let spec = map ^> array binding in
  declare "Map.to_array" spec R.Map.to_array C.Map.to_array;

  let spec = seq_binding ^> map in
  declare "Map.of_seq" spec R.Map.of_seq C.Map.of_seq;

  let spec = seq_binding ^> map ^> map in
  declare "Map.add_seq" spec R.Map.add_seq C.Map.add_seq;

  let spec = map ^> seq_binding in
  declare "Map.to_seq" spec R.Map.to_seq C.Map.to_seq;

  let spec = key ^> map ^> seq_binding in
  declare "Map.to_seq_from" spec R.Map.to_seq_from C.Map.to_seq_from;

  let spec = map ^> seq_binding in
  declare "Map.to_rev_seq" spec R.Map.to_rev_seq C.Map.to_rev_seq;

  (* Section 4: iterating, searching, transforming maps. *)

  let spec = pair_iter (map ^> list binding) in
  declare "Map.iter" spec R.Map.iter C.Map.iter;

  let spec = pair_fold (map ^> list binding) in
  declare "Map.fold" spec R.Map.fold C.Map.fold;

  let spec = ipredicate ^> map ^> bool in
  declare "Map.for_all" spec R.Map.for_all C.Map.for_all;

  let spec = ipredicate ^> map ^> bool in
  declare "Map.exists" spec R.Map.exists C.Map.exists;

  let spec = increasing ^> map ^!> binding in
  declare "Map.find_first" spec R.Map.find_first C.Map.find_first;

  let spec = increasing ^> map ^> option binding in
  declare "Map.find_first_opt" spec R.Map.find_first_opt C.Map.find_first_opt;

  let spec = decreasing ^> map ^!> binding in
  declare "Map.find_last" spec R.Map.find_last C.Map.find_last;

  let spec = decreasing ^> map ^> option binding in
  declare "Map.find_last_opt" spec R.Map.find_last_opt C.Map.find_last_opt;

  (* We use pure functions, for simplicity, so we do not test that
     [map] invokes [f] in the correct order (that is, by increasing
     order of keys). *)

  let spec = transformation ^> map ^> map in
  declare "Map.map" spec R.Map.map C.Map.map;

  let spec = itransformation ^> map ^> map in
  declare "Map.mapi" spec R.Map.mapi C.Map.mapi;

  (* When applied to [ialways], [filter] must preserve sharing. *)
  let spec = sharing (map ^> map) in
  declare "(Map.filter ialways)" spec
    (R.Map.filter ialways) (C.Map.filter ialways);

  let spec = ipredicate ^> map ^> map in
  declare "Map.filter" spec R.Map.filter C.Map.filter;

  let spec = iotransformation ^> map ^> map in
  declare "Map.filter_map" spec R.Map.filter_map C.Map.filter_map;

  let spec = ipredicate ^> map ^> map *** map in
  declare "Map.partition" spec R.Map.partition C.Map.partition;

  (* Section 5: random access. *)

  if Candidate.has_random_access_functions then begin

    let spec = map ^>> fun m -> lt (R.Map.cardinal m) ^> binding in
    declare "Map.get" spec R.Map.get C.Map.get;

    declare_key_map_function `MayFailWhenAbsentKey int
      "Map.index" R.Map.index C.Map.index;

    let spec = map ^>> fun s -> le (R.Map.cardinal s) ^> map *** map in
    declare "Map.cut" spec R.Map.cut C.Map.cut;

    let spec = map ^>> fun s -> lt (R.Map.cardinal s) ^> triple map binding map in
    declare "Map.cut_and_get" spec R.Map.cut_and_get C.Map.cut_and_get;

  end;

  (* Section 6: enumerations. *)

  let enum = declare_abstract_type () in

  let spec = enum in
  declare "Map.Enum.empty" spec R.Map.Enum.empty C.Map.Enum.empty;

  let spec = enum ^> bool in
  declare "Map.Enum.is_empty" spec R.Map.Enum.is_empty C.Map.Enum.is_empty;

  let spec = map ^> enum in
  declare "Map.Enum.enum" spec R.Map.Enum.enum C.Map.Enum.enum;

  let spec = key ^> map ^> enum in
  declare "Map.Enum.from_enum" spec R.Map.Enum.from_enum C.Map.Enum.from_enum;

  let spec = enum ^!> binding in
  declare "Map.Enum.head" spec R.Map.Enum.head C.Map.Enum.head;

  let spec = enum ^!> enum in
  declare "Map.Enum.tail" spec R.Map.Enum.tail C.Map.Enum.tail;

  let spec = enum ^> option binding in
  declare "Map.Enum.head_opt" spec R.Map.Enum.head_opt C.Map.Enum.head_opt;

  let spec = enum ^> option enum in
  declare "Map.Enum.tail_opt" spec R.Map.Enum.tail_opt C.Map.Enum.tail_opt;

  let spec = key ^> enum ^> enum in
  declare "Map.Enum.from" spec R.Map.Enum.from C.Map.Enum.from;

  let spec = enum ^> seq_binding in
  declare "Map.Enum.to_seq" spec R.Map.Enum.to_seq C.Map.Enum.to_seq;

  let spec = enum ^> map in
  declare "Map.Enum.elements" spec R.Map.Enum.elements C.Map.Enum.elements;

  let spec = enum ^> int in
  declare "Map.Enum.length" spec R.Map.Enum.length C.Map.Enum.length;

  ()

(* -------------------------------------------------------------------------- *)

(* Declare the operations that involve both sets and maps. *)

(* These operations are the reason why we test sets and maps together,
   rather than separately. This has a cost, as we end up building many
   scenarios that involve both sets and maps, even though, most of the
   time, this will not help us find any bugs. But things are simpler
   this way. *)

let () =

  let spec = map ^> set in
  declare "domain" spec R.domain C.domain;

  let spec = transformation ^> set ^> map in
  declare "lift" spec R.lift C.lift;

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  dprintf "          open Helpers;;\n";
  dprintf "          open %s;;\n" Candidate.name;
  let fuel = 16 in
  main fuel
