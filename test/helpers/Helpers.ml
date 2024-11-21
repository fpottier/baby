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

(* This library defines a few helper functions that are useful while testing. *)

(* In the future, some of these functions could move into Monolith or
   Monolith.Support. *)

open Monolith

let pair (x, y) =
  (x, constant y)

let choose items =
  constructible (Gen.choose (List.map pair items))

(* -------------------------------------------------------------------------- *)

(* Support functions. *)

let unnest (x, (y, z)) = (x,  y, z)
let   nest (x,  y, z)  = (x, (y, z))

(* -------------------------------------------------------------------------- *)

(* A combinator for arrays. *)

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

(* A combinator for triples. *)

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

(* A combinator to test that sharing is preserved. *)

(* [must_preserve_sharing f] behaves like [f],
   and tests that the result is physically equal to the argument.
   Otherwise, it fails. *)

let must_preserve_sharing f s =
  let s' = f s in
  assert (s == s');
  s'

let sharing spec =
  map_into
    must_preserve_sharing
    (must_preserve_sharing, constant "must_preserve_sharing")
    spec

(* -------------------------------------------------------------------------- *)

(* Combinators to test [iter] and [fold] functions. *)

type ('a, 'c) iter =
  ('a -> unit) -> 'c -> unit

let elements_of_iter (iter : ('a, 'c) iter) (c : 'c) : 'a list =
  let xs = ref [] in
  let push x = xs := x :: !xs in
  iter push c;
  List.rev !xs

let iter spec =
  map_into
    elements_of_iter
    (elements_of_iter, constant "elements_of_iter")
    spec

type ('a, 's, 'c) fold =
  ('a -> 's -> 's) -> 'c -> 's -> 's

let elements_of_fold (fold : ('a, 's, 'c) fold) (c : 'c) : 'a list =
  let cons x xs = x :: xs in
  fold cons c []
  |> List.rev

let fold spec =
  map_into
    elements_of_fold
    (elements_of_fold, constant "elements_of_fold")
    spec

(* -------------------------------------------------------------------------- *)

(* Combinators to test [iter] and [fold] functions that produce pairs
   with a curried calling convention. *)

let elements_of_pair_iter iter c =
  let xvs = ref [] in
  let push x v = xvs := (x, v) :: !xvs in
  iter push c;
  List.rev !xvs

let pair_iter spec =
  map_into
    elements_of_pair_iter
    (elements_of_pair_iter, constant "elements_of_pair_iter")
    spec

let elements_of_pair_fold fold c =
  let cons x v xvs = (x, v) :: xvs in
  fold cons c []
  |> List.rev

let pair_fold spec =
  map_into
    elements_of_pair_fold
    (elements_of_pair_fold, constant "elements_of_pair_fold")
    spec

(* -------------------------------------------------------------------------- *)

(* Ad hoc support functions. *)

(* -------------------------------------------------------------------------- *)

(* Functions of type [int -> bool]. *)

let positive x =
  0 <= x

let negative x =
  x < 0

let always _ =
  true

let never _ =
  false

let even x =
  (x mod 2 = 0)

let odd x =
  (x mod 2 <> 0)

let predicate =
  choose [
    positive, "positive";
    negative, "negative";
    always, "always";
    never, "never";
    even, "even";
    odd, "odd";
  ]

(* Increasing functions of type [int -> bool]. *)

let increasing =
  choose [
    positive, "positive";
    always, "always";
    never, "never";
  ]

(* Decreasing functions of type [int -> bool]. *)

let decreasing =
  choose [
    negative, "negative";
    always, "always";
    never, "never";
  ]

(* -------------------------------------------------------------------------- *)

(* Functions of type [int -> int -> bool]. *)

let ialways _ _ =
  true

let inever _ _ =
  false

let ieven   _ v =
  v mod 2 = 0

let ieven_diff i v =
  (i - v) mod 2 = 0

let ipredicate =
  choose [
    ialways, "ialways";
    inever, "inever";
    ieven, "ieven";
    ieven_diff, "ieven_diff";
  ]

(* -------------------------------------------------------------------------- *)

(* Boolean-valued equality functions, of type [int -> int -> bool]. *)

let beqalways _ _ =
  true

let same_parity x y =
  x mod 2 = y mod 2

let bequality =
  choose [
    (=), "(=)";
    beqalways, "beqalways";
    same_parity, "same_parity";
  ]

(* -------------------------------------------------------------------------- *)

(* Boolean-valued (total) ordering functions, of type [int -> int -> bool]. *)

let bordering : (int -> int -> bool, int -> int -> bool) spec =
  choose [
    (<=), "(<=)";
    (>=), "(>=)";
  ]

(* -------------------------------------------------------------------------- *)

(* Integer-valued (total) ordering functions, of type [int -> int -> int]. *)

let compare_rev x y =
  -(Int.compare x y)

let iordering =
  choose [
    Int.compare, "Int.compare";
    compare_rev, "compare_rev";
  ]

(* -------------------------------------------------------------------------- *)

(* Functions of type [int -> int]. *)

let transformation =
  choose [
    Fun.id, "Fun.id";
    succ, "succ";
    Int.neg, "Int.neg";
  ]

(* -------------------------------------------------------------------------- *)

(* Functions of type [int -> int option]. *)

let keep x =
  Some x

let drop_or_increment x =
  if x mod 2 = 0 then Some (x + 1) else None

let drop_or_negate x =
  if x mod 2 = 0 then Some (-x) else None

let otransformation =
  choose [
    keep, "keep";
    drop_or_increment, "drop_or_increment";
  ]

(* -------------------------------------------------------------------------- *)

(* Functions of type [int option -> int option]. *)

let increment = function
  | None   -> None
  | Some v -> Some (v + 1)

let remove_if_odd = function
  | Some v when v mod 2 = 0 -> Some v
  | _                       -> None

let with_zero = function
  | _ -> Some 0

let ootransformation =
  choose [
    Fun.id, "Fun.id";
    increment, "increment";
    remove_if_odd, "remove_if_odd";
    with_zero, "with_zero";
  ]

(* -------------------------------------------------------------------------- *)

(* Functions of type [int -> int -> int]. *)

let itransformation =
  choose [
    (+), "(+)";
    (-), "(-)";
    ( * ), "(*)";
    (fun _ y -> y + 1), "(fun _ y -> y + 1)"
  ]

(* -------------------------------------------------------------------------- *)

(* Functions of type [int -> int -> int option]. *)

let subo k v =
  if (k - v) mod 2 = 0 then Some (k - v) else None

let iotransformation =
  choose [
    subo, "subo";
  ]

(* -------------------------------------------------------------------------- *)

(* Functions of type [int -> int -> int -> int option]. *)

let select_fst _ v1 _v2 =
  Some v1

let select_snd _ _v1 v2 =
  Some v2

let combine _ v1 v2 =
  Some (v1 + v2)

let drop _ _v1 _v2 =
  None

let combine3 k v1 v2 =
  Some (k * (v1 - v2))

let cook k v1 v2 =
  if odd k && v1 < v2 then Some (k + v2 - v1) else None

let combination =
  choose [
    select_fst, "select_fst";
    select_snd, "select_snd";
    combine, "combine";
    drop, "drop";
    combine3, "combine3";
    cook, "cook";
  ]

(* -------------------------------------------------------------------------- *)

(* Functions of type [int -> int option -> int option -> int option]. *)

let agreement _ ov1 ov2 =
  if ov1 = ov2 then ov1 else None

let oxor _ ov1 ov2 =
  match ov1, ov2 with
  | None, Some _  -> ov2
  | Some _, None  -> ov1
  | Some _, Some _
  | None, None    -> None

let use_key key ov1 ov2 =
  match ov1, ov2 with
  | Some v1, Some v2 -> Some (key + v1 - v2)
  | _, None
  | None, _          -> None

let complex_combination =
  choose [
    agreement, "agreement";
    oxor, "oxor";
    use_key, "use_key";
  ]
