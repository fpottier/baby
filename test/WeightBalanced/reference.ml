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

(* A reference implementation of sets. *)

module MySet (E : sig
  type t
  val compare : t -> t -> int
end) = struct

  (* This implementation is based on OCaml's Set library. *)
  include Set.Make(E)

  let remove_min_elt s =
    let x = min_elt s in
    remove x s

  let remove_max_elt s =
    let x = max_elt s in
    remove x s

  let xor s1 s2 =
    union (diff s1 s2) (diff s2 s1)

  let of_array a =
    a |> Array.to_list |> of_list

  let to_array s =
    s |> elements |> Array.of_list

  let of_sorted_unique_array =
    of_array

  let get s i =
    List.nth (elements s) i

  (* This is [List.find_index], which appears in OCaml 5.2. *)
  let find_index p =
    let rec aux i = function
      [] -> None
      | a::l -> if p a then Some i else aux (i+1) l in
    aux 0

  let index x s =
    let equal x y = E.compare x y = 0 in
    match find_index (equal x) (elements s) with
    | Some i ->
        i
    | None ->
        raise Not_found

  let cut s i =
    let a = Array.of_list (elements s) in
    of_array (Array.sub a 0 i),
    of_array (Array.sub a i (Array.length a - i))

  let cut_and_get s i =
    let a = Array.of_list (elements s) in
    of_array (Array.sub a 0 i),
    a.(i),
    of_array (Array.sub a (i+1) (Array.length a - (i+1)))

  module Enum = struct

    type enum =
      elt Seq.t

    let empty =
      Seq.empty

    let is_empty =
      Seq.is_empty

    let enum =
      to_seq

    let rec from x (e : enum) =
      match e() with
      | Seq.Cons (x', e') ->
          if E.compare x x' <= 0 then
            e
          else
            from x e'
      | Seq.Nil ->
          e

    let from_enum x s =
      from x (enum s)

    let head e =
      match e() with
      | Seq.Cons (x, _) ->
          x
      | Seq.Nil ->
          raise Not_found

    let tail e =
      match e() with
      | Seq.Cons (_, e) ->
          e
      | Seq.Nil ->
          raise Not_found

    let head_opt e =
      match e() with
      | Seq.Cons (x, _) ->
          Some x
      | Seq.Nil ->
          None

    let tail_opt e =
      match e() with
      | Seq.Cons (_, e) ->
          Some e
      | Seq.Nil ->
          None

    let to_seq e =
      e

    let elements =
      of_seq

    let length =
      Seq.length

  end

end

(* A reference implementation of maps. *)

module MyMap (E : sig
  type t
  val compare : t -> t -> int
end) = struct

  (* This implementation is based on OCaml's Map library. *)

  include Map.Make(E)

  (* The following functions are missing in OCaml's Map library. *)

  let remove_min_binding m =
    match min_binding_opt m with
    | None ->
        raise Not_found
    | Some (k, _) ->
        remove k m

  let remove_max_binding m =
    match max_binding_opt m with
    | None ->
        raise Not_found
    | Some (k, _) ->
        remove k m

  let inter f m1 m2 =
    let f' key od1 d2 =
      match od1, d2 with
      | Some v1, Some v2 -> f key v1 v2
      | _, _ -> None
    in
    merge f' m1 m2

  let diff m1 m2 =
    let f _key od1 od2 = match od2 with None -> od1 | Some _ -> None in
    merge f m1 m2

  let xor m1 m2 =
    let f _key od1 od2 =
      match od1, od2 with
      | Some v1, None     -> Some v1
      | None   , Some v2  -> Some v2
      | Some  _, Some  _  -> None
      | None   , None     -> None
    in
    merge f m1 m2

  let disjoint m1 m2 =
    let f _key _v1 _v2 = Some () in
    let m = inter f m1 m2 in
    is_empty m

  let sub leq m1 m2 =
    let exception Nope in
    try
      iter (fun k1 v1 ->
        match find_opt k1 m2 with
        | Some v2 when leq v1 v2 -> ()
        | _                      -> raise Nope
      ) m1;
      true
    with Nope ->
      false

  (* [of_list] appears in Stdlib.Map in 5.1. *)
  let of_list bs =
    List.fold_left (fun m (k, v) -> add k v m) empty bs

  let of_array a =
    of_list (Array.to_list a)

  let to_array m =
    Array.of_list (bindings m)

  let get m i =
    List.nth (bindings m) i

  (* This is [List.find_index], which appears in OCaml 5.2. *)
  let find_index p =
    let rec aux i = function
      [] -> None
      | a::l -> if p a then Some i else aux (i+1) l in
    aux 0

  let index x m =
    let has_key x (y, _) = E.compare x y = 0 in
    match find_index (has_key x) (bindings m) with
    | Some i ->
        i
    | None ->
        raise Not_found

  let cut m i =
    let a = Array.of_list (bindings m) in
    of_array (Array.sub a 0 i),
    of_array (Array.sub a i (Array.length a - i))

  let cut_and_get m i =
    let a = Array.of_list (bindings m) in
    of_array (Array.sub a 0 i),
    a.(i),
    of_array (Array.sub a (i+1) (Array.length a - (i+1)))

  module Enum = struct

    type 'a enum =
      (key * 'a) Seq.t

    let empty =
      Seq.empty

    let is_empty =
      Seq.is_empty

    let enum =
      to_seq

    let rec from x (e : 'a enum) =
      match e() with
      | Seq.Cons ((x', _), e') ->
          if E.compare x x' <= 0 then
            e
          else
            from x e'
      | Seq.Nil ->
          e

    let from_enum x s =
      from x (enum s)

    let head e =
      match e() with
      | Seq.Cons (x, _) ->
          x
      | Seq.Nil ->
          raise Not_found

    let tail e =
      match e() with
      | Seq.Cons (_, e) ->
          e
      | Seq.Nil ->
          raise Not_found

    let head_opt e =
      match e() with
      | Seq.Cons (x, _) ->
          Some x
      | Seq.Nil ->
          None

    let tail_opt e =
      match e() with
      | Seq.Cons (_, e) ->
          Some e
      | Seq.Nil ->
          None

    let to_seq e =
      e

    let elements =
      of_seq

    let length =
      Seq.length

  end

end

(* We bundle sets and maps together. *)

module Make  (E : sig
  type t
  val compare : t -> t -> int
end) = struct

  module Set = MySet(E)
  module Map = MyMap(E)

  let domain m =
    Set.of_list (List.map fst (Map.bindings m))

  let lift f s =
    Set.elements s
    |> List.map (fun x -> (x, f x))
    |> Map.of_list

end
