(******************************************************************************)
(*                                                                            *)
(*                                   Bistro                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

(* A reference implementation of sets. *)

module Make (E : sig
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
