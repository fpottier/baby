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

end
