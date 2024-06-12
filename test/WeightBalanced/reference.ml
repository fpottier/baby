(* A reference implementation of sets. *)

open Bistro

module Make (E : OrderedType) = struct

  include Stdlib.Set.Make(E)

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

  let index x s =
    let equal x y = E.compare x y = 0 in
    match List.find_index (equal x) (elements s) with
    | Some i ->
        i
    | None ->
        raise Not_found

  let split_at_2 s i =
    let a = Array.of_list (elements s) in
    of_array (Array.sub a 0 i),
    of_array (Array.sub a i (Array.length a - i))

  let split_at_3 s i =
    let a = Array.of_list (elements s) in
    of_array (Array.sub a 0 i),
    a.(i),
    of_array (Array.sub a (i+1) (Array.length a - (i+1)))

end
