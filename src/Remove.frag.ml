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

(* [remove_min_elt_1 l v r] removes the minimum element of the tree
   [NODE(l, v, r)]. *)

let rec remove_min_elt_1 (l : TREE) (v : ELT) (r : TREE) : TREE =
  match VIEW(l) with
  | LEAF ->
      r
  | NODE(ll, lv, lr)
      let l = remove_min_elt_1 ll lv lr in
      join_quasi_siblings l v r

(* [remove_min_elt t] removes the minimum element of the tree [t]. *)

let remove_min_elt (t : TREE) : TREE =
  match VIEW(t) with
  | LEAF ->
      raise Not_found
  | NODE(l, v, r)
      remove_min_elt_1 l v r

(* [remove_max_elt_1 l v r] removes the maximum element of the tree
   [NODE(l, v, r)]. *)

let rec remove_max_elt_1 (l : TREE) (v : ELT) (r : TREE) : TREE =
  match VIEW(r) with
  | LEAF ->
      l
  | NODE(rl, rv, rr)
      let r = remove_max_elt_1 rl rv rr in
      join_quasi_siblings l v r

(* [remove_max_elt t] removes the maximum element of the tree [t]. *)

let remove_max_elt (t : TREE) : TREE =
  match VIEW(t) with
  | LEAF ->
      raise Not_found
  | NODE(l, v, r)
      remove_max_elt_1 l v r

(* In the map variant, the above operations have different names. *)

#ifdef MAP_VARIANT
let remove_min_binding = remove_min_elt
let remove_max_binding = remove_max_elt
#endif

(* [join2_siblings l r] is analogous to [join2 l r], but requires the
   subtrees [l] and [r] to be siblings. *)

(* [join2_siblings] is named [merge] in OCaml's Set library. *)

(* This implementation arbitrarily chooses to place the minimum element of the
   tree [r] at the root. One could also choose to place the maximum element of
   the tree [l] at the root. One could imagine choosing between these
   alternatives, based on the weights or heights of the trees [l] and [r], if
   such a notion exists. That would remove the need for rebalancing. However,
   this seems to make essentially no difference in practice. *)

let join2_siblings (l : TREE) (r : TREE) : TREE =
  assert (siblings l r);
  match VIEW(l), VIEW(r) with
  | _, LEAF ->
      l
  | LEAF, _ ->
      r
  | _, NODE(rl, rv, rr)
      join_quasi_siblings
        l
        (min_elt_1 rv rl)           (* same as [min_elt r] *)
        (remove_min_elt_1 rl rv rr) (* same as [remove_min_elt r] *)

(* This is removal in the style of BFS. *)

(* (Disabled.) (Set variant only.)

let remove (k : key) (t : TREE) : TREE =
  let l, _, r = split k t in
  join2 l r

 *)

(* This is a less elegant but more efficient version of removal. *)

(* This implementation is taken from OCaml's Set library. *)

let rec remove (x : key) (t : TREE) : TREE =
  match VIEW(t) with
  | LEAF ->
      empty
  | NODE(l, v, r)
      let c = E.compare x (GET_KEY(v)) in
      if c = 0 then
        join2_siblings l r
      else if c < 0 then
        let l' = remove x l in
        if l == l' then t else join_quasi_siblings l' v r
      else
        let r' = remove x r in
        if r == r' then t else join_quasi_siblings l v r'

#ifdef MAP_VARIANT

(* [update] generalizes [add], [replace], and [remove] by letting the user
   choose how to transform the information associated with a specific key. *)

(* This code is essentially the same as in OCaml's Map library. *)

let rec update
  (x : key) (f : 'a option -> 'a option)
  (t : 'a binding tree) : 'a binding tree =
  match VIEW(t) with
  | LEAF ->
      begin match f None with
      | None ->
          t
      | Some d ->
          let v = (x, d) in
          singleton v
      end
  | NODE(l, v, r)
      let (k, d) = v in
      let c = E.compare x k in
      if c = 0 then begin
        match f (Some d) with
        | None ->
            join2_siblings l r
        | Some d' ->
            if d == d' then t else
            let v = (k, d') in
            join_siblings l v r
      end
      else if c < 0 then
        let l' = update x f l in
        if l == l' then t else join_quasi_siblings l' v r
      else
        let r' = update x f r in
        if r == r' then t else join_quasi_siblings l v r'

(* [add_to_list] is taken from OCaml's Map library (and reformulated). *)

let[@inline] flatten (ods : 'a list option) : 'a list =
  match ods with None -> [] | Some ds -> ds

let add_to_list x d m =
  let add ods = Some (d :: flatten ods) in
  update x add m

#endif
