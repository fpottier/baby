(* This is insertion in the style of BFS. *)

let _simple_add (k : key) (t : tree) : tree =
  let l, _, r = split k t in
  join l k r

(* This is a less elegant but more efficient version of insertion. *)

(* This implementation is taken from OCaml's Set library. *)

let rec add (x : key) (t : tree) : tree =
  match VIEW(t) with
  | LEAF ->
      singleton x
  | NODE(l, v, r) ->
      let c = E.compare x v in
      if c = 0 then
        t
      else if c < 0 then
        let l' = add x l in
        if l == l' then t else join_neighbors l' v r
      else
        let r' = add x r in
        if r == r' then t else join_neighbors l v r'
