(* This is insertion in the style of BFS. *)

let _simple_add (k : key) (t : tree) : tree =
  let l, _, r = split k t in
  join l k r

(* This is a less elegant but more efficient version of insertion. *)

(* This implementation of [add] is inspired by OCaml's Set library,
   but must use [node] to join two subtrees, because [node] is the
   only operation exposed by the abstract interface; whereas OCaml's
   Set library uses [bal] here. *)

let rec add (x : key) (t : tree) : tree =
  match VIEW(t) with
  | LEAF ->
      singleton x
  | NODE(l, v, r) ->
      let c = E.compare x v in
      if c = 0 then
        t
      else if c < 0 then
        let ll = add x l in
        if l == ll then t else join ll v r
      else
        let rr = add x r in
        if r == rr then t else join l v rr
