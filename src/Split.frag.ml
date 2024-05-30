let rec split (k : key) (t : tree) : tree * bool * tree =
  match VIEW(t) with
  | LEAF ->
      leaf, false, leaf
  | NODE(l, m, r) ->
      let c = E.compare k m in
      if c = 0 then
        l, true, r
      else if c < 0 then
        let ll, b, lr = split k l in
        ll, b, node lr m r
      else
        let rl, b, rr = split k r in
        node l m rl, b, rr

let rec split_last (l : tree) (k : key) (r : tree) : tree * key =
  match VIEW(r) with
  | LEAF ->
      l, k
  | NODE(l', k', r') ->
      let r, m = split_last l' k' r' in
      node l k r, m

(* [join2] is known as [concat] in OCaml's Set library. *)

let join2 (l : tree) (r : tree) : tree =
  match VIEW(l) with
  | LEAF ->
      r
  | NODE(ll, m, lr) ->
      let l', k = split_last ll m lr in
      node l' k r
