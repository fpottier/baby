(* [split] is implemented in the same way in OCaml's Set library and by BFS. *)

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
        ll, b, join lr m r
      else
        let rl, b, rr = split k r in
        join l m rl, b, rr

(* A specialized version of [split] that returns just the Boolean component
   of the result is [mem]. *)

(* [split13] is a variant of [split] that returns only the first and third
   components of the result. *)

let rec split13 (k : key) (t : tree) : tree * tree =
  match VIEW(t) with
  | LEAF ->
      leaf, leaf
  | NODE(l, m, r) ->
      let c = E.compare k m in
      if c = 0 then
        l, r
      else if c < 0 then
        let ll, lr = split13 k l in
        ll, join lr m r
      else
        let rl, rr = split13 k r in
        join l m rl, rr

(* [join2] is known as [concat] in OCaml's Set library. *)

(* This is the code proposed by BFS. Their [split_last] function
   corresponds to our functions [min_elt] and [remove_min_elt_1].

let rec split_last (l : tree) (k : key) (r : tree) : tree * key =
  match VIEW(r) with
  | LEAF ->
      l, k
  | NODE(l', k', r') ->
      let r, m = split_last l' k' r' in
      join l k r, m

let join2 (l : tree) (r : tree) : tree =
  match VIEW(l) with
  | LEAF ->
      r
  | NODE(ll, m, lr) ->
      let l', k = split_last ll m lr in
      join l' k r

 *)

let join2 (l : tree) (r : tree) : tree =
  match VIEW(l), VIEW(r) with
  | LEAF, _ ->
      r
  | _, LEAF ->
      l
  | _, NODE(rl, rv, rr) ->
      join
        l
        (min_elt_1 rv rl)           (* same as [min_elt r] *)
        (remove_min_elt_1 rl rv rr) (* same as [remove_min_elt r] *)
