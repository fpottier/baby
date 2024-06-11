(* -------------------------------------------------------------------------- *)

(* Random access. *)

(* This implementation assumes that we have a constant-time [cardinal]
   function. Its complexity is logarithmic. *)

(* If [cardinal] requires linear time then this implementation of [get] has
   quadratic time complexity, which is unacceptable. In that case, it is
   preferable to just use [to_array], which has linear time complexity,
   followed with [Array.get]. *)

let rec get (t : tree) (i : int) : key =
  if debug then assert (0 <= i && i < cardinal t);
  match VIEW(t) with
  | LEAF ->
      assert false
  | NODE(l, v, r) ->
      let cl = cardinal l in
      if i = cl then
        v
      else if i < cl then
        get l i
      else
        get r (i - (cl + 1))

let get (t : tree) (i : int) : key =
  if constant_time_cardinal then
    if 0 <= i && i < cardinal t then
      get t i
    else
      Printf.sprintf "get: index %d is out of expected range [0, %d)"
        i (cardinal t)
      |> invalid_arg
  else
    failwith "get: operation is not available"

(* -------------------------------------------------------------------------- *)

(* Splitting by index -- in two parts. *)

let rec split_at_2 (t : tree) (i : int) : tree * tree =
  if debug then assert (0 <= i && i <= cardinal t);
  if i = 0 then
    leaf, t
  else if i = cardinal t then
    t, leaf
  else
    match VIEW(t) with
    | LEAF ->
        assert false
    | NODE(l, v, r) ->
        let cl = cardinal l in
        if i <= cl then
          let ll, lr = split_at_2 l i in
          if debug then assert (lr != l);
          ll, join lr v r
        else (* [cl < i] *)
          let rl, rr = split_at_2 r (i - (cl + 1)) in
          if debug then assert (rl != r);
          join l v rl, rr

let split_at_2 (t : tree) (i : int) : tree * tree =
  if constant_time_cardinal then
    if 0 <= i && i <= cardinal t then
      split_at_2 t i
    else
      Printf.sprintf "split_at_2: index %d is out of expected range [0, %d]"
        i (cardinal t)
      |> invalid_arg
  else
    failwith "split_at_2: operation is not available"

(* -------------------------------------------------------------------------- *)

(* Splitting by index -- in three parts. *)

let rec split_at_3 (t : tree) (i : int) : tree * key * tree =
  if debug then assert (0 <= i && i < cardinal t);
  match VIEW(t) with
  | LEAF ->
      assert false
  | NODE(l, v, r) ->
      let cl = cardinal l in
      if i = cl then
        l, v, r
      else if i < cl then
        let ll, lv, lr = split_at_3 l i in
        ll, lv, join lr v r
      else
        let rl, rv, rr = split_at_3 r (i - (cl + 1)) in
        join l v rl, rv, rr

let split_at_3 (t : tree) (i : int) : tree * key * tree =
  if constant_time_cardinal then
    if 0 <= i && i < cardinal t then
      split_at_3 t i
    else
      Printf.sprintf "split_at_3: index %d is out of expected range [0, %d)"
        i (cardinal t)
      |> invalid_arg
  else
    failwith "split_at_3: operation is not available"
