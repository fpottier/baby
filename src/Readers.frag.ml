(* -------------------------------------------------------------------------- *)

(* Membership. *)

let rec mem (x : key) (t : tree) : bool =
  match VIEW(t) with
  | LEAF ->
      false
  | NODE(l, v, r) ->
      let c = E.compare x v in
      c = 0 || mem x (if c < 0 then l else r)

(* -------------------------------------------------------------------------- *)

(* Conversion to a sorted list. *)

let rec elements (t : tree) (k : elt list) : elt list =
  match VIEW(t) with
  | LEAF ->
      k
  | NODE(l, v, r) ->
      elements l (v :: elements r k)

let[@inline] elements (t : tree) : elt list =
  elements t []

(* -------------------------------------------------------------------------- *)

(* Enumerations. *)

module Enum = struct

  type tree = t

  type set = tree

  type enum =
    | End
    | More of elt * t * enum

  type t = enum

  let empty : enum =
    End

  let[@inline] is_empty (e : enum) : bool =
    match e with
    | End -> true
    | More _ -> false

  (* [enum_1 t e] concatenates the tree [t] in front of the
     enumeration [e]. *)

  (* This function is named [cons_enum] in OCaml's Set library. *)

  let rec enum_1 (t : tree) (e : enum) : enum =
    match VIEW(t) with
    | LEAF ->
        e
    | NODE(l, v, r) ->
        enum_1 l (More (v, r, e))

  (* [enum] converts a tree to an enumeration. *)

  let[@inline] enum (t : tree) : enum =
    enum_1 t empty

  (* [enum_from_1 low t e] constructs an enumeration whose elements are:
     1- the elements [x] of the tree [t] such that [low <= x] holds,
     followed with 2- the elements of the enumeration [e]. *)

  let rec enum_from_1 (low : key) (t : tree) (e : enum) : enum =
    match VIEW(t) with
    | LEAF ->
        e
    | NODE(l, v, r) ->
        let c = E.compare v low in
        if c = 0 then
          More (v, r, e)
        else if c < 0 then
          enum_from_1 low r e
        else
          enum_from_1 low l (More (v, r, e))

  let[@inline] enum_from (low : key) (t : tree) : enum =
    enum_from_1 low t empty

  (* [from_more low r e] extracts from an enumeration [More (v, r, e)],
     where the value [v] is known to lie below the threshold [low],
     the elements that lie at or above the threshold [low]. *)

  let rec from_more (low : key) (r : tree) (e : enum) : enum =
    (* Peek past [r] at the first element [v'] of [e], if there is one. *)
    match e with
    | More (v', r', e') ->
        let c = E.compare low v' in
        if c > 0 then
          (* [v'] is below the threshold.
             The subtree [r] and the value [v'] must be discarded.
             Continue with [r'] and [e']. *)
          from_more low r' e'
        else if c = 0 then
          (* [v'] is at the threshold.
             The subtree [r] must be discarded. [e] must be kept. *)
          e
        else (* c < 0 *)
          (* [v'] is above the threshold. *)
          (* No part of [e] must be discarded. *)
          (* Keep part of [r], followed with [e]. *)
          enum_from_1 low r e
    | End ->
        (* [e] is empty. Keep part of [r]. *)
        enum_from low r
          (* this is equivalent to [enum_from_1 low r e] *)

  (* [from low e] extracts from the enumeration [e]
     the elements that lie at or above the threshold [low] . *)

  (* One could define [from low e] as [from_more low Empty e].
     However, the following code is slightly more efficient. *)

  let from (low : key) (e : enum) : enum =
    match e with
    | More (v, r, e') ->
        if E.compare low v <= 0 then
          (* [v] is at or above the threshold. Keep all elements. *)
          e
        else
          (* [v] is below the threshold. [v] must be discarded. *)
          from_more low r e'
    | End ->
        End

  let head (e : enum) : key =
    match e with
    | End            -> raise Not_found
    | More (v, _, _) -> v

  let tail (e : enum) : enum =
    match e with
    | End            -> raise Not_found
    | More (_, r, e) -> enum_1 r e

  let head_opt (e : enum) : key option =
    match e with
    | End            -> None
    | More (v, _, _) -> Some v

  let tail_opt (e : enum) : enum option =
    match e with
    | End            -> None
    | More (_, r, e) -> Some (enum_1 r e)

  (* [compare e1 e2] compares the enumerations [e1] and [e2]
     according to a lexicographic ordering. *)

  let rec compare (e1 : enum) (e2 : enum) : int =
    match e1, e2 with
    | End, End ->
        0
    | End, More _ ->
        -1
    | More _, End ->
        1
    | More (v1, r1, e1), More (v2, r2, e2) ->
        let c = E.compare v1 v2 in
        if c <> 0 then c else
        compare (enum_1 r1 e1) (enum_1 r2 e2)

  (* [to_seq] converts an enumeration to an OCaml sequence. *)

  let rec to_seq_node (e : enum) : key Seq.node =
    match e with
    | End ->
        Seq.Nil
    | More (v, r, e) ->
        Seq.Cons (v, fun () -> to_seq_node (enum_1 r e))

  let to_seq (e : enum) : key Seq.t =
    fun () -> to_seq_node e

  (* [elements] converts an enumeration back to a tree. *)

  (* It is the only function in this file that constructs a tree.
     It exploits the construction function [join].
     It performs no key comparisons. *)

  let rec elements (v : key) (r : tree) (e : enum) : tree =
    match e with
    | End ->
        join leaf v r
    | More (v', r', e) ->
        elements v (join r v' r') e

  let elements (e : enum) : tree =
    match e with
    | End ->
        leaf
    | More (v, r, e) ->
        elements v r e

  (* Disjointness. *)

  exception NotDisjoint

  (* [enum_from_disjoint_1 low t e] returns the same result as [enum_from_1
     low t e], except that it raises [NotDisjoint] if the key [low] appears
     in its result. *)

  let rec enum_from_disjoint_1 (low : key) (t : tree) (e : enum) : enum =
    match VIEW(t) with
    | LEAF ->
        e
    | NODE(l, v, r) ->
        let c = E.compare v low in
        if c = 0 then
          raise NotDisjoint
        else if c < 0 then
          enum_from_disjoint_1 low r e
        else
          enum_from_disjoint_1 low l (More (v, r, e))

  (* [from_more_disjoint low r e] returns the same result as [from_more low r
     e], except that it raises [NotDisjoint] if the key [low] appears in its
     result. *)

  let rec from_more_disjoint (low : key) (r : tree) (e : enum) : enum =
    match e with
    | More (v', r', e') ->
        let c = E.compare low v' in
        if c > 0 then
          from_more_disjoint low r' e'
        else if c = 0 then
          raise NotDisjoint
        else
          enum_from_disjoint_1 low r e
    | End ->
        enum_from_disjoint_1 low r e

  (* [disjoint_more_more v1 r1 e1 v2 r2 e2] requires [v1 < v2]. It determines
     whether the enumerations [More(v1, r1, e1)] and [More(v2, r2, e2)] are
     disjoint. It either returns [true] or raises [NotDisjoint]. *)

  (* This is Veldhuizen's leapfrog join algorithm. *)

  let rec disjoint_more_more v1 r1 e1 v2 r2 e2 =
    if debug then assert (E.compare v1 v2 < 0);
    (* Skip past [v2] in the enumeration [e1]. *)
    (* If [v2] appears in [e1], fail. *)
    let e1 = from_more_disjoint v2 r1 e1 in
    match e1 with
    | End ->
        (* If [e1] is now empty, we are done. *)
        true
    | More (v1, r1, e1) ->
        (* If [e1] is nonempty, then its front value [v1] must be greater than
           [v2]. Exchange the roles of the two enumerations and continue. *)
        if debug then assert (E.compare v2 v1 < 0);
        disjoint_more_more v2 r2 e2 v1 r1 e1

  (* [disjoint e1 e2] determines whether the enumerations [e1] and [e2] are
     disjoint. *)

  let disjoint (e1 : enum) (e2 : enum) : bool =
    match e1, e2 with
    | End, _
    | _, End ->
        true
    | More (v1, r1, e1), More (v2, r2, e2) ->
        let c = E.compare v1 v2 in
        if c = 0 then
          false
        else
          try
            if c < 0 then
              disjoint_more_more v1 r1 e1 v2 r2 e2
            else
              disjoint_more_more v2 r2 e2 v1 r1 e1
          with NotDisjoint ->
            false

end

(* Conversion of a tree to an OCaml sequence. *)

let to_seq (t : tree) : key Seq.t =
  fun () -> Enum.(to_seq_node (enum t))

(* -------------------------------------------------------------------------- *)

(* Comparison. *)

(* Instead of using enumerations of the trees [t1] and [t2], one could perform
   a recursive traversal of [t1], while consuming an enumeration of [t2]. I
   have benchmarked this variant: it allocates less memory, and can be faster,
   but can also be about twice slower. *)

let compare (t1 : tree) (t2 : tree) : int =
  if t1 == t2 then 0 else (* fast path *)
  Enum.(compare (enum t1) (enum t2))

(* -------------------------------------------------------------------------- *)

(* Equality. *)

(* Equality can be implemented in several ways. E.g., [equal t1 t2] could be
   implemented in one line by [subset t1 t2 && subset t2 t1] or also in one
   line by [is_empty (xor t1 t2)]. (The latter idea could be optimized, so
   as to avoid actually constructing the tree [xor t1 t2] in memory.) Some
   experiments suggest that either of these approaches is more expensive
   than the following approach, which is based on [compare]. *)

(* In weight-balanced trees, the weight of a tree can be determined in
   constant time. This yields a fast path: if the weights and [t1] and [t2]
   differ, then they cannot possibly be equal. In height-balanced trees, the
   [weight] function returns a constant value, so this fast path is
   disabled. *)

let[@inline] equal t1 t2 =
  weight t1 = weight t2 && (* fast path *)
  compare t1 t2 = 0

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

(* Splitting by index. *)

let rec split_at (t : tree) (i : int) : tree * key * tree =
  if debug then assert (0 <= i && i < cardinal t);
  match VIEW(t) with
  | LEAF ->
      assert false
  | NODE(l, v, r) ->
      let cl = cardinal l in
      if i = cl then
        l, v, r
      else if i < cl then
        let ll, lv, lr = split_at l i in
        ll, lv, join lr v r
      else
        let rl, rv, rr = split_at r (i - (cl + 1)) in
        join l v rl, rv, rr

let split_at (t : tree) (i : int) : tree * key * tree =
  if constant_time_cardinal then
    if 0 <= i && i < cardinal t then
      split_at t i
    else
      Printf.sprintf "split_at: index %d is out of expected range [0, %d)"
        i (cardinal t)
      |> invalid_arg
  else
    failwith "split_at: operation is not available"
