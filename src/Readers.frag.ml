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

let elements (t : tree) : elt list =
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

  (* [cons_enum t e] concatenates the tree [t] in front of the
     enumeration [e]. *)
  (* TODO rename *)

  let rec cons_enum (t : tree) (e : enum) : enum =
    match VIEW(t) with
    | LEAF ->
        e
    | NODE(l, v, r) ->
        cons_enum l (More (v, r, e))

  (* [enum] converts a tree to an enumeration. *)

  let[@inline] enum (t : tree) : enum =
    cons_enum t empty

  (* [enum_from_aux low t e] constructs an enumeration whose elements are:
     1- the elements [x] of the tree [t] such that [low <= x] holds,
     followed with 2- the elements of the enumeration [e]. *)
  (* TODO rename *)

  let rec enum_from_aux (low : key) (t : tree) (e : enum) : enum =
    match VIEW(t) with
    | LEAF ->
        e
    | NODE(l, v, r) ->
        let c = E.compare v low in
        if c = 0 then
          More (v, r, e)
        else if c < 0 then
          enum_from_aux low r e
        else
          enum_from_aux low l (More (v, r, e))

  let[@inline] enum_from (low : key) (t : tree) : enum =
    enum_from_aux low t empty

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
          enum_from_aux low r e
    | End ->
        (* [e] is empty. Keep part of [r]. *)
        enum_from low r
          (* this is equivalent to [enum_from_aux low r e] *)

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
    | More (_, r, e) -> cons_enum r e

  let head_opt (e : enum) : key option =
    match e with
    | End            -> None
    | More (v, _, _) -> Some v

  let tail_opt (e : enum) : enum option =
    match e with
    | End            -> None
    | More (_, r, e) -> Some (cons_enum r e)

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
        compare (cons_enum r1 e1) (cons_enum r2 e2)

  (* [to_seq] converts an enumeration to an OCaml sequence. *)

  let rec to_seq_node (e : enum) : key Seq.node =
    match e with
    | End ->
        Seq.Nil
    | More (v, r, e) ->
        Seq.Cons (v, fun () -> to_seq_node (cons_enum r e))

  let to_seq (e : enum) : key Seq.t =
    fun () -> to_seq_node e

  (* [elements] converts an enumeration back to a tree. *)

  (* It is the only function in this file that constructs a tree.
     It exploits the construction functions [add_min_element] and
     [join2]. *)

  let rec elements (e : enum) : tree =
    match e with
    | End ->
        leaf
    | More (v, r, e) ->
        add_min_element v (join2 r (elements e))

end

(* Conversion of a tree to an OCaml sequence. *)

let to_seq (t : tree) : key Seq.t =
  fun () -> Enum.(to_seq_node (enum t))

(* -------------------------------------------------------------------------- *)

(* Comparison. *)

(* Instead of using two numerations for the trees [t1] and [t2], one could
   perform a recursive traversal of [t1], while consuming an enumeration of
   [t2]. I have tested and benchmarked this variant: it allocates less memory,
   and can be faster, but can also be about twice slower. *)

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

let equal t1 t2 =
  compare t1 t2 = 0
