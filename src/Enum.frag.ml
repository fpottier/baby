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

  (* [from_enum_1 low t e] constructs an enumeration whose elements are: 1-
     the elements [x] of the tree [t] such that [low <= x] holds, followed
     with 2- all elements of the enumeration [e]. *)

  (* In [from_enum_1 low t e], only the tree [t] is filtered by the constraint
     [low <= x]. The enumeration [e] is not filtered (typically because it is
     already known that all of its elements satisfy this constraint). This is
     in contrast with [from_more low t e] (below), where both [t] and [e] are
     filtered. *)

  let rec from_enum_1 (low : key) (t : tree) (e : enum) : enum =
    match VIEW(t) with
    | LEAF ->
        e
    | NODE(l, v, r) ->
        let c = E.compare v low in
        if c = 0 then
          More (v, r, e)
        else if c < 0 then
          from_enum_1 low r e
        else
          from_enum_1 low l (More (v, r, e))

  let[@inline] from_enum (low : key) (t : tree) : enum =
    from_enum_1 low t empty

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
          from_enum_1 low r e
    | End ->
        (* [e] is empty. Keep part of [r]. *)
        from_enum low r
          (* this is equivalent to [from_enum_1 low r e] *)

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

  (* [from_enum_disjoint_1 low t e] returns the same result as [from_enum_1
     low t e], except that it raises [NotDisjoint] if the key [low] appears
     in its result. *)

  let rec from_enum_disjoint_1 (low : key) (t : tree) (e : enum) : enum =
    match VIEW(t) with
    | LEAF ->
        e
    | NODE(l, v, r) ->
        let c = E.compare v low in
        if c = 0 then
          raise NotDisjoint
        else if c < 0 then
          from_enum_disjoint_1 low r e
        else
          from_enum_disjoint_1 low l (More (v, r, e))

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
          from_enum_disjoint_1 low r e
    | End ->
        from_enum_disjoint_1 low r e

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

  (* [length e] computes the length of the enumeration [e]. If we have
     a constant-time [cardinal] function on sets, then its complexity
     is logarithmic. Otherwise, its complexity is linear. *)

  let rec length_aux accu (e : enum) : int =
    match e with
    | End ->
        accu
    | More (_, r, e) ->
        length_aux (accu + cardinal r) e

  let[@inline] length (e : enum) : int =
    length_aux 0 e

end (* Enum *)
