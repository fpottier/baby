open Monolith

module V = Int

module R = Reference.Make(V)

module C = struct

#ifdef WEIGHT
  include Bbst.WeightBalanced.Make(V)
#else
  include Bbst.HeightBalanced.Make(V)
#endif

#ifdef WEIGHT

  (* [union] and [inter] guarantee that if the result is logically equal
     to one of the arguments then it is physically equal to one of the
     arguments. *)

  (* This guarantee holds for weight-balanced trees, but not for
     height-balanced trees; indeed, a reliable way of comparing
     the cardinals of the two sets is needed. *)

  let union t1 t2 =
    let result = union t1 t2 in
    if equal result t1 || equal result t2 then
      assert (result == t1 || result == t2);
    result

  let inter t1 t2 =
    let result = inter t1 t2 in
    if equal result t1 || equal result t2 then
      assert (result == t1 || result == t2);
    result

#endif

  (* [diff] guarantees that if the result is logically equal to [t1]
     then it is physically equal to [t1]. This holds regardless of
     which balancing criterion is used. *)

  let[@inline] diff t1 t2 =
    let result = diff t1 t2 in
    if equal result t1 then assert(result == t1);
    result

end

(* -------------------------------------------------------------------------- *)

(* We have one abstract type, namely [set]. *)

(* It is equipped with a well-formedness check,
   which ignores the model (the reference side). *)

let check _model =
  C.check,
  constant "check"

let set =
  declare_abstract_type ~check ()

(* We draw random integer keys. *)

let range =
  1 lsl 8

let value =
  semi_open_interval (-range) (range-1)

(* We can also draw an inhabitant out of a set. *)

let inhabits s =
  int_within @@ fun () ->
    let open R in
    let open Gen in
    if is_empty s then reject() else
    let x = min_elt s
    and y = max_elt s in
    let k = x + Random.int (y - x + 1) in
    let _, b, r = split k s in
    let z = if b then k else min_elt r in
    assert (mem z s);
    z

(* Generating arrays. *)

let array_value =
  easily_constructible
    Gen.(array (int range) (semi_open_interval (-range) (range-1)))
    Print.(array int)

let sorted_array compare n element () =
  let a = Gen.array n element () in
  Array.sort compare a;
  a

let sorted_unique_array compare n element () =
  let a = sorted_array compare n element () in
  let equal x y = compare x y = 0 in
  let n = Bbst.ArrayExtra.compress equal a in
  Array.sub a 0 n

let sorted_unique_array_value =
  easily_constructible
    Gen.(sorted_unique_array Int.compare (int 16) (semi_open_interval (-16) (15)))
    Print.(array int)

(* Consuming a sequence. *)

let seq_value =
  declare_seq value

(* Exchanging two arguments. *)

let flip f x y =
  f y x

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = set in
  declare "empty" spec R.empty C.empty;

  let spec = value ^> set in
  declare "singleton" spec R.singleton C.singleton;

  (* not tested: [is_empty] *)

  let spec = set ^!> value in
  declare "min_elt" spec R.min_elt C.min_elt;

  let spec = set ^!> option value in
  declare "min_elt_opt" spec R.min_elt_opt C.min_elt_opt;

  let spec = set ^!> value in
  declare "max_elt" spec R.max_elt C.max_elt;

  let spec = set ^!> option value in
  declare "max_elt_opt" spec R.max_elt_opt C.max_elt_opt;

  let spec = value ^> set ^> bool in
  declare "mem" spec R.mem C.mem;

  let spec = value ^> set ^> set in
  declare "add" spec R.add C.add;

  let spec = value ^> set ^> set in
  declare "remove" spec R.remove C.remove;

  (* Specifically remove a value that is in the set. *)
  let spec = set ^>> fun s -> (inhabits s) ^> set in
  declare "flip remove" spec (flip R.remove) (flip C.remove);

  let spec = set ^!> set in
  declare "remove_min_elt" spec R.remove_min_elt C.remove_min_elt;

  let spec = set ^!> set in
  declare "remove_max_elt" spec R.remove_max_elt C.remove_max_elt;

  let spec = set ^> set ^> set in
  declare "union" spec R.union C.union;

  let spec = set ^> set ^> set in
  declare "inter" spec R.inter C.inter;

  let spec = set ^> set ^> bool in
  declare "disjoint" spec R.disjoint C.disjoint;

  let spec = set ^> set ^> set in
  declare "diff" spec R.diff C.diff;

  let spec = set ^> set ^> bool in
  declare "subset" spec R.subset C.subset;

  let spec = set ^> set ^> set in
  declare "xor" spec R.xor C.xor;

  let spec = set ^> set ^> int in
  declare "compare" spec R.compare C.compare;

  (* [split] is not tested. *)

  let spec = set ^> list value in
  declare "elements" spec R.elements C.elements;

  let spec = set ^> seq_value in
  declare "to_seq" spec R.to_seq C.to_seq;

  (* [of_list] is important in this test because it offers a cheap way
     of creating nontrivial sets. It consumes just one unit of fuel. *)
  let spec = list value ^> set in
  declare "of_list" spec R.of_list C.of_list;

  let spec = array_value ^> set in
  declare "of_array" spec R.of_array C.of_array;

  let spec = sorted_unique_array_value ^> set in
  declare "of_sorted_unique_array" spec R.of_array C.of_array;

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let prologue () =
#ifdef WEIGHT
    dprintf "          open Bbst.WeightBalanced.Make(Int);;\n";
#else
    dprintf "          open Bbst.HeightBalanced.Make(Int);;\n";
#endif
    dprintf "          let flip f x y = f y x;;\n";
    ()
  in
  let fuel = 16 in
  main ~prologue fuel
