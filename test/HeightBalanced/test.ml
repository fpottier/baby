open Monolith

module V = Int
module R = Reference.Make(V)
module C = Bbst.HeightBalanced.Make(V)

(* -------------------------------------------------------------------------- *)

(* We have one abstract type, namely [set]. *)

let set =
  declare_abstract_type ()

(* We draw random integer keys. *)

let value =
  let n = 1 lsl 10 in
  semi_open_interval (-n) (n-1)

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = set in
  declare "empty" spec R.empty C.empty;

  let spec = value ^> set ^> bool in
  declare "mem" spec R.mem C.mem;

  let spec = value ^> set ^> set in
  declare "add" spec R.add C.add;

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 128 in
  main fuel
