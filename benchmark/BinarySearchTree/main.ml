open Printf
open Bbst

module B = Common.Benchmark
module R = Set.Make(Int)
module C = BinarySearchTree.Make(Int)(Height.Make(Int))
module F = HeightBalanced.Make(Int)

module type PARAMS = sig
  val seed : int
  val n : int
  val u : int
  val candidate : string
end

let quota =
  "5.0s"

(* n = number of operations per batch *)
let n = 1000
(* u = size of element universe *)

(* -------------------------------------------------------------------------- *)

(* Insertion benchmark. *)

module Add (S : sig
  type t
  val empty : t
  val add : int -> t -> t
end) (P : PARAMS) = struct
  open P
  open S
  let () = Random.init seed
  let basis = n
  let a = Array.init n (fun _i -> Random.int u)
  let name = sprintf "add (universe size %d) (%s)" u candidate
  let run () () =
    Array.fold_left (fun s x -> add x s) empty a
    |> ignore
  let benchmark = B.benchmark ~name ~quota ~basis ~run
end

let add u =
  let module P = struct
    let seed = 123
    let n = n
    let u = u
  end in
  let module R = Add(R)(struct include P let candidate = "reference" end) in
  let module C = Add(C)(struct include P let candidate = "new/modular" end) in
  let module F = Add(F)(struct include P let candidate = "new/flat" end) in
  [ R.benchmark; C.benchmark; F.benchmark ]

(* -------------------------------------------------------------------------- *)

(* Removal benchmark. *)

module Remove (S : sig
  type t
  val empty : t
  val add : int -> t -> t
  val remove : int -> t -> t
end) (P : PARAMS) = struct
  open P
  open S
  let () = Random.init seed
  let basis = n
  let a = Array.init n (fun _i -> Random.int u)
  let s = Array.fold_left (fun s x -> add x s) empty a
  let name = sprintf "remove (universe size %d) (%s)" u candidate
  let run () () =
    Array.fold_left (fun s x -> remove x s) s a
    |> ignore
  let benchmark = B.benchmark ~name ~quota ~basis ~run
end

let remove u =
  let module P = struct
    let seed = 123
    let n = n
    let u = u
  end in
  let module R = Remove(R)(struct include P let candidate = "reference" end) in
  let module C = Remove(C)(struct include P let candidate = "new/modular" end) in
  let module F = Remove(F)(struct include P let candidate = "new/flat" end) in
  [ R.benchmark; C.benchmark; F.benchmark ]

(* -------------------------------------------------------------------------- *)

(* Union benchmark. *)

module Union (S : sig
  type t
  val empty : t
  val add : int -> t -> t
  val union : t -> t -> t
end) (P : PARAMS) = struct
  open P
  open S
  let () = Random.init seed
  let basis = n
  let rec multiples_from k a =
    if a < u then add a (multiples_from k (a+k)) else empty
  let multiples k = multiples_from k 0
  let a = Array.init n (fun k -> multiples (k+5))
  let name = sprintf "union (universe size %d) (%s)" u candidate
  let run () () =
    Array.fold_left union empty a
    |> ignore
  let benchmark = B.benchmark ~name ~quota ~basis ~run
end

let union u =
  let module P = struct
    let seed = 123
    let n = n
    let u = u
  end in
  let module R = Union(R)(struct include P let candidate = "reference" end) in
  let module C = Union(C)(struct include P let candidate = "new/modular" end) in
  let module F = Union(F)(struct include P let candidate = "new/flat" end) in
  [ R.benchmark; C.benchmark; F.benchmark ]

(* -------------------------------------------------------------------------- *)

(* Main. *)

let run benchmarks =
  List.iter B.drive_and_display benchmarks

let () =

  if true then begin
    eprintf "*** add\n";
    eprintf "\n";
    run (add (1 lsl 8));
    eprintf "\n";
    run (add (1 lsl 16));
    eprintf "\n";
  end;

  if true then begin
    eprintf "*** remove\n";
    eprintf "\n";
    run (remove (1 lsl 8));
    eprintf "\n";
    run (remove (1 lsl 16));
    eprintf "\n";
  end;

  if true then begin
    eprintf "*** union\n";
    eprintf "\n";
    run (union (1 lsl 8));
    eprintf "\n";
    run (union (1 lsl 16));
    eprintf "\n";
  end;

  ()
