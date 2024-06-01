open Printf
open Bbst

module B = Common.Benchmark
module R = Set.Make(Int)
module C = BinarySearchTree.Make(Int)(Height.Make(Int))
module F = HeightBalanced.Make(Int)

let quota =
  "5.0s"

(* n = number of operations per batch *)
let n = 1000
(* u = size of element universe *)

(* -------------------------------------------------------------------------- *)

(* Insertion benchmark. *)

let insertion u =
  let basis = n in
  let a = Array.init n (fun _i -> Random.int u) in

  let name = sprintf "Insertion (universe size %d) (new modular code)" u
  and run () () =
    Array.fold_left (fun s x -> C.add x s) C.empty a
    |> Sys.opaque_identity
    |> ignore
  in
  let insertion_modular = B.benchmark ~name ~quota ~basis ~run in

  let name = sprintf "Insertion (universe size %d) (new flat code)" u
  and run () () =
    Array.fold_left (fun s x -> F.add x s) F.empty a
    |> Sys.opaque_identity
    |> ignore
  in
  let insertion_flat = B.benchmark ~name ~quota ~basis ~run in

  let name = sprintf "Insertion (universe size %d) (OCaml Set)" u
  and run () () =
    Array.fold_left (fun s x -> R.add x s) R.empty a
    |> Sys.opaque_identity
    |> ignore
  in
  let insertion_old = B.benchmark ~name ~quota ~basis ~run in

  [ insertion_old; insertion_modular; insertion_flat ]

(* -------------------------------------------------------------------------- *)

(* Removal benchmark. *)

let remove u =
  let basis = n in
  let a = Array.init n (fun _i -> Random.int u) in
  let c = C.(Array.fold_left (fun s x -> add x s) empty a)
  and f = F.(Array.fold_left (fun s x -> add x s) empty a)
  and r = R.(Array.fold_left (fun s x -> add x s) empty a) in

  let name = sprintf "Removal (universe size %d) (new modular code)" u
  and run () () =
    Array.fold_left (fun s x -> C.remove x s) c a
    |> Sys.opaque_identity
    |> ignore
  in
  let modular = B.benchmark ~name ~quota ~basis ~run in

  let name = sprintf "Removal (universe size %d) (new flat code)" u
  and run () () =
    Array.fold_left (fun s x -> F.remove x s) f a
    |> Sys.opaque_identity
    |> ignore
  in
  let flat = B.benchmark ~name ~quota ~basis ~run in

  let name = sprintf "Removal (universe size %d) (OCaml Set)" u
  and run () () =
    Array.fold_left (fun s x -> R.remove x s) r a
    |> Sys.opaque_identity
    |> ignore
  in
  let old = B.benchmark ~name ~quota ~basis ~run in

  [ old; modular; flat ]

(* -------------------------------------------------------------------------- *)

(* Union benchmark. *)

let union u =
  let basis = n in

  let name = sprintf "Union (universe size %d) (new modular code)" u
  and run () =
    let open C in
    let rec multiples_from k a =
      if a < u then add a (multiples_from k (a+k)) else empty in
    let multiples k = multiples_from k 0 in
    let a = Array.init n (fun k -> multiples (k+5)) in
    fun () ->
      Array.fold_left union empty a
      |> Sys.opaque_identity
      |> ignore
  in
  let union_modular = B.benchmark ~name ~quota ~basis ~run in

  let name = sprintf "Union (universe size %d) (new flat code)" u
  and run () =
    let open F in
    let rec multiples_from k a =
      if a < u then add a (multiples_from k (a+k)) else empty in
    let multiples k = multiples_from k 0 in
    let a = Array.init n (fun k -> multiples (k+5)) in
    fun () ->
      Array.fold_left union empty a
      |> Sys.opaque_identity
      |> ignore
  in
  let union_flat = B.benchmark ~name ~quota ~basis ~run in

  let name = sprintf "Union (universe size %d) (OCaml Set)" u
  and run () =
    let open R in
    let rec multiples_from k a =
      if a < u then add a (multiples_from k (a+k)) else empty in
    let multiples k = multiples_from k 0 in
    let a = Array.init n (fun k -> multiples (k+5)) in
    fun () ->
      Array.fold_left union empty a
      |> Sys.opaque_identity
      |> ignore
  in
  let union_old = B.benchmark ~name ~quota ~basis ~run in

  [ union_old; union_modular; union_flat ]

(* -------------------------------------------------------------------------- *)

(* Main. *)

let run benchmarks =
  List.iter B.drive_and_display benchmarks

let () =

  if false then begin
    eprintf "*** Insertion benchmarks.\n";
    eprintf "\n";
    run (insertion (1 lsl 8));
    eprintf "\n";
    run (insertion (1 lsl 16));
    eprintf "\n";
  end;

  if true then begin
    eprintf "*** Removal benchmarks.\n";
    eprintf "\n";
    run (remove (1 lsl 8));
    eprintf "\n";
    run (remove (1 lsl 16));
    eprintf "\n";
  end;

  if false then begin
    eprintf "*** Union benchmarks.\n";
    eprintf "\n";
    run (union (1 lsl 8));
    eprintf "\n";
    run (union (1 lsl 16));
    eprintf "\n";
  end;

  ()
