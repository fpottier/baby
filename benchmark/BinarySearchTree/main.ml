open Printf
open Bbst

module B = Common.Benchmark
let run benchmarks =
  List.iter B.drive_and_display benchmarks

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

(* Disjointness benchmark. *)

(* u1, u2 = number of elements that appear in one set -- not in the other set *)
(* c = number of elements that appear in both sets *)
(* cm = set construction method *)
type cm =
  | Common
  | Separate

module Disjoint (S : sig
  type t
  val empty : t
  val add : int -> t -> t
  val union : t -> t -> t
  val elements : t -> int list
  val disjoint : t -> t -> bool
end) (P : sig
  val seed : int
  val n : int
  val u1 : int
  val u2 : int
  val c : int
  val candidate : string
  val cm : cm
end) = struct
  open P
  open S
  let () = Random.init seed
  let basis = n

  (* Accumulate [s] random elements, drawn using [f], into [accu]. *)
  let rec mk_random_set f s accu =
    if s = 0 then accu else
    let i = f() in
    let accu = add i accu in
    mk_random_set f (s-1) accu
  let mk_random_set f s =
    mk_random_set f s empty
  let even_random_integer () =
    let i = Random.int (1 lsl 30 - 1) in
    2 * i
  let odd_random_integer () =
    even_random_integer() + 1
  let random_integer () =
    if Random.bool() then even_random_integer() else odd_random_integer()

  let (sets, comment : (S.t * S.t) array * string) =
    match cm with
    | Common ->
        (* Allocate an array of pairs of sets. *)
        begin
          Array.init n @@ fun _i ->
          let u = u1 + u2 in
          let c = mk_random_set random_integer c in
          let u1 = mk_random_set even_random_integer u1
          and u2 = mk_random_set odd_random_integer u2 in
          (* The unique elements are inserted into a common basis,
             namely the set [c]. *)
          let s1 = union u1 c
          and s2 = union u2 c in
          if u = 0 then assert (s1 == s2);
          s1, s2
        end,
        sprintf "common basis (u1 = %d, u2 = %d, c = %d)" u1 u2 c
    | Separate ->
        (* Allocate an array of pairs of sets. *)
        begin
          Array.init n @@ fun _i ->
          let u1 = mk_random_set even_random_integer u1
          and u2 = mk_random_set odd_random_integer u2 in
          (* The unique elements are inserted into two different representations
             of the same set, [c1] and [c2]. *)
          let c1 = mk_random_set random_integer c in
          let a2 = Array.of_list (S.elements c1) in
          Array.shuffle ~rand:Random.int a2;
          let c2 = Array.fold_left (fun accu x -> S.add x accu) S.empty a2 in
          let s1 = union u1 c1
          and s2 = union u2 c2 in
          s1, s2
        end,
        sprintf "separate bases (u1 = %d, u2 = %d, c = %d)" u1 u2 c

  let benchmark =
    let name = sprintf "disjoint (%s); %s" candidate comment
    and run () () =
      sets |> Array.iter @@ fun (s1, s2) ->
      ignore (disjoint s1 s2)
    in
    B.benchmark ~name ~quota ~basis ~run

end

let disjoint cm u1 u2 c =
  let module P = struct
    let seed = 123
    let n = n
    let u1, u2, c = u1, u2, c
    let cm = cm
  end in
  let module R = Disjoint(R)(struct include P let candidate = "reference" end) in
  (* let module C = Disjoint(C)(struct include P let candidate = "new/modular" end) in *)
  let module F = Disjoint(F)(struct include P let candidate = "new/flat" end) in
  [ R.benchmark; F.benchmark ]

let triple (u1, u2, c) =
  eprintf "\n";
  if u1 + u2 = 0 then eprintf "--- here, the sets are physically equal\n";
  if u1 = 0 || u2 = 0 then eprintf "--- here, one set is a subset of the other\n";
  if c = 0 && u1 + u2 > 0 then eprintf "--- here, the sets are disjoint\n";
  if u1 = u2 then eprintf "--- here, the sets have the same size\n";
  if u1 > u2 && u2 + c > 0 then eprintf "--- here, the size ratio is %d\n" ((u1 + c) / (u2 + c));
  run (disjoint Common u1 u2 c);
  if u1 + u2 = 0 then eprintf "--- here, the sets are equal\n";
  if u1 = 0 || u2 = 0 then eprintf "--- here, one set is a subset of the other\n";
  if c = 0 && u1 + u2 > 0 then eprintf "--- here, the sets are disjoint\n";
  if u1 = u2 then eprintf "--- here, the sets have the same size\n";
  if u1 > u2 && u2 + c > 0 then eprintf "--- here, the size ratio is %d\n" ((u1 + c) / (u2 + c));
  run (disjoint Separate u1 u2 c)

(* -------------------------------------------------------------------------- *)

(* Main. *)

let () =

  if false then begin
    eprintf "*** add\n";
    eprintf "\n";
    run (add (1 lsl 8));
    eprintf "\n";
    run (add (1 lsl 16));
    eprintf "\n";
  end;

  if false then begin
    eprintf "*** remove\n";
    eprintf "\n";
    run (remove (1 lsl 8));
    eprintf "\n";
    run (remove (1 lsl 16));
    eprintf "\n";
  end;

  if false then begin
    eprintf "*** union\n";
    eprintf "\n";
    run (union (1 lsl 8));
    eprintf "\n";
    run (union (1 lsl 16));
    eprintf "\n";
  end;

  if false then begin
    eprintf "*** disjoint\n";
    eprintf "\n";
    List.iter triple [
      (* No unique elements. *)
      0, 0, 10;
      0, 0, 100;
      0, 0, 1000;
      0, 0, 10000;
      (* 10 unique elements on one side. *)
      10, 0, 10;
      10, 0, 100;
      10, 0, 1000;
      10, 0, 10000;
      (* 10 unique elements on each side. *)
      10, 10, 0;
      10, 10, 10;
      10, 10, 100;
      10, 10, 1000;
      10, 10, 10000;
      (* 100 unique elements on one side. *)
      100, 0, 10;
      100, 0, 100;
      100, 0, 1000;
      100, 0, 10000;
      (* 100 unique elements on each side. *)
      100, 100, 0;
      100, 100, 10;
      100, 100, 100;
      100, 100, 1000;
      100, 100, 10000;
      (* 1000 unique elements on each side. *)
      1000, 1000, 0;
      1000, 1000, 10;
      1000, 1000, 100;
      1000, 1000, 1000;
      1000, 1000, 10000;
      (* 10000 unique elements on each side. *)
      10000, 10000, 0;
      10000, 10000, 10;
      10000, 10000, 100;
      10000, 10000, 1000;
      10000, 10000, 10000;
    ]
  end;

  ()
