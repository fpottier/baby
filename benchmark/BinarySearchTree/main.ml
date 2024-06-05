open Printf
open Bbst

module B = Common.Benchmark
let run benchmarks =
  List.iter B.drive_and_display benchmarks

module R = struct

  include Set.Make(Int)

  let xor s1 s2 =
    union (diff s1 s2) (diff s2 s1)

end

module C = BinarySearchTree.Make(Int)(Height.Make(Int))
module F = HeightBalanced.Make(Int)
module W = WeightBalanced.Make(Int)

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
  let module C = Add(C)(struct include P let candidate = "height/modular" end) in
  let module F = Add(F)(struct include P let candidate = "height/flat" end) in
  let module W = Add(W)(struct include P let candidate = "weight/flat" end) in
  [ R.benchmark; (* C.benchmark; *) F.benchmark; W.benchmark ]

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
  let module C = Remove(C)(struct include P let candidate = "height/modular" end) in
  let module F = Remove(F)(struct include P let candidate = "height/flat" end) in
  let module W = Remove(W)(struct include P let candidate = "weight/flat" end) in
  [ R.benchmark; (* C.benchmark; *) F.benchmark; W.benchmark ]

(* -------------------------------------------------------------------------- *)

(* A benchmark for all operations that take two sets as arguments:
   [union], [inter], [diff], [xor], [subset], [equal], [compare],
   etc. *)

(* The parameters [u1] and [u2] control how many unique elements each set
   contains -- that is, how many elements are one set and in not in the other
   set. The parameter [c] controls how many elements are common. *)

(* The parameter [cm] is a construction method for the two sets. If the
   parameter is [Common] then we first build a set of the common elements, and
   insert the unique elements into this common basis. If the parameter is
   [Separate] then we build two separate sets of the common elements, using
   different insertion orders, so the two sets are likely to be represented by
   trees of different shapes, even though they are equal sets. *)

type cm =
  | Common
  | Separate

module Binary (S : sig
  (* These are used to construct sets. *)
  type t
  val empty : t
  val add : int -> t -> t
  val union : t -> t -> t
  val elements : t -> int list
end) (P : sig
  val seed : int
  val n : int
  val u1 : int
  val u2 : int
  val c : int
  val candidate : string
  val cm : cm
  (* This is the binary operation that we want to test. *)
  type result
  val binary : S.t -> S.t -> result
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
    let name = sprintf "%s; %s" candidate comment
    and run () () =
      sets |> Array.iter @@ fun (s1, s2) ->
      ignore (binary s1 s2)
    in
    B.benchmark ~name ~quota ~basis ~run

end

(* This emits a human-readable comment on a quadruple [u1, u2, c, cm]. *)

let binary_benchmark_comment (u1, u2, c, cm) =
  eprintf "\n";
  eprintf "--- construction method: %s\n"
    (match cm with Common -> "common" | Separate -> "separate");
  if u1 + u2 = 0 then eprintf "--- the sets are %s\n"
    (match cm with Common -> "physically equal" | Separate -> "equal")
  else begin
    if u1 = 0 || u2 = 0 then eprintf "--- one set is a subset of the other\n";
    if c = 0 && u1 + u2 > 0 then eprintf "--- the sets are disjoint\n";
    if u1 = u2 then eprintf "--- the sets have the same size\n";
    if u1 > u2 && u2 + c > 0 then eprintf "--- the size ratio is %d\n" ((u1 + c) / (u2 + c))
  end

(* This is a list of [u1, u2, c] triples. *)

let aux u =
  [
    (* [u] unique elements on the left. *)
    u, 0, 10;
    u, 0, 100;
    u, 0, 1000;
    u, 0, 10000;
    (* [u] unique elements on the right. *)
    0, u, 10;
    0, u, 100;
    0, u, 1000;
    0, u, 10000;
    (* [u] unique elements on each side. *)
    u, u, 0;
    u, u, 10;
    u, u, 100;
    u, u, 1000;
    u, u, 10000;
  ]

let binary_benchmark_data : (int * int * int) list =
  [
    (* No unique elements. *)
    0, 0, 10;
    0, 0, 10000;
  ] @
  aux 10 @
  aux 10000 @
  []

(* This is a list of [u1, u2, c, cm] quadruples. *)

type quadruple =
  int * int * int * cm

let binary_benchmark_data : quadruple list =
  binary_benchmark_data
  |> List.map (fun (u1, u2, c) ->
      [ (u1, u2, c, Common); (u1, u2, c, Separate) ])
  |> List.flatten

(* This runs a benchmark of a binary operation,
   using the above quadruples. *)

let run_binary_benchmark (benchmark : quadruple -> B.benchmark list) =
  binary_benchmark_data |> List.iter @@ fun (q : quadruple) ->
  binary_benchmark_comment q;
  run (benchmark q)

(* -------------------------------------------------------------------------- *)

(* Union. *)

let union (u1, u2, c, cm) =
  let module P = struct
    let seed, n, u1, u2, c, cm = 123, n, u1, u2, c, cm
  end in
  let module R = Binary(R)(struct include P let binary = R.union type result = R.t let candidate = "union (reference)" end) in
  let module F = Binary(F)(struct include P let binary = F.union type result = F.t let candidate = "union (height/flat)" end) in
  let module W = Binary(W)(struct include P let binary = W.union type result = W.t let candidate = "union (weight/flat)" end) in
  [ R.benchmark; F.benchmark; W.benchmark ]

(* Intersection. *)

let inter (u1, u2, c, cm) =
  let module P = struct
    let seed, n, u1, u2, c, cm = 123, n, u1, u2, c, cm
  end in
  let module R = Binary(R)(struct include P let binary = R.inter type result = R.t let candidate = "inter (reference)" end) in
  let module F = Binary(F)(struct include P let binary = F.inter type result = F.t let candidate = "inter (height/flat)" end) in
  let module W = Binary(W)(struct include P let binary = W.inter type result = W.t let candidate = "inter (weight/flat)" end) in
  [ R.benchmark; F.benchmark; W.benchmark ]

(* Difference. *)

let diff (u1, u2, c, cm) =
  let module P = struct
    let seed, n, u1, u2, c, cm = 123, n, u1, u2, c, cm
  end in
  let module R = Binary(R)(struct include P let binary = R.diff type result = R.t let candidate = "diff (reference)" end) in
  let module F = Binary(F)(struct include P let binary = F.diff type result = F.t let candidate = "diff (height/flat)" end) in
  let module W = Binary(W)(struct include P let binary = W.diff type result = W.t let candidate = "diff (weight/flat)" end) in
  [ R.benchmark; F.benchmark; W.benchmark ]

(* Symmetric difference. *)

let xor (u1, u2, c, cm) =
  let module P = struct
    let seed, n, u1, u2, c, cm = 123, n, u1, u2, c, cm
  end in
  let module R = Binary(R)(struct include P let binary = R.xor type result = R.t let candidate = "xor (reference)" end) in
  let module F = Binary(F)(struct include P let binary = F.xor type result = F.t let candidate = "xor (height/flat)" end) in
  let module W = Binary(W)(struct include P let binary = W.xor type result = W.t let candidate = "xor (weight/flat)" end) in
  [ R.benchmark; F.benchmark; W.benchmark ]

(* Inclusion. *)

let subset (u1, u2, c, cm) =
  let module P = struct
    let seed, n, u1, u2, c, cm = 123, n, u1, u2, c, cm
    type result = bool
  end in
  let module R = Binary(R)(struct include P let binary = R.subset let candidate = "subset (reference)" end) in
  let module F = Binary(F)(struct include P let binary = F.subset let candidate = "subset (height/flat)" end) in
  let module W = Binary(W)(struct include P let binary = W.subset let candidate = "subset (weight/flat)" end) in
  [ R.benchmark; F.benchmark; W.benchmark ]

(* Disjointness. *)

let disjoint (u1, u2, c, cm) =
  let module P = struct
    let seed, n, u1, u2, c, cm = 123, n, u1, u2, c, cm
    type result = bool
  end in
  let module R = Binary(R)(struct include P let binary = R.disjoint let candidate = "disjoint (reference)" end) in
  let module F = Binary(F)(struct include P let binary = F.disjoint let candidate = "disjoint (height/flat)" end) in
  let module W = Binary(W)(struct include P let binary = W.disjoint let candidate = "disjoint (weight/flat)" end) in
  [ R.benchmark; F.benchmark; W.benchmark ]

(* Comparison. *)

let compare (u1, u2, c, cm) =
  let module P = struct
    let seed, n, u1, u2, c, cm = 123, n, u1, u2, c, cm
    type result = int
  end in
  let module R = Binary(R)(struct include P let binary = R.compare let candidate = "compare (reference)" end) in
  let module W = Binary(W)(struct include P let binary = W.compare let candidate = "compare (weight/flat)" end) in
  [ R.benchmark; W.benchmark ]

(* Equality. *)

let equal (u1, u2, c, cm) =
  let module P = struct
    let seed, n, u1, u2, c, cm = 123, n, u1, u2, c, cm
    type result = bool
  end in
  let module R = Binary(R)(struct include P let binary = R.equal let candidate = "equal (reference)" end) in
  let module F = Binary(F)(struct include P let binary = F.equal let candidate = "equal (height/flat)" end) in
  let module W = Binary(W)(struct include P let binary = W.equal let candidate = "equal (weight/flat)" end) in
  [ R.benchmark; F.benchmark; W.benchmark ]

(* -------------------------------------------------------------------------- *)

(* Synthetic benchmarks. *)

module Eratosthenes (S : sig
  type t
  val empty : t
  val add : int -> t -> t
  val diff : t -> t -> t
  val union : t -> t -> t
end) (P : sig
  val u : int
  val candidate : string
end) = struct
  open S
  open P
  let basis = 1
  let rec multiples_from k a =
    if a < u then add a (multiples_from k (a+k)) else empty
  let multiples k : t =
    multiples_from k 0
  let rec foreach f i j accu =
    if i < j then foreach f (i+1) j (f i accu) else accu
  let interval i j : t =
    foreach add i j empty

  let name = sprintf "eratosthenes/diff (universe size %d) (%s)" u candidate
  let run () () =
    (* Eratosthenes in one line! Starting from all numbers in the interval
       [0, u), for each [k], remove the multiples of [k]. *)
    foreach (fun k s -> diff s (multiples k)) 2 u (interval 0 u)
    |> ignore
  let benchmark1 = B.benchmark ~name ~quota ~basis ~run

  let name = sprintf "eratosthenes/union (universe size %d) (%s)" u candidate
  let run () () =
    (* Starting from nothing, for each [k], add the multiples of [k]. *)
    foreach (fun k s -> union s (multiples k)) 2 u empty
    |> ignore
  let benchmark2 = B.benchmark ~name ~quota ~basis ~run

end

let eratosthenes u =
  let module P = struct let u = u end in
  let module R = Eratosthenes(R)(struct include P let candidate = "reference" end) in
  let module F = Eratosthenes(F)(struct include P let candidate = "height/flat" end) in
  let module W = Eratosthenes(W)(struct include P let candidate = "weight/flat" end) in
  [ R.benchmark1; F.benchmark1; W.benchmark1;
    R.benchmark2; F.benchmark2; W.benchmark2; ]

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
    run_binary_benchmark union
  end;

  if false then begin
    eprintf "*** inter\n";
    run_binary_benchmark inter
  end;

  if false then begin
    eprintf "*** diff\n";
    run_binary_benchmark diff
  end;

  if false then begin
    eprintf "*** xor\n";
    run_binary_benchmark xor
  end;

  if false then begin
    eprintf "*** subset\n";
    run_binary_benchmark subset
  end;

  if false then begin
    eprintf "*** disjoint\n";
    run_binary_benchmark disjoint
  end;

  if false then begin
    eprintf "*** compare\n";
    run_binary_benchmark compare
  end;

  if false then begin
    eprintf "*** equal\n";
    run_binary_benchmark equal
  end;

  if true then begin
    eprintf "*** eratosthenes\n";
    eprintf "\n";
    run (eratosthenes 100);
    eprintf "\n";
    run (eratosthenes 1000);
    eprintf "\n";
    run (eratosthenes 10000);
    eprintf "\n";
    run (eratosthenes 100000)
  end;

  ()
