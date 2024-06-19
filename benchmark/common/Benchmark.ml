(******************************************************************************)
(*                                                                            *)
(*                                    Baby                                    *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

open Gc
open Printf
module TSC = Time_stamp_counter
module Time = Core.Time_float
module Span = Time.Span
module Float = Core.Float

let postincrement index =
  let i = !index in
  index := i + 1;
  i

let stabilize_gc () =
  Gc.compact()

(* -------------------------------------------------------------------------- *)

(* We pre-allocate an array of [measurement] records, so as to not disturb
   the GC while the benchmark is running. *)

type measurement =
  {
    mutable repetitions:  int; (* how many repetitions were timed *)
    (* The following fields measure *all* of the above repetitions together: *)
    mutable cycles:     float;
    mutable nanos:      float;
    mutable minor:      float; (* words allocated in the minor heap *)
    mutable major:      float; (* words allocated in the major heap *)
    mutable promoted:   float; (* words promoted from minor to major heap *)
  }

let measurement _ : measurement =
  {
    repetitions = 0;
    cycles      = 0.0;
    nanos       = 0.0;
    minor       = 0.0;
    major       = 0.0;
    promoted    = 0.0;
  }

let isum f xs =
  Array.fold_left (fun sum x ->
    sum + f x
  ) 0 xs

let fsum f xs =
  Array.fold_left (fun sum x ->
    sum +. f x
  ) 0.0 xs

let total_repetitions xs =
  isum (fun x -> x.repetitions) xs

let average f xs =
  fsum f xs /. float_of_int (total_repetitions xs)

let average_cycles measurements =
  average (fun m -> m.cycles) measurements

let average_nanos measurements =
  average (fun m -> m.nanos) measurements

let average_minor measurements =
  average (fun m -> m.minor) measurements

let average_major measurements =
  average (fun m -> m.major) measurements

let average_promoted measurements =
  average (fun m -> m.promoted) measurements

let average (measurements : measurement array) : measurement =
  {
    repetitions = total_repetitions measurements;
    cycles = average_cycles measurements;
    nanos = average_nanos measurements;
    minor = average_minor measurements;
    major = average_major measurements;
    promoted = average_promoted measurements;
  }

let divide (basis : int) (m : measurement) : measurement =
  let basis = float_of_int basis in
  {
    repetitions = m.repetitions;
    cycles = m.cycles /. basis;
    nanos = m.nanos /. basis;
    minor = m.minor /. basis;
    major = m.major /. basis;
    promoted = m.promoted /. basis;
  }

(* -------------------------------------------------------------------------- *)

(* Description of a benchmark. *)

type benchmark =
  {
    name:        string;
    quota:       Span.t;
    basis:       int;
    run:         unit -> unit -> unit
}

(* Construction of a benchmark. *)

let benchmark ~name ~quota ~basis ~run =
  let quota = Span.of_string quota in
  { name; quota; basis; run }

(* -------------------------------------------------------------------------- *)

(* The function [drive] drives a benchmark. *)

let drive { name; quota; basis; run } =
  eprintf "%s.\n%!" name;
  (* Space for storing measurements. *)
  let max_measurements = 3000 in
  let measurements = Array.init max_measurements measurement in
  (* Counters. *)
  let index = ref 0 in
  (* Decide how many experiments should be run together in a group.
     This number is increased geometrically. This is believed to
     help smooth out GC noise. *)
  let repetitions = ref 3 in
  let scale = 1.05 in
  (* Record our start time and prepare to stop after a certain amount of time
     has elapsed. *)
  let start = Time.now() in
  let elapsed () = Time.diff (Time.now()) start in
  (* The main loop. *)
  while
    Span.(<=) (elapsed()) quota &&
    !index < max_measurements
  do
    (* Run the setup stage. This phase is not timed.
       We prepare as many copies of the benchmark as
       we need for this group. *)
    let group : (unit -> unit) array =
      Array.init !repetitions (fun _i -> run())
    in
    (* Stabilize the GC. *)
    stabilize_gc ();
    (* Perform pre-run measurements. *)
    let gc1 = Gc.quick_stat () in
    let t1 = Time.now () in
    let c1 = TSC.now () in
    (* Run each copy of the benchmark in this group. *)
    for i = 0 to !repetitions-1 do group.(i) () done;
    (* Perform post-run measurements. *)
    let c2 = TSC.now () in
    let t2 = Time.now () in
    let gc2 = Gc.quick_stat () in
    (* Save this measurement. *)
    let m = measurements.(postincrement index) in
    m.repetitions <- !repetitions;
    m.cycles      <- float_of_int (TSC.Span.to_int_exn (TSC.diff c2 c1));
    m.nanos       <- Span.to_ns (Time.diff t2 t1);
    m.minor       <- gc2.minor_words -. gc1.minor_words;
    m.major       <- gc2.major_words -. gc1.major_words;
    m.promoted    <- gc2.promoted_words -. gc1.promoted_words;
    (* If repetition is allowed, increase geometrically the number of
       repetitions. *)
    if !repetitions > 1 then
      repetitions :=
        max
          (!repetitions + 1)
          (Float.iround_towards_zero_exn (Float.of_int !repetitions *. scale));
  done;
  let duration = elapsed() in
  let n = !index in
  let measurements = Array.sub measurements 0 n in
  eprintf "Total time (including setup): %s; measurements: %03d; max group size: %03d.\n%!"
    (Span.to_short_string duration) n !repetitions;
  duration, divide basis (average measurements)

(* -------------------------------------------------------------------------- *)

(* Human-readable display. *)

let display (_duration, { repetitions = _; cycles; nanos; minor; major; promoted }) =
  let allocated = major +. minor -. promoted in
  eprintf "Cycles     | Nanos      | Major      | Minor      | Promoted   | Allocated  |\n";
  eprintf "%.3Ef | %.3Ef | %.3Ef | %.3Ef | %.3Ef | %.3Ef |\n%!"
    cycles nanos minor major promoted allocated

(* -------------------------------------------------------------------------- *)

(* Machine-readable display. *)

let print (duration, { repetitions; cycles; nanos; minor; major; promoted }) =
  let allocated = major +. minor -. promoted in
  printf "exectime     %f\nrepetitions %9d\ncycles      %9f\nnanos       %9f\nmajor       %9f\nminor       %9f\npromoted    %9f\nallocated   %9f\n%!"
    (Span.to_sec duration)
    repetitions cycles nanos minor major promoted allocated

(* -------------------------------------------------------------------------- *)

(* The main entry points. *)

let drive_and_display benchmark =
  display (drive benchmark)

let drive_and_print benchmark =
  print (drive benchmark)

let run_once { name; run; _ } =
  eprintf "%s.\n%!" name;
  run()()
