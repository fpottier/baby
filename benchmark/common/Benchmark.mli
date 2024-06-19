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

(**We support both persistent and ephemeral benchmarks.

   A persistent benchmark requires no setup. (In other words, its setup
   is performed once and for all, outside our control, by the user.) It
   can be repeated as many times as one wishes.

   An ephemeral benchmark requires some setup, which must be repeated every
   time. (The benchmark destroys its initial state.) In order to be able to
   run several copies of the benchmark as a group (and time them together), we
   need to create several copies of the initial state ahead of time. To allow
   this, a benchmark is represented as a function [run] of type [unit -> unit
   -> unit], where the two stages (setup and execution) are distinguished.

   Thus, persistent benchmarks form a special case of ephemeral benchmarks. *)
type benchmark

(**The parameters of the constructor [benchmark] are as follows:

   [name] is a descriptive name (e.g., "Perform 1000 push operations.").

   [quota] indicates how long the benchmark is allowed to take. This is
   indicates by a string that can be parsed by [Core.Time.Span.of_string]: for
   example, ["1.0s"]. The benchmark is repeated many times until the quota is
   exhausted.

   [basis] is an integer number by which every result is divided. For example,
   if the function [run] performs 1000 "push" operations on a stack, then, by
   setting [basis] to 1000, you get results that represent the cost of one
   push operation.

   [run] is the task whose execution must be timed. As explained above, this
   task is expected to operate in two stages, setup and execution. Only the
   execution stage is timed. *)
val benchmark:
  name:string ->
  quota:string ->
  basis:int ->
  run:(unit -> unit -> unit) ->
  benchmark

(**[drive_and_display benchmark] runs the benchmark [benchmark] and displays
   the results in a human-readable format on the standard output channel. *)
val drive_and_display : benchmark -> unit

(**[drive_and_display benchmark] runs the benchmark [benchmark] and displays
   the results in a machine-readable format on the standard output channel. *)
val drive_and_print : benchmark -> unit

(**[run_once benchmark] prints the name of the benchmark [benchmark] on the
   standard error channel and runs this benchmark once, without measuring
   anything. *)
val run_once : benchmark -> unit
