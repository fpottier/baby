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

open Array

let compress equal a =
  let n = length a in
  if n <= 1 then
    (* If the length of the array is 0 or 1, there is nothing to do. *)
    n
  else
    (* Now, prepare to copy the elements of the array downwards, in place,
       so as to eliminate duplicate elements. [last] is the last element
       that was read and kept. [!dst] is the number of elements that have
       been kept so far, so it is also the index of the slot that will be
       written next. *)
    let last = ref a.(0) in
    let dst = ref 1 in
    (* Let [src] scan the array. *)
    for src = 1 to n - 1 do
      assert (1 <= !dst && !dst <= src && src < n);
      assert (equal !last a.(src-1));
      let current = a.(src) in
      if current = !last then
        (* Skip this element. *)
        ()
      else begin
        (* Keep this element, copying it down to index [!dst]. *)
        if !dst < src then a.(!dst) <- current;
        last := current;
        incr dst
      end;
      assert (equal !last current);
      assert (equal !last a.(src));
    done;
    !dst

type run =
  int * int

let rec foreach_increasing_run_in_slice compare yield accu a i n =
  assert (0 <= i && i <= n && n <= length a);
  if i = n then
    (* There are no more runs. *)
    accu
  else
    (* A new run begins at index [i]. *)
    let last = a.(i)
    and j = i + 1 in
    scan compare yield accu a i last j n

and scan compare yield accu a i last j n =
  assert (0 <= i && i <= j && j <= n && n <= length a);
  if j = n then
    (* The run [i, j] ends here,
       and the loop ends as well. *)
    yield accu i j
  else
    let current = a.(j) in
    if compare last current < 0 then
      (* The current run continues. *)
      let last = current
      and j = j + 1 in
      scan compare yield accu a i last j n
    else
      (* The run [i, j] ends here,
         and the loop continues. *)
      let accu = yield accu i j in
      let last = current
      and i = j
      and j = j + 1 in
      scan compare yield accu a i last j n

let foreach_increasing_run compare yield accu a =
  let i = 0
  and n = length a in
  foreach_increasing_run_in_slice compare yield accu a i n

let increasing_runs compare a =
  let yield runs i j = ((i, j) :: runs) in
  foreach_increasing_run compare yield [] a
  |> List.rev
