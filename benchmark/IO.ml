(******************************************************************************)
(*                                                                            *)
(*                                   Bistro                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

(* [exhaust channel] reads all of the data that's available on [channel].
   It does not assume that the length of the data is known ahead of time.
   It does not close the channel. *)

let exhaust channel =
  let chunk_size = 16384 in
  let buffer = Buffer.create chunk_size in
  let chunk = Bytes.create chunk_size in
  let rec loop () =
    let length = input channel chunk 0 chunk_size in
    if length = 0 then
      Buffer.contents buffer
    else begin
      Buffer.add_subbytes buffer chunk 0 length;
      loop()
    end
  in
  loop()

(* [read_whole_file filename] reads the file [filename] in text mode and
   returns its contents as a string. *)

let read_whole_file filename =
  let channel = open_in filename in
  let s = exhaust channel in
  close_in channel;
  s
