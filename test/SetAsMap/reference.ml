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

(* A reference implementation of maps. *)

module Make (E : sig
  type t
  val compare : t -> t -> int
end) = struct

  (* This implementation is based on OCaml's Map library. *)
  include Map.Make(E)

end
