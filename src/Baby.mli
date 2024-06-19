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

(**This library offers two flavors of binary search trees as well as
   building blocks that allow advanced users to construct their own
   custom flavors.

   For {b height-balanced binary search trees}, ready for use,
   please see {!Baby.H.Set.Make}.

   For {b weight-balanced binary search trees}, ready for use,
   please see {!Baby.W.Set.Make}.

 *)

(** @inline *)
include module type of Signatures

(**The functor [Baby.Make] constructs balanced binary search trees
   based on a user-supplied balancing scheme. The main operation that
   the user is expected to provide is [join]. *)
module Make
(E : OrderedType)
(_ : CORE with type key = E.t)
: SET with type elt = E.t

(**The module [Baby.H] provides ready-made height-balanced binary
   search trees. *)
module H : sig

  module Set : sig
    module type OrderedType = OrderedType
    module type S = SET
    module Make (E : OrderedType) : SET with type elt = E.t
    module Int : SET with type elt = int
  end

end

(**The module [Baby.W] provides ready-made weight-balanced binary
   search trees. *)
module W : sig

  module Set : sig
    module type OrderedType = OrderedType
    module type S = SET
    module Make (E : OrderedType) : SET with type elt = E.t
    module Int : SET with type elt = int
  end

end

(**/**)

(* The following modules must be exported, because they are (or may be) used
   in the benchmarks. Because they are somewhat unlikely to be useful to an
   end user, their existence is not advertised. *)

module Height : sig
  (** @inline *)
   include module type of Height
end

module Weight : sig
  (** @inline *)
   include module type of Weight
end
