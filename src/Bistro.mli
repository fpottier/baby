(**************************************************************************)
(*                                                                        *)
(*                                  Bistro                                *)
(*                                                                        *)
(*                      François Pottier, Inria Paris                     *)
(*                                                                        *)
(*      Copyright 2024--2024 Inria. All rights reserved. This file is     *)
(*      distributed under the terms of the GNU Library General Public     *)
(*      License, with an exception, as described in the file LICENSE.     *)
(*                                                                        *)
(**************************************************************************)

(**This library offers two flavors of binary search trees as well as
   building blocks that allow advanced users to construct their own
   custom flavors.

   For {b height-balanced binary search trees}, ready for use,
   please see {!Bistro.H.Set.Make}.

   For {b weight-balanced binary search trees}, ready for use,
   please see {!Bistro.W.Set.Make}.

 *)

(** @inline *)
include module type of Signatures

module H : sig

  module Set : sig
    module Make (E : OrderedType) : SET with type elt = E.t
  end

end

module W : sig

  module Set : sig
    module Make (E : OrderedType) : SET with type elt = E.t
  end

end

module Make
(E : OrderedType)
(_ : BST with type key = E.t)
: SET with type elt = E.t

module Height : sig
  (** @inline *)
   include module type of Height
end

module Weight : sig
  (** @inline *)
   include module type of Weight
end
