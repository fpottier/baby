(* This is the type of the functor [Set.Make]. *)
module type FACTORY =
  functor (E : Set.OrderedType) -> Set.S with type elt = E.t

(* We check that our functors have the same type. This guarantees that (as far
   as the type-checker is concerned) they can be used as drop-in replacements
   for OCaml's [Set.Make]. *)
module TestH = (Bistro.H.Set.Make : FACTORY)
module TestW = (Bistro.W.Set.Make : FACTORY)
