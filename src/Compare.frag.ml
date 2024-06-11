(* -------------------------------------------------------------------------- *)

(* Comparison. *)

(* Instead of using enumerations of the trees [t1] and [t2], one could perform
   a recursive traversal of [t1], while consuming an enumeration of [t2]. I
   have benchmarked this variant: it allocates less memory, and can be faster,
   but can also be about twice slower. *)

let compare (t1 : tree) (t2 : tree) : int =
  if t1 == t2 then 0 else (* fast path *)
  Enum.(compare (enum t1) (enum t2))
