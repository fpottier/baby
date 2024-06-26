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

open Signatures

module Set = struct

  module type OrderedType = OrderedType

  module type S = SET

  module[@inline] Make (E : OrderedType) = struct

    include Height.Make(E)

    #define VIEW(t)       (t)
    #define LEAF          TLeaf
    #define NODE(x, y, z) TNode { l = x; v = y; r = z; _ }

    #include "Macros.frag.ml"
    #include "Common.frag.ml"

  end

  (* This module is equivalent to [Make(Stdlib.Int)]. *)
  module Int = struct

    module E = Stdlib.Int
    include Weight.Make(E)
    #include "Common.frag.ml"

  end

end
