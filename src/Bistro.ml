include Signatures

module H = struct

  module Set = HeightBalanced

end

module W = struct

  module Set = WeightBalanced

end

include BinarySearchTree

module Height = Height
module Weight = Weight
