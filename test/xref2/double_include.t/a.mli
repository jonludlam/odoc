
module DoubleInclude1 : sig
  module DoubleInclude2 : sig
    type double_include
  end
end

module DoubleInclude3 : sig
  include module type of DoubleInclude1
end

include module type of DoubleInclude3.DoubleInclude2


