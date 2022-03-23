module Foo = struct end
module Baz = struct
  include struct
    module Bar = Foo
  end
  module Bar = Bar
end
include Baz

