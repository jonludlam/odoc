(* can also be := *)
module type Fail = sig end

(* must be := *)
module type Monad := sig
  (* must be := and the same name *)
  module type Fail := Fail

  include Fail
end

include Monad

