(* Test fixture for qualified module call detection bug *)
(* Bug: Module.func calls are not properly tracked, causing false positives *)

(* Simulates a library module with a helper function *)
module Lib = struct
  let library_helper x = x + 1
  let another_func x = x * 2
end

(* Call to Lib.library_helper using qualified syntax *)
let test_function () =
  Lib.library_helper 42;
  Lib.another_func 10
