(* Test fixture for pipe operator with opened module bug *)
(* Module Util with wrap_error function *)
module Util = struct
  let wrap_error msg result = result
  let another_func x = x + 1
end

(* Open the Util module *)
open Util

(* Use wrap_error via pipe operator - should NOT be reported as unused *)
let test_pipe_operator () =
  let result = Ok 42 in
  result |> wrap_error "error message"

(* Use another_func via pipe operator - should NOT be reported as unused *)
let test_pipe_operator_2 () = 5 |> another_func

(* This function is truly unused - should be reported as unused *)
let unused_function x = x - 1
