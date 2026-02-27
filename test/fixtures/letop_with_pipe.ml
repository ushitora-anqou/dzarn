(* Test fixture for let* syntax with opened module *)
module Util = struct
  let wrap_error msg result = result
  let another_func x = x + 1
end

open Util

(* Use wrap_error via let* with pipe operator - should NOT be reported as unused *)
let test_letop_with_pipe () =
  let result = Ok 42 in
  let* x = result |> wrap_error "error message" in
  Ok x

(* Use another_func via let* with pipe operator - should NOT be reported as unused *)
let test_letop_with_pipe_2 () =
  let* y = 5 |> another_func in
  Ok y

(* This function is truly unused - should be reported as unused *)
let unused_function x = x - 1
