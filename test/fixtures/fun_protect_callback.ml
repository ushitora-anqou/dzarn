(* Test fixture for Fun.protect with callback function *)
module Usecase = struct
  module Stream_service = struct
    let stop _ = ()
    let another_func _ = ()
  end
end

(* Test case: Fun.protect with callback in ~finally argument *)
(* Usecase.Stream_service.stop should NOT be reported as unused *)
let test_fun_protect_finally () =
  Fun.protect ~finally:(fun () -> Usecase.Stream_service.stop ()) @@ fun () ->
  ()

(* Test case: Fun.protect with callback in ~finally argument with qualified call *)
let test_fun_protect_finally_qualified _subscription =
  Fun.protect ~finally:(fun () -> Usecase.Stream_service.stop _subscription)
  @@ fun () -> ()

(* Test case: Nested function calls in callback *)
let test_nested_callback _subscription =
  Fun.protect ~finally:(fun () ->
      Usecase.Stream_service.stop _subscription;
      Usecase.Stream_service.another_func ())
  @@ fun () -> ()

(* Test case: Callback without any function call - should be unused *)
let unused_function x = x + 1
