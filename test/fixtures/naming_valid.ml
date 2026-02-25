(* Test fixture for valid naming conventions *)

(* All variables/functions are lowercase snake_case *)
let my_function x = x + 1
let another_function (param_one : int) (param_two : int) = param_one + param_two
let calculate_result () = 42

(* All variant constructors are uppercase snake_case *)
type my_variant = First_case | Second_case | Third_case
type another_type = Valid_constructor | Another_valid_case | Yet_another_one

(* All exceptions are uppercase snake_case *)
exception My_exception
exception Another_valid_exception

(* Pattern matching with valid names *)
let check_value x =
  match x with First_case -> 1 | Second_case -> 2 | Third_case -> 3

(* Let bindings with valid names *)
let () =
  let local_var = 1 in
  let another_local = 2 in
  ()

(* Lambda parameters with valid names *)
let my_lambda = fun (good_param : int) -> good_param + 1
let another_lambda = fun (arg_one : int) (arg_two : int) -> arg_one + arg_two

(* Record with valid field access *)
type point = { x : int; y : int }

let get_x p = p.x
let get_y p = p.y

(* All functions are used *)
let () =
  let _ = my_function 10 in
  let _ = another_function 1 2 in
  let _ = calculate_result () in
  let _ = check_value First_case in
  let _ = my_lambda 5 in
  let _ = another_lambda 1 2 in
  let pt = { x = 10; y = 20 } in
  let _ = get_x pt in
  let _ = get_y pt in
  ()
