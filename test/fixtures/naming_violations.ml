(* Test fixture for naming convention violations *)

(* Variable/function name violations - should be lowercase snake_case *)

let camelCase x = x + 1
let pascalCase x = x * 2
let mixedCase () = 42
let anotherBadOne x y = x + y

(* Valid lowercase snake_case variables *)
let good_function x = x + 1
let another_good_func () = 42

(* Variant constructor violations - should be uppercase snake_case *)

type my_variant = Lower_case | CamelCaseVariant | Mixed_case_variant
type another_type = Good_case | Another_good | Bad_variant

(* Exception violations - should be uppercase snake_case *)

exception Bad_exception_name
exception AnotherBadOne
exception Good_exception

(* Function parameters with violations *)
let bad_param camelParam = camelParam + 1
let good_param good_param = good_param + 1

(* Pattern matching - using strings for demo *)
let test_pattern x = match x with "first" -> 1 | "second" -> 2 | _ -> 0

(* Let bindings with violations *)
let () =
  let badLocal = 1 in
  let good_local = 2 in
  ()

(* Lambda parameters with violations *)
let lambda = fun badArg -> badArg + 1
let good_lambda = fun good_arg -> good_arg + 1

(* Unused function - for testing combined linters *)
let unused_naming_function () = 999
