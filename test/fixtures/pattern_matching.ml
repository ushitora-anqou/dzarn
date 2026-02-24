(* Test function usage in pattern matching contexts *)

(* Function used in pattern match guard *)
let check_positive x = x > 0

let test_guard x =
  match x with _ when check_positive x -> "positive" | _ -> "not positive"

(* Function used in pattern match result expressions *)
let double x = x * 2
let pattern_result x = match x with 0 -> double 0 | n -> double n

(* Function used in nested pattern matching *)
let get_default () = 42

let nested_match x y =
  match (x, y) with 0, 0 -> get_default () | x, y -> x + y

(* Main function that uses the above *)
let main () =
  let _ = test_guard 10 in
  let _ = pattern_result 5 in
  let _ = nested_match 1 2 in
  ()
