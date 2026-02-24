(* Comprehensive AST node coverage test *)
(* Tests that functions called within various OCaml syntax constructs are tracked *)

(* 1. Helper function called in local module context *)
let helper_for_local x = x + 1

module LocalMod = struct
  let use_helper x = helper_for_local x
end

let use_local_module () = LocalMod.use_helper 5

(* 2. Helper function called in functor *)
let functor_helper s = String.length s

module StringLen (X : sig end) = struct
  let len s = functor_helper s
end

module StringLenInst = StringLen (struct end)

let use_functor () = StringLenInst.len "hello"

(* 3. Helper function called in class method *)
let class_helper x = x + 10

module Counter = struct
  class counter =
    object
      val count = ref 0
      method get = !count
      method inc = class_helper !count
    end
end

let use_class () =
  let c = new Counter.counter in
  c#inc;
  c#get

(* 4. Helper function called in try-with handler *)
let handle_error () = Printf.printf "Error occurred\n"

let try_with_test x =
  try x + 1
  with _ ->
    handle_error ();
    0

(* 5. Helper function called in object expression *)
let object_helper x = x * 2

let obj_expr () =
  object
    val x = object_helper 5
    method get_x = x
  end

(* 6. Helper function called in let-open scope *)
let let_open_helper s = s ^ "!"

let let_open_test x =
  let open String in
  let_open_helper (concat " " [ x ])

(* 7. Helper function called in loop body *)
let loop_helper i = i * 2

let complex_loop () =
  let acc = ref 0 in
  for i = 0 to 10 do
    acc := !acc + loop_helper i
  done;
  !acc

(* 8. Helper function called in record construction *)
type point = { x : int; y : int }

let record_x x = x + 1
let record_y y = y + 2
let nested_record () = { x = record_x 1; y = record_y 2 }

(* 9. Helper function called in array operation *)
let array_helper x = x + 10

let array_ops () =
  let arr = [| 1; 2; 3 |] in
  Array.map array_helper arr |> Array.length

(* 10. Helper function called in list operation *)
let list_helper x = x * 3
let list_ops () = [ 1; 2; 3 ] |> List.map list_helper |> List.length

(* 11. Helper function called in option pattern matching *)
let option_helper x = x + 100

let option_ops () =
  let f = function Some x -> option_helper x | None -> 0 in
  f (Some 5)

(* 12. Helper function called in lazy evaluation *)
let lazy_helper x = x + 1000

let lazy_test () =
  let l = lazy (lazy_helper 42) in
  Lazy.force l

(* 13. Helper function used in function composition *)
let compose_helper f g x = f (g x)

let use_compose () =
  let add5 x = x + 5 in
  let times2 x = x * 2 in
  compose_helper times2 add5 10

(* 14. Helper function called in recursive function *)
let rec_helper n = n + 1
let rec factorial n = if n <= 1 then 1 else n * factorial (rec_helper n)
let use_factorial () = factorial 5

(* 15. Helper function called in mutually recursive functions *)
let mut_helper n = n - 1

let rec even n =
  if n = 0 then true else if n = 1 then false else even (mut_helper n)

and odd n = if n = 0 then false else if n = 1 then true else odd (mut_helper n)

let use_mutual () = even 3

(* 16. Helper function called in sequence expression *)
let seq_helper () = Printf.printf "seq\n"

let seq_test () =
  seq_helper ();
  42

(* 17. Helper function called in if-then-else *)
let if_helper x = x + 5
let if_test x = if x > 0 then if_helper x else 0

(* 18. Helper function called in assert *)
let assert_helper x = x <> 0
let assert_test x = if assert_helper x then x else failwith "zero"

(* 19. Main function using various constructs *)
let main () =
  use_local_module ();
  use_functor ();
  use_class ();
  try_with_test 10;
  obj_expr ();
  let_open_test "test";
  complex_loop ();
  nested_record ();
  array_ops ();
  list_ops ();
  option_ops ();
  lazy_test ();
  use_compose ();
  use_factorial ();
  use_mutual ();
  seq_test ();
  if_test 5;
  assert_test 10

(* 20. Unused function - should be reported as unused *)
let unused_function x = x + 1
