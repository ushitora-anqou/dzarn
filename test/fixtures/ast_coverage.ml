(* Comprehensive AST node coverage test *)

(* 1. Pattern matching *)
let match_test x = match x with 0 -> "zero" | 1 -> "one" | _ -> "other"

(* 2. Local functions (let-in) *)
let local_function_outer x =
  let helper y = y * 2 in
  helper x + 1

(* 3. Nested local functions *)
let nested_local x =
  let inner z = z + 1 in
  let wrapper y = inner y in
  wrapper x

(* 4. Functions as values (higher-order) *)
let apply_twice f x = f (f x)
let add_one n = n + 1

(* 5. Composition *)
let compose f g x = f (g x)

(* 6. Operators *)
let ( +@ ) x y = x ^ y

(* 7. Tuples *)
let tuple_func (x, y) = x + y

(* 8. Records *)
let point_field { x; y } = x + y
let create_point x y = { x; y }

(* 9. Arrays *)
let array_get arr i = arr.(i)
let array_set arr i v = arr.(i) <- v

(* 10. Option handling *)
let opt_func = function Some v -> v + 1 | None -> 0

(* 11. Result handling *)
let result_func = function Ok v -> v + 1 | Error _ -> 0

(* 12. Lists *)
let list_sum lst = List.fold_left (fun acc x -> acc + x) 0 lst

(* 13. Sequences *)
let seq_func () =
  print_endline "first";
  print_endline "second"

(* 14. Loops *)
let loop_test n =
  let acc = ref 0 in
  for i = 0 to n do
    acc := !acc + i
  done;
  while !acc > 0 do
    acc := !acc - 1
  done;
  !acc

(* 15. Conditionals *)
let cond_test x y = if x > 0 then y else y * 2

(* 16. Try-with *)
let try_test x = try x / 0 with Division_by_zero -> 0

(* 17. Assert *)
let assert_test x =
  assert (x > 0);
  x + 1

(* 18. Lazy values *)
let lazy_value = lazy 42

(* 19. Functors (module-level) *)
module type Comparable = sig
  type t

  val compare : t -> t -> int
end

module MakeComparable (X : Comparable) = struct
  let lt x y = X.compare x y < 0
end

(* 20. Classes *)
module Counter = struct
  class count =
    object
      val mutable count = 0
      method get = count
      method inc = count <- count + 1
    end
end

let create_counter () = new Counter.count

(* 21. Module includes *)
include struct
  let included_val = 42
end

(* 22. Used function to verify analysis works *)
let used_in_coverage = match_test 10 + 1
