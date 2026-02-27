(* Test file for preserving comments during --fix *)

(* This is a module-level comment *)
let used_function x = x + 1

(** This is an unused function with documentation comment. It should be removed,
    but comments in other places should be preserved. *)
let unused_function_with_doc x = x * 2

(* Another unused function *)
let another_unused x = x - 1

(* This function is used *)
let another_used x = used_function x

(* Comment at the end of file *)
let final_function x = x + 10
