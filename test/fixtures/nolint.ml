(* Test file for nolint attributes *)

(* Expression-level nolint for naming violations *)
let test_function () =
  let (badLocal [@nolint "naming"]) = 5 in
  badLocal + 1

(* Item-level nolint for naming violations *)
let anotherBadFunction x = x + 3 [@@nolint "naming"]

(* This should NOT be suppressed and should be reported *)
let camelCaseFunction x = x + 2
