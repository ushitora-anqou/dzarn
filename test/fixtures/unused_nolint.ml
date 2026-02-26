(* Test fixture for unused nolint detection *)

(* This nolint is unused - there's no naming violation *)
let good_name x = x + 1 [@@nolint "naming"]

(* This nolint is unused - complexity is below threshold *)
let simple () = 1 [@@nolint "complexity"]

(* This nolint is unused - length is below threshold *)
let short () = 2 [@@nolint "length"]

(* Expression-level unused nolint *)
let another_good () =
  let (x [@nolint "naming"]) = 5 in
  x + 1

(* This nolint IS used - suppresses naming violation *)
let camelCase x = x + 2 [@@nolint "naming"]
