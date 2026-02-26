(* Test fixture for used nolint validation *)

(* This nolint IS used - suppresses real naming violation *)
let camelCase x = x + 1 [@@nolint "naming"]

(* This nolint IS used - suppresses complexity violation *)
let complex () = if true then 1 else if false then 2 else 3
[@@nolint "complexity"]
