(* Test fixture for qualified module call bug *)
(* Bug: when A.record is called, it incorrectly marks top-level 'record' as used *)

(* Module A with a 'record' function *)
module A = struct
  let record x = x + 1
end

(* Module B with a 'helper' function *)
module B = struct
  let helper x = x + 2
end

(* Call to A.record - BUG: this incorrectly marks top-level 'record' as used *)
let test_func () =
  A.record 42;
  B.helper 10

(* Top-level functions - should be detected as unused but aren't due to the bug *)
let record x = x + 3
let helper x = x + 4
