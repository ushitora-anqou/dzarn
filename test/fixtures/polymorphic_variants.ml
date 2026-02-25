(* Test fixture for polymorphic variants *)

(* Valid polymorphic variants - uppercase snake_case *)
type valid_poly = [ `Valid_case | `Another_good ]

(* Invalid polymorphic variants - would be violations if we could extract names *)
(* type invalid_poly = [ `camelCase | `lower_case ] *)
(* Note: Currently, polymorphic variants in type declarations are not checked *)
(* due to AST structure complexity *)

(* Polymorphic variant usage in patterns - not currently checked *)
let check_poly (x : [ `A | `B ]) = match x with `A -> 1 | `B -> 2

(* Polymorphic variant usage in expressions - not currently checked *)
let value = `A
