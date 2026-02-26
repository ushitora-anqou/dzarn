(* Naming convention linter *)

(* Check if a character is uppercase *)
let is_uppercase c = 'A' <= c && c <= 'Z'

(* Check if a character is lowercase *)
let is_lowercase c = 'a' <= c && c <= 'z'

(* Check if a character is valid in snake_case (lowercase, digit, or underscore) *)
let is_snake_char c = is_lowercase c || ('0' <= c && c <= '9') || c = '_'

(* Check if a string is lowercase snake_case *)
let is_lowercase_snake_case s =
  if Stdlib.String.length s = 0 then false
  else
    let first = s.[0] in
    (* First char must be lowercase letter *)
    if not (is_lowercase first) then false
    else
      (* All chars must be snake_case chars *)
      let rec check i =
        if i >= Stdlib.String.length s then true
        else if is_snake_char s.[i] then check (i + 1)
        else false
      in
      check 0

(* Check if a string is uppercase snake_case (first char uppercase, rest snake_case) *)
let is_uppercase_snake_case s =
  if Stdlib.String.length s = 0 then false
  else
    let first = s.[0] in
    (* First char must be uppercase letter *)
    if not (is_uppercase first) then false
    else
      (* All chars must be snake_case chars (lowercase, digit, underscore) *)
      let rec check i =
        if i >= Stdlib.String.length s then true
        else if is_snake_char s.[i] then check (i + 1)
        else false
      in
      check 1
(* Skip first char since we already validated it *)

(* Helper to create a naming violation *)
let make_violation name loc violation_type file =
  { Types.name; Types.loc; Types.violation_type; Types.source_file = file }

(* Check if a name is private (starts with underscore) *)
let is_private_name name = Stdlib.String.length name > 0 && name.[0] = '_'

(* Check polymorphic variant name - should be uppercase snake_case *)
let check_poly_variant_name violations name loc file =
  if not (is_uppercase_snake_case name) then
    let violation_loc =
      {
        Types.file = loc.Ppxlib.Location.loc_start.pos_fname;
        line = loc.Ppxlib.Location.loc_start.pos_lnum;
        column =
          loc.Ppxlib.Location.loc_start.pos_cnum
          - loc.Ppxlib.Location.loc_start.pos_bol;
      }
    in
    violations :=
      make_violation ("`" ^ name) violation_loc
        "polymorphic variant should be uppercase snake_case" file
      :: !violations

(* Check pattern for naming violations (variables, function parameters) *)
let rec check_pattern violations file = function
  | {
      Ppxlib.Parsetree.ppat_desc = Ppxlib.Parsetree.Ppat_var { txt = name; _ };
      ppat_loc = loc;
      _;
    } ->
      (* Skip private names *)
      if not (is_private_name name) then
        (* Check if variable name is lowercase snake_case *)
        if not (is_lowercase_snake_case name) then
          let loc =
            {
              Types.file = loc.Ppxlib.Location.loc_start.pos_fname;
              line = loc.Ppxlib.Location.loc_start.pos_lnum;
              column =
                loc.Ppxlib.Location.loc_start.pos_cnum
                - loc.Ppxlib.Location.loc_start.pos_bol;
            }
          in
          violations :=
            make_violation name loc
              "variable/function name should be lowercase snake_case" file
            :: !violations
  | {
      Ppxlib.Parsetree.ppat_desc =
        Ppxlib.Parsetree.Ppat_alias (pat, { txt = name; _ });
      _;
    } ->
      (* Skip private names *)
      if not (is_private_name name) then
        (* Check alias names *)
        if not (is_lowercase_snake_case name) then (
          let loc =
            {
              Types.file = pat.Ppxlib.Parsetree.ppat_loc.loc_start.pos_fname;
              line = pat.Ppxlib.Parsetree.ppat_loc.loc_start.pos_lnum;
              column =
                pat.Ppxlib.Parsetree.ppat_loc.loc_start.pos_cnum
                - pat.Ppxlib.Parsetree.ppat_loc.loc_start.pos_bol;
            }
          in
          violations :=
            make_violation name loc "alias name should be lowercase snake_case"
              file
            :: !violations;
          check_pattern violations file pat)
        else check_pattern violations file pat
      else check_pattern violations file pat
  | { Ppxlib.Parsetree.ppat_desc = Ppxlib.Parsetree.Ppat_variant (_, pat); _ }
    ->
      (* Check polymorphic variant name - skipping for now *)
      (* The name is in the location but extracting it is complex *)
      Option.iter (check_pattern violations file) pat
  (* Skip other pattern types for simplicity *)
  | _ -> ()

(* Check expression for function parameters in lambdas *)
let rec check_expression violations file = function
  | {
      Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_function (_, _, body);
      _;
    } -> (
      match body with
      | Pfunction_body e -> check_expression violations file e
      | Pfunction_cases (cases, _, _) ->
          Stdlib.List.iter
            (fun c ->
              check_pattern violations file c.Ppxlib.Parsetree.pc_lhs;
              Option.iter
                (check_expression violations file)
                c.Ppxlib.Parsetree.pc_guard;
              check_expression violations file c.Ppxlib.Parsetree.pc_rhs)
            cases)
  | { pexp_desc = Ppxlib.Parsetree.Pexp_match (e, cases); _ } ->
      check_expression violations file e;
      Stdlib.List.iter
        (fun c ->
          check_pattern violations file c.Ppxlib.Parsetree.pc_lhs;
          Option.iter
            (check_expression violations file)
            c.Ppxlib.Parsetree.pc_guard;
          check_expression violations file c.Ppxlib.Parsetree.pc_rhs)
        cases
  | { pexp_desc = Ppxlib.Parsetree.Pexp_try (e, cases); _ } ->
      check_expression violations file e;
      Stdlib.List.iter
        (fun c ->
          check_pattern violations file c.Ppxlib.Parsetree.pc_lhs;
          Option.iter
            (check_expression violations file)
            c.Ppxlib.Parsetree.pc_guard;
          check_expression violations file c.Ppxlib.Parsetree.pc_rhs)
        cases
  | { pexp_desc = Ppxlib.Parsetree.Pexp_let (_, bindings, e); _ } ->
      Stdlib.List.iter
        (fun binding ->
          check_pattern violations file binding.Ppxlib.Parsetree.pvb_pat;
          check_expression violations file binding.Ppxlib.Parsetree.pvb_expr)
        bindings;
      check_expression violations file e
  | { pexp_desc = Ppxlib.Parsetree.Pexp_letmodule (_, _, e); _ } ->
      check_expression violations file e
  | { pexp_desc = Ppxlib.Parsetree.Pexp_letexception (_, e); _ } ->
      check_expression violations file e
  | { pexp_desc = Ppxlib.Parsetree.Pexp_ifthenelse (e1, e2, e3); _ } ->
      check_expression violations file e1;
      check_expression violations file e2;
      Option.iter (check_expression violations file) e3
  | { pexp_desc = Ppxlib.Parsetree.Pexp_sequence (e1, e2); _ } ->
      check_expression violations file e1;
      check_expression violations file e2
  | { pexp_desc = Ppxlib.Parsetree.Pexp_while (e1, e2); _ } ->
      check_expression violations file e1;
      check_expression violations file e2
  | { pexp_desc = Ppxlib.Parsetree.Pexp_for (_, _, _, _, e); _ } ->
      check_expression violations file e
  | { pexp_desc = Ppxlib.Parsetree.Pexp_apply (e, args); _ } ->
      check_expression violations file e;
      Stdlib.List.iter
        (fun (_, arg) -> check_expression violations file arg)
        args
  | { pexp_desc = Ppxlib.Parsetree.Pexp_tuple el; _ } ->
      Stdlib.List.iter (fun e -> check_expression violations file e) el
  | { pexp_desc = Ppxlib.Parsetree.Pexp_array el; _ } ->
      Stdlib.List.iter (check_expression violations file) el
  | { pexp_desc = Ppxlib.Parsetree.Pexp_record (fields, _); _ } ->
      Stdlib.List.iter
        (fun (_, expr) -> check_expression violations file expr)
        fields
  | { pexp_desc = Ppxlib.Parsetree.Pexp_field (e, _); _ } ->
      check_expression violations file e
  | { pexp_desc = Ppxlib.Parsetree.Pexp_setfield (e1, _, e2); _ } ->
      check_expression violations file e1;
      check_expression violations file e2
  | { pexp_desc = Ppxlib.Parsetree.Pexp_construct (_, Some e); _ } ->
      check_expression violations file e
  | { pexp_desc = Ppxlib.Parsetree.Pexp_construct (_, None); _ } -> ()
  | { pexp_desc = Ppxlib.Parsetree.Pexp_variant (_, e); _ } ->
      (* Check polymorphic variant name - skipping for now *)
      (* The name is in the location but extracting it is complex *)
      Option.iter (check_expression violations file) e
  | { pexp_desc = Ppxlib.Parsetree.Pexp_assert e; _ } ->
      check_expression violations file e
  | { pexp_desc = Ppxlib.Parsetree.Pexp_lazy e; _ } ->
      check_expression violations file e
  | { pexp_desc = Ppxlib.Parsetree.Pexp_poly (e, _); _ } ->
      check_expression violations file e
  | { pexp_desc = Ppxlib.Parsetree.Pexp_open (_, e); _ } ->
      check_expression violations file e
  | { pexp_desc = Ppxlib.Parsetree.Pexp_send (e, _); _ } ->
      check_expression violations file e
  (* Skip other expression types *)
  | _ -> ()

(* Check type declaration for variant constructor names *)
let check_type_decl violations file
    ({ ptype_kind; _ } : Ppxlib.Parsetree.type_declaration) =
  match ptype_kind with
  | Ppxlib.Parsetree.Ptype_variant constructors ->
      Stdlib.List.iter
        (fun ({ pcd_name = { txt = ctor_name; _ }; pcd_loc; _ } :
               Ppxlib.Parsetree.constructor_declaration) ->
          (* Check if variant constructor name is uppercase snake_case *)
          if not (is_uppercase_snake_case ctor_name) then
            let loc =
              {
                Types.file = pcd_loc.loc_start.pos_fname;
                line = pcd_loc.loc_start.pos_lnum;
                column = pcd_loc.loc_start.pos_cnum - pcd_loc.loc_start.pos_bol;
              }
            in
            violations :=
              make_violation ctor_name loc
                "variant constructor should be uppercase snake_case" file
              :: !violations)
        constructors
  | Ppxlib.Parsetree.Ptype_record _ ->
      (* Record fields are typically lowercase, but we check them as variables *)
      ()
  | Ppxlib.Parsetree.Ptype_open -> ()
  | Ppxlib.Parsetree.Ptype_abstract -> ()

(* Skip checking polymorphic variants in type declarations for now *)
(* The structure varies by OCaml version and is complex to extract *)

(* Check structure item for naming violations *)
let check_structure_item violations file = function
  | {
      Ppxlib.Parsetree.pstr_desc = Ppxlib.Parsetree.Pstr_value (_, bindings);
      _;
    } ->
      Stdlib.List.iter
        (fun binding ->
          check_pattern violations file binding.Ppxlib.Parsetree.pvb_pat;
          check_expression violations file binding.Ppxlib.Parsetree.pvb_expr)
        bindings
  | {
      Ppxlib.Parsetree.pstr_desc = Ppxlib.Parsetree.Pstr_type (_, type_decls);
      _;
    } ->
      Stdlib.List.iter (check_type_decl violations file) type_decls
  | _ -> ()

(* Collect all naming violations from files *)
let collect_naming_violations parse_ml files =
  let ml_files =
    Stdlib.List.filter (fun f -> Filename.check_suffix f ".ml") files
  in
  let violations = ref [] in
  Stdlib.List.iter
    (fun file ->
      match parse_ml file with
      | Error _ -> ()
      | Ok ast -> Stdlib.List.iter (check_structure_item violations file) ast)
    ml_files;
  Stdlib.List.rev !violations

(* Report naming violations *)
let report_naming_violations violations =
  Stdlib.List.iter
    (fun violation ->
      Stdlib.Printf.printf "Naming violation: %s at %s:%d:%d\n%s\n"
        violation.Types.name violation.Types.source_file
        violation.Types.loc.line violation.Types.loc.column
        violation.Types.violation_type)
    violations;
  Stdlib.flush stdout
