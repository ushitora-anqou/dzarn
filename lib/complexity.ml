(* Cyclomatic complexity calculator *)

(* Calculate cyclomatic complexity of an expression *)
(* Base complexity is 1, additional complexity for decision points *)
let rec complexity_of_expr = function
  | {
      Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_construct (_, payload);
      _;
    } -> (
      match payload with Some e -> complexity_of_expr e | None -> 1)
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_tuple items; _ } ->
      (* Tuple doesn't add complexity *)
      Stdlib.List.fold_left
        (fun acc e -> max acc (complexity_of_expr e))
        1 items
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_record (fields, _); _ }
    ->
      (* Record doesn't add complexity *)
      Stdlib.List.fold_left
        (fun acc (_label, expr) -> max acc (complexity_of_expr expr))
        1 fields
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_array items; _ } ->
      Stdlib.List.fold_left
        (fun acc e -> max acc (complexity_of_expr e))
        1 items
  | {
      Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_ifthenelse (e1, e2, e3);
      _;
    } ->
      (* if-then-else: condition + branches, adds 1 for the decision *)
      1
      + max (complexity_of_expr e1)
          (max (complexity_of_expr e2)
             (match e3 with Some e -> complexity_of_expr e | None -> 1))
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_sequence (e1, e2); _ }
    ->
      (* Sequence doesn't add complexity, take max *)
      max (complexity_of_expr e1) (complexity_of_expr e2)
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_while (e1, e2); _ } ->
      (* While loop adds 1 *)
      1 + max (complexity_of_expr e1) (complexity_of_expr e2)
  | {
      Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_for (_, _, _, _, e);
      _;
    } ->
      (* For loop adds 1 *)
      1 + complexity_of_expr e
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_match (e, cases); _ }
    ->
      (* Match adds 1 for the decision point *)
      let case_complexity =
        Stdlib.List.fold_left
          (fun acc c ->
            max acc
              (complexity_of_expr c.Ppxlib.Parsetree.pc_rhs
              + (function Some _ -> 1 | None -> 0) c.Ppxlib.Parsetree.pc_guard))
          1 cases
      in
      max (complexity_of_expr e) (1 + case_complexity)
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_try (e, cases); _ } ->
      (* Try-with adds 1 for the decision point *)
      let handler_complexity =
        Stdlib.List.fold_left
          (fun acc c ->
            max acc
              (complexity_of_expr c.Ppxlib.Parsetree.pc_rhs
              + (function Some _ -> 1 | None -> 0) c.Ppxlib.Parsetree.pc_guard))
          1 cases
      in
      max (complexity_of_expr e) (1 + handler_complexity)
  | {
      Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_function (_, _, body);
      _;
    } -> (
      match body with
      | Ppxlib.Parsetree.Pfunction_body e -> complexity_of_expr e
      | Ppxlib.Parsetree.Pfunction_cases (cases, _, _) ->
          (* Function match adds 1 for the decision point *)
          let case_complexity =
            Stdlib.List.fold_left
              (fun acc c ->
                max acc
                  (complexity_of_expr c.Ppxlib.Parsetree.pc_rhs
                  + (function Some _ -> 1 | None -> 0)
                      c.Ppxlib.Parsetree.pc_guard))
              1 cases
          in
          1 + case_complexity)
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_apply (e, args); _ } ->
      Stdlib.List.fold_left
        (fun acc (_, arg) -> max acc (complexity_of_expr arg))
        (complexity_of_expr e) args
  | {
      Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_let (_, bindings, e);
      _;
    } ->
      let bindings_complexity =
        Stdlib.List.fold_left
          (fun acc binding ->
            max acc (complexity_of_expr binding.Ppxlib.Parsetree.pvb_expr))
          1 bindings
      in
      max bindings_complexity (complexity_of_expr e)
  | {
      Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_letmodule (_, _, e);
      _;
    } ->
      complexity_of_expr e
  | {
      Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_letexception (_, e);
      _;
    } ->
      complexity_of_expr e
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_assert e; _ } ->
      complexity_of_expr e
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_lazy e; _ } ->
      complexity_of_expr e
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_poly (e, _); _ } ->
      complexity_of_expr e
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_open (_, e); _ } ->
      complexity_of_expr e
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_new _; _ } -> 1
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_send (e, _); _ } ->
      complexity_of_expr e
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_field (e, _); _ } ->
      complexity_of_expr e
  | {
      Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_setfield (e1, _, e2);
      _;
    } ->
      max (complexity_of_expr e1) (complexity_of_expr e2)
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_object _; _ } -> 1
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_pack _; _ } -> 1
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_letop _; _ } -> 1
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_constant _; _ } -> 1
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_extension (_, _); _ }
    ->
      1
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_unreachable; _ } -> 1
  | { Ppxlib.Parsetree.pexp_desc = Ppxlib.Parsetree.Pexp_ident _; _ } -> 1
  | _ -> 1

(* Calculate complexity of a structure item *)
let complexity_of_structure_item mod_name file item :
    Types.complexity_issue list =
  match item.Ppxlib.Parsetree.pstr_desc with
  | Pstr_value (_, bindings) ->
      Stdlib.List.map
        (fun binding ->
          match binding.Ppxlib.Parsetree.pvb_pat.Ppxlib.Parsetree.ppat_desc with
          | Ppat_var { txt = name; _ } ->
              ({
                 id =
                   {
                     Types.module_name = mod_name;
                     name;
                     loc =
                       {
                         Types.file;
                         line = binding.pvb_pat.ppat_loc.loc_start.pos_lnum;
                         column =
                           binding.pvb_pat.ppat_loc.loc_start.pos_cnum
                           - binding.pvb_pat.ppat_loc.loc_start.pos_bol;
                       };
                   };
                 complexity = complexity_of_expr binding.pvb_expr;
                 source_file = file;
               }
                : Types.complexity_issue)
          | _ ->
              ({
                 id =
                   {
                     Types.module_name = mod_name;
                     name = "<unknown>";
                     loc =
                       {
                         Types.file;
                         line = binding.pvb_pat.ppat_loc.loc_start.pos_lnum;
                         column =
                           binding.pvb_pat.ppat_loc.loc_start.pos_cnum
                           - binding.pvb_pat.ppat_loc.loc_start.pos_bol;
                       };
                   };
                 complexity = complexity_of_expr binding.pvb_expr;
                 source_file = file;
               }
                : Types.complexity_issue))
        bindings
  | _ -> []

(* Collect complexity for all functions in files *)
let collect_complexity parse_ml module_name files =
  let ml_files =
    Stdlib.List.filter (fun f -> Filename.check_suffix f ".ml") files
  in
  let results = ref [] in
  Stdlib.List.iter
    (fun file ->
      match parse_ml file with
      | Result.Error _ -> ()
      | Result.Ok ast ->
          let mod_name = module_name file in
          Stdlib.List.iter
            (fun item ->
              let complexities =
                complexity_of_structure_item mod_name file item
              in
              results := complexities @ !results)
            ast)
    ml_files;
  !results

(* Find functions exceeding threshold *)
let find_complex_functions threshold all_complexities =
  Stdlib.List.filter
    (fun issue -> issue.Types.complexity > threshold)
    all_complexities

(* Report complex functions with threshold *)
let report_complex_with_threshold threshold
    (issues : Types.complexity_issue list) =
  Stdlib.List.iter
    (fun (issue : Types.complexity_issue) ->
      Printf.printf "Function '%s' has complexity %d (threshold: %d) in %s:%d\n"
        issue.id.name issue.Types.complexity threshold issue.source_file
        issue.id.loc.line)
    issues;
  flush stdout
