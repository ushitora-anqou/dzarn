(* Function length checker *)

open Types

(* Nolint suppression support *)
type suppression_state = {
  suppressed_linters_by_line : (int * Nolint.linter list) list;
  usage_tracker : Nolint.usage_tracker option;
}

(* Check if a location is suppressed for the length linter *)
let is_suppressed state loc =
  Nolint.is_location_suppressed state.suppressed_linters_by_line loc
    Nolint.Length

(* Check if suppressed and mark as used *)
let is_suppressed_and_track state loc =
  let suppressed = is_suppressed state loc in
  (if suppressed then
     match state.usage_tracker with
     | None -> ()
     | Some tracker ->
         let line = Nolint.line_of_loc loc in
         Nolint.mark_nolint_used tracker line "length");
  suppressed

(* Calculate line count of an expression from its location *)
let line_count_of_expr expr =
  let start_line =
    expr.Ppxlib.Parsetree.pexp_loc.Ppxlib.Location.loc_start.pos_lnum
  in
  let end_line =
    expr.Ppxlib.Parsetree.pexp_loc.Ppxlib.Location.loc_end.pos_lnum
  in
  end_line - start_line + 1

(* Collect length from a single binding (top-level or nested) *)
let collect_binding state mod_name file results binding =
  match binding.Ppxlib.Parsetree.pvb_pat.Ppxlib.Parsetree.ppat_desc with
  | Ppxlib.Parsetree.Ppat_var { txt = name; _ } ->
      let loc = binding.Ppxlib.Parsetree.pvb_pat.Ppxlib.Parsetree.ppat_loc in
      (* Check if suppressed by nolint attribute *)
      if not (is_suppressed_and_track state loc) then
        let line_count = line_count_of_expr binding.Ppxlib.Parsetree.pvb_expr in
        results :=
          {
            id =
              {
                Types.module_name = mod_name;
                name;
                loc =
                  {
                    Types.file;
                    line = loc.loc_start.pos_lnum;
                    column = loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
                  };
              };
            line_count;
            source_file = file;
          }
          :: !results
  | _ -> ()

(* Recursively find nested let bindings in an expression *)
let rec find_nested_bindings state mod_name file results expr =
  (* Check if this is a let expression with nested functions *)
  match expr.Ppxlib.Parsetree.pexp_desc with
  | Ppxlib.Parsetree.Pexp_let (_, bindings, body) ->
      (* Collect the nested bindings *)
      List.iter (collect_binding state mod_name file results) bindings;
      (* Also check the body for more nested bindings *)
      List.iter
        (fun binding ->
          find_nested_bindings state mod_name file results
            binding.Ppxlib.Parsetree.pvb_expr)
        bindings;
      (* Recursively check the body expression *)
      find_nested_bindings state mod_name file results body
  | Ppxlib.Parsetree.Pexp_function (_, _, body) -> (
      match body with
      | Ppxlib.Parsetree.Pfunction_body e ->
          find_nested_bindings state mod_name file results e
      | Ppxlib.Parsetree.Pfunction_cases (cases, _, _) ->
          List.iter
            (fun c ->
              Option.iter
                (fun e -> find_nested_bindings state mod_name file results e)
                c.Ppxlib.Parsetree.pc_guard;
              find_nested_bindings state mod_name file results
                c.Ppxlib.Parsetree.pc_rhs)
            cases)
  | Ppxlib.Parsetree.Pexp_match (e, cases) ->
      find_nested_bindings state mod_name file results e;
      List.iter
        (fun c ->
          Option.iter
            (fun e -> find_nested_bindings state mod_name file results e)
            c.Ppxlib.Parsetree.pc_guard;
          find_nested_bindings state mod_name file results
            c.Ppxlib.Parsetree.pc_rhs)
        cases
  | Ppxlib.Parsetree.Pexp_try (e, cases) ->
      find_nested_bindings state mod_name file results e;
      List.iter
        (fun c ->
          Option.iter
            (fun e -> find_nested_bindings state mod_name file results e)
            c.Ppxlib.Parsetree.pc_guard;
          find_nested_bindings state mod_name file results
            c.Ppxlib.Parsetree.pc_rhs)
        cases
  | Ppxlib.Parsetree.Pexp_ifthenelse (e1, e2, e3) ->
      find_nested_bindings state mod_name file results e1;
      find_nested_bindings state mod_name file results e2;
      Option.iter (find_nested_bindings state mod_name file results) e3
  | Ppxlib.Parsetree.Pexp_sequence (e1, e2) ->
      find_nested_bindings state mod_name file results e1;
      find_nested_bindings state mod_name file results e2
  | Ppxlib.Parsetree.Pexp_while (e1, e2) ->
      find_nested_bindings state mod_name file results e1;
      find_nested_bindings state mod_name file results e2
  | Ppxlib.Parsetree.Pexp_for (_, _, _, _, e) ->
      find_nested_bindings state mod_name file results e
  | Ppxlib.Parsetree.Pexp_apply (e, args) ->
      find_nested_bindings state mod_name file results e;
      List.iter
        (fun (_, arg) -> find_nested_bindings state mod_name file results arg)
        args
  | Ppxlib.Parsetree.Pexp_tuple items ->
      List.iter (find_nested_bindings state mod_name file results) items
  | Ppxlib.Parsetree.Pexp_array items ->
      List.iter (find_nested_bindings state mod_name file results) items
  | Ppxlib.Parsetree.Pexp_record (fields, _) ->
      List.iter
        (fun (_, expr) -> find_nested_bindings state mod_name file results expr)
        fields
  | Ppxlib.Parsetree.Pexp_field (e, _) ->
      find_nested_bindings state mod_name file results e
  | Ppxlib.Parsetree.Pexp_setfield (e1, _, e2) ->
      find_nested_bindings state mod_name file results e1;
      find_nested_bindings state mod_name file results e2
  | Ppxlib.Parsetree.Pexp_construct (_, Some e) ->
      find_nested_bindings state mod_name file results e
  | Ppxlib.Parsetree.Pexp_variant (_, Some e) ->
      find_nested_bindings state mod_name file results e
  | Ppxlib.Parsetree.Pexp_letmodule (_, _, e) ->
      find_nested_bindings state mod_name file results e
  | Ppxlib.Parsetree.Pexp_letexception (_, e) ->
      find_nested_bindings state mod_name file results e
  | Ppxlib.Parsetree.Pexp_assert e ->
      find_nested_bindings state mod_name file results e
  | Ppxlib.Parsetree.Pexp_lazy e ->
      find_nested_bindings state mod_name file results e
  | Ppxlib.Parsetree.Pexp_poly (e, _) ->
      find_nested_bindings state mod_name file results e
  | Ppxlib.Parsetree.Pexp_open (_, e) ->
      find_nested_bindings state mod_name file results e
  | Ppxlib.Parsetree.Pexp_send (e, _) ->
      find_nested_bindings state mod_name file results e
  (* Skip other expression types *)
  | _ -> ()

(* Collect length for all functions in files *)
let collect_length parse_ml module_name ?(usage_tracker = None) files =
  let ml_files = List.filter (fun f -> Filename.check_suffix f ".ml") files in
  let results = ref [] in
  List.iter
    (fun file ->
      match parse_ml file with
      | Error _ -> ()
      | Ok ast ->
          let mod_name = module_name file in
          (* Collect nolint suppressions for this file *)
          let suppressed_linters_by_line = Nolint.collect_suppressions ast in
          let state = { suppressed_linters_by_line; usage_tracker } in
          List.iter
            (fun item ->
              match item.Ppxlib.Parsetree.pstr_desc with
              | Ppxlib.Parsetree.Pstr_value (_, bindings) ->
                  (* Process top-level bindings *)
                  List.iter
                    (collect_binding state mod_name file results)
                    bindings;
                  (* Also find nested bindings inside function bodies *)
                  List.iter
                    (fun binding ->
                      find_nested_bindings state mod_name file results
                        binding.Ppxlib.Parsetree.pvb_expr)
                    bindings
              | _ -> ())
            ast)
    ml_files;
  List.rev !results

(* Find functions exceeding line count threshold *)
let find_long_functions threshold all_lengths =
  List.filter (fun issue -> issue.line_count > threshold) all_lengths

(* Report functions that exceed the line count threshold *)
let report_length_with_threshold threshold issues =
  List.iter
    (fun issue ->
      Printf.printf "Function '%s' has %d lines (threshold: %d) in %s:%d\n"
        issue.id.name issue.line_count threshold issue.source_file
        issue.id.loc.line)
    issues;
  flush stdout
