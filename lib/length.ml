(* Function length checker *)

open Types
open Stdlib
open Ppxlib.Parsetree
open Ppxlib.Location

(* Calculate line count of an expression from its location *)
let line_count_of_expr expr =
  let start_line = expr.pexp_loc.loc_start.pos_lnum in
  let end_line = expr.pexp_loc.loc_end.pos_lnum in
  end_line - start_line + 1

(* Collect length from a single binding (top-level or nested) *)
let collect_binding mod_name file results binding =
  match binding.pvb_pat.ppat_desc with
  | Ppat_var { txt = name; _ } ->
      let line_count = line_count_of_expr binding.pvb_expr in
      results :=
        {
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
          line_count;
          source_file = file;
        }
        :: !results
  | _ -> ()

(* Recursively find nested let bindings in an expression *)
let rec find_nested_bindings mod_name file results expr =
  (* Check if this is a let expression with nested functions *)
  match expr.pexp_desc with
  | Pexp_let (_, bindings, body) ->
      (* Collect the nested bindings *)
      List.iter (collect_binding mod_name file results) bindings;
      (* Also check the body for more nested bindings *)
      List.iter
        (fun binding ->
          find_nested_bindings mod_name file results binding.pvb_expr)
        bindings;
      (* Recursively check the body expression *)
      find_nested_bindings mod_name file results body
  | Pexp_function (_, _, body) -> (
      match body with
      | Pfunction_body e -> find_nested_bindings mod_name file results e
      | Pfunction_cases (cases, _, _) ->
          List.iter
            (fun c ->
              Option.iter
                (fun e -> find_nested_bindings mod_name file results e)
                c.pc_guard;
              find_nested_bindings mod_name file results c.pc_rhs)
            cases)
  | Pexp_match (e, cases) ->
      find_nested_bindings mod_name file results e;
      List.iter
        (fun c ->
          Option.iter
            (fun e -> find_nested_bindings mod_name file results e)
            c.pc_guard;
          find_nested_bindings mod_name file results c.pc_rhs)
        cases
  | Pexp_try (e, cases) ->
      find_nested_bindings mod_name file results e;
      List.iter
        (fun c ->
          Option.iter
            (fun e -> find_nested_bindings mod_name file results e)
            c.pc_guard;
          find_nested_bindings mod_name file results c.pc_rhs)
        cases
  | Pexp_ifthenelse (e1, e2, e3) ->
      find_nested_bindings mod_name file results e1;
      find_nested_bindings mod_name file results e2;
      Option.iter (find_nested_bindings mod_name file results) e3
  | Pexp_sequence (e1, e2) ->
      find_nested_bindings mod_name file results e1;
      find_nested_bindings mod_name file results e2
  | Pexp_while (e1, e2) ->
      find_nested_bindings mod_name file results e1;
      find_nested_bindings mod_name file results e2
  | Pexp_for (_, _, _, _, e) -> find_nested_bindings mod_name file results e
  | Pexp_apply (e, args) ->
      find_nested_bindings mod_name file results e;
      List.iter
        (fun (_, arg) -> find_nested_bindings mod_name file results arg)
        args
  | Pexp_tuple items ->
      List.iter (find_nested_bindings mod_name file results) items
  | Pexp_array items ->
      List.iter (find_nested_bindings mod_name file results) items
  | Pexp_record (fields, _) ->
      List.iter
        (fun (_, expr) -> find_nested_bindings mod_name file results expr)
        fields
  | Pexp_field (e, _) -> find_nested_bindings mod_name file results e
  | Pexp_setfield (e1, _, e2) ->
      find_nested_bindings mod_name file results e1;
      find_nested_bindings mod_name file results e2
  | Pexp_construct (_, Some e) -> find_nested_bindings mod_name file results e
  | Pexp_variant (_, Some e) -> find_nested_bindings mod_name file results e
  | Pexp_letmodule (_, _, e) -> find_nested_bindings mod_name file results e
  | Pexp_letexception (_, e) -> find_nested_bindings mod_name file results e
  | Pexp_assert e -> find_nested_bindings mod_name file results e
  | Pexp_lazy e -> find_nested_bindings mod_name file results e
  | Pexp_poly (e, _) -> find_nested_bindings mod_name file results e
  | Pexp_open (_, e) -> find_nested_bindings mod_name file results e
  | Pexp_send (e, _) -> find_nested_bindings mod_name file results e
  (* Skip other expression types *)
  | _ -> ()

(* Collect length for all functions in files *)
let collect_length parse_ml module_name files =
  let ml_files = List.filter (fun f -> Filename.check_suffix f ".ml") files in
  let results = ref [] in
  List.iter
    (fun file ->
      match parse_ml file with
      | Result.Error _ -> ()
      | Result.Ok ast ->
          let mod_name = module_name file in
          List.iter
            (fun item ->
              match item.pstr_desc with
              | Pstr_value (_, bindings) ->
                  (* Process top-level bindings *)
                  List.iter (collect_binding mod_name file results) bindings;
                  (* Also find nested bindings inside function bodies *)
                  List.iter
                    (fun binding ->
                      find_nested_bindings mod_name file results
                        binding.pvb_expr)
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
