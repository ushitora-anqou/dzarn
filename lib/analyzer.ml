open Types
open Stdlib
open Lexing
open Parsetree
open Longident
open Location

(* String set module *)
module StringSet = Set.Make (String)

(* Module for (module_name, function_name) pairs *)
module UsagePair = struct
  type t = string * string

  let compare (m1, f1) (m2, f2) =
    match String.compare m1 m2 with 0 -> String.compare f1 f2 | c -> c
end

module UsageSet = Set.Make (UsagePair)

(* File discovery *)
let find_ocaml_files dir =
  let files = ref [] in
  let rec traverse dir =
    try
      let entries = Sys.readdir dir |> Array.to_list in
      List.iter
        (fun entry ->
          let path = Filename.concat dir entry in
          if Sys.is_directory path then
            if
              entry <> "." && entry <> ".." && entry <> "_build"
              && (not (String.starts_with ~prefix:"." entry))
              && not (String.starts_with ~prefix:"systemd" entry)
            then traverse path
            else ()
          else if
            Filename.check_suffix entry ".ml"
            || Filename.check_suffix entry ".mli"
          then files := path :: !files
          else ())
        entries
    with Sys_error _ -> ()
  in
  traverse dir;
  List.rev !files

(* Location conversion from Location.t to our loc type *)
let loc_of_location location =
  {
    file = location.loc_start.pos_fname;
    line = location.loc_start.pos_lnum;
    column = location.loc_start.pos_cnum - location.loc_start.pos_bol;
  }

(* AST parsing *)
let parse_ml_file filename =
  let ic = open_in_bin filename in
  try
    let lexbuf = Lexing.from_channel ic in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let ast = Parse.implementation lexbuf in
    close_in ic;
    Ok ast
  with e ->
    close_in ic;
    Error e

let parse_mli_file filename =
  let ic = open_in_bin filename in
  try
    let lexbuf = Lexing.from_channel ic in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let ast = Parse.interface lexbuf in
    close_in ic;
    Ok ast
  with e ->
    close_in ic;
    Error e

(* Check if a name is private (starts with underscore) *)
let is_private_name name = String.length name > 0 && name.[0] = '_'

(* Extract module name from file path *)
let module_name_of_file file =
  let base = Filename.basename file in
  let name = Filename.chop_extension base in
  String.capitalize_ascii name

(* Collect public function signatures from .mli files *)
let collect_mli_signatures mli_files =
  (* Returns a map from module name to set of public function names *)
  let module StringMap = Map.Make (String) in
  let sigs = ref StringMap.empty in
  List.iter
    (fun file ->
      match parse_mli_file file with
      | Error _ -> ()
      | Ok ast ->
          let mod_name = module_name_of_file file in
          (* Collect value declarations *)
          let mod_sigs =
            List.fold_left
              (fun acc item ->
                match item.psig_desc with
                | Psig_value { pval_name = { txt = name; _ }; _ } ->
                    StringSet.add name acc
                | _ -> acc)
              StringSet.empty ast
          in
          sigs := StringMap.add mod_name mod_sigs !sigs)
    mli_files;
  !sigs

(* Function definition collector - simple traversal *)
let collect_function_definitions files =
  let functions = ref [] in
  let module StringMap = Map.Make (String) in
  let mli_files = List.filter (fun f -> Filename.check_suffix f ".mli") files in
  let ml_files = List.filter (fun f -> Filename.check_suffix f ".ml") files in
  let mli_sigs = collect_mli_signatures mli_files in

  let process_file file =
    match parse_ml_file file with
    | Error _ -> ()
    | Ok ast ->
        let mod_name = module_name_of_file file in
        let has_mli = StringMap.mem mod_name mli_sigs in
        let mod_sigs =
          if has_mli then Some (StringMap.find mod_name mli_sigs) else None
        in
        List.iter
          (fun (item : Parsetree.structure_item) ->
            match item.pstr_desc with
            | Parsetree.Pstr_value (_, bindings) ->
                List.iter
                  (fun binding ->
                    match binding.pvb_pat.ppat_desc with
                    | Parsetree.Ppat_var { txt = name; _ } ->
                        let is_public =
                          match mod_sigs with
                          | Some sigs -> StringSet.mem name sigs
                          | None -> not (is_private_name name)
                        in
                        if is_public then
                          functions :=
                            {
                              id =
                                {
                                  module_name = mod_name;
                                  name;
                                  loc = loc_of_location binding.pvb_pat.ppat_loc;
                                };
                              is_public = true;
                              source_file = file;
                            }
                            :: !functions
                    | _ -> ())
                  bindings
            | _ -> ())
          ast
  in
  List.iter process_file ml_files;
  List.rev !functions

(* Usage tracker - simple recursive traversal *)
let rec collect_expr usages mod_name file = function
  | { pexp_desc = Pexp_ident { txt = Lident name; _ }; _ } ->
      (* Direct call: f *)
      (* Skip common operators and builtins *)
      if
        name = "=" || name = "<>" || name = "+" || name = "-" || name = "*"
        || name = "/" || name = "^" || name = "@" || name = "&" || name = "or"
        || name = "&&" || name = "||" || name = "not" || name = "true"
        || name = "false" || name = "()" || name = "[]" || name = "[|]"
        || name = "::" || name = "::@"
      then ()
      else
        usages :=
          {
            id =
              {
                module_name = mod_name;
                name;
                loc = { file; line = 0; column = 0 };
              };
            is_public = true;
            source_file = file;
          }
          :: !usages
  | {
      pexp_desc =
        Pexp_ident
          { txt = Ldot ({ txt = _prefix; _ }, { txt = func_name; _ }); _ };
      _;
    } ->
      (* Qualified call: Module.func or A.B.func *)
      (* We track just the function name since cross-module calls are common *)
      usages :=
        {
          id =
            {
              module_name = mod_name;
              (* Keep current module for context *)
              name = func_name;
              loc = { file; line = 0; column = 0 };
            };
          is_public = true;
          source_file = file;
        }
        :: !usages
  | { pexp_desc = Pexp_tuple el; _ } ->
      List.iter (fun (_, e) -> collect_expr usages mod_name file e) el
  | { pexp_desc = Pexp_construct (_, Some e); _ } ->
      collect_expr usages mod_name file e
  | { pexp_desc = Pexp_construct (_, None); _ } -> ()
  | { pexp_desc = Pexp_variant (_, Some e); _ } ->
      collect_expr usages mod_name file e
  | { pexp_desc = Pexp_record (fields, _); _ } ->
      (* Process record field values *)
      (* fields is (string loc * expression) list *)
      List.iter
        (fun (_label, expr) -> collect_expr usages mod_name file expr)
        fields
  | { pexp_desc = Pexp_array el; _ } ->
      List.iter (collect_expr usages mod_name file) el
  | { pexp_desc = Pexp_sequence (e1, e2); _ } ->
      collect_expr usages mod_name file e1;
      collect_expr usages mod_name file e2
  | { pexp_desc = Pexp_while (e1, e2); _ } ->
      collect_expr usages mod_name file e1;
      collect_expr usages mod_name file e2
  | { pexp_desc = Pexp_for (_, _, _, _, e); _ } ->
      collect_expr usages mod_name file e
  | { pexp_desc = Pexp_let (_, ebs, e); _ } ->
      List.iter (fun eb -> collect_expr usages mod_name file eb.pvb_expr) ebs;
      collect_expr usages mod_name file e
  | { pexp_desc = Pexp_function (_, _, body); _ } -> (
      match body with
      | Pfunction_body e -> collect_expr usages mod_name file e
      | Pfunction_cases (cases, _, _) ->
          List.iter
            (fun c ->
              Option.iter (collect_expr usages mod_name file) c.pc_guard;
              collect_expr usages mod_name file c.pc_rhs)
            cases)
  | { pexp_desc = Pexp_apply (e, args); _ } ->
      collect_expr usages mod_name file e;
      List.iter (fun (_, arg) -> collect_expr usages mod_name file arg) args
  | { pexp_desc = Pexp_ifthenelse (e1, e2, e3); _ } ->
      collect_expr usages mod_name file e1;
      collect_expr usages mod_name file e2;
      Option.iter (collect_expr usages mod_name file) e3
  | { pexp_desc = Pexp_try (e, cases); _ } ->
      (* try...with and effect handlers: process the body and all handler cases *)
      collect_expr usages mod_name file e;
      List.iter
        (fun c ->
          Option.iter (collect_expr usages mod_name file) c.pc_guard;
          collect_expr usages mod_name file c.pc_rhs)
        cases
  | { pexp_desc = Pexp_field (e, _); _ } -> collect_expr usages mod_name file e
  | { pexp_desc = Pexp_setfield (e1, _, e2); _ } ->
      collect_expr usages mod_name file e1;
      collect_expr usages mod_name file e2
  | { pexp_desc = Pexp_send (e, _); _ } -> collect_expr usages mod_name file e
  | { pexp_desc = Pexp_new _; _ } -> ()
  | { pexp_desc = Pexp_letmodule (_, _, e); _ } ->
      collect_expr usages mod_name file e
  | { pexp_desc = Pexp_letexception (_, e); _ } ->
      collect_expr usages mod_name file e
  | { pexp_desc = Pexp_assert e; _ } -> collect_expr usages mod_name file e
  | { pexp_desc = Pexp_lazy e; _ } -> collect_expr usages mod_name file e
  | { pexp_desc = Pexp_poly (e, _); _ } -> collect_expr usages mod_name file e
  | { pexp_desc = Pexp_object _; _ } ->
      (* Skip class internals for now *)
      ()
  | { pexp_desc = Pexp_pack _; _ } ->
      (* Skip module pack for now *)
      ()
  | { pexp_desc = Pexp_letop _; _ } -> ()
  | { pexp_desc = Pexp_constant _; _ } -> ()
  | { pexp_desc = Pexp_open (_, e); _ } -> collect_expr usages mod_name file e
  | { pexp_desc = Pexp_extension (_, _); _ } -> ()
  | { pexp_desc = Pexp_unreachable; _ } -> ()
  | _ -> ()

let rec collect_structure usages mod_name file = function
  | [] -> ()
  | item :: rest ->
      (match item.pstr_desc with
      | Pstr_eval (e, _) -> collect_expr usages mod_name file e
      | Pstr_value (_, bindings) ->
          List.iter
            (fun binding -> collect_expr usages mod_name file binding.pvb_expr)
            bindings
      | Pstr_primitive _ -> ()
      | Pstr_type _ -> ()
      | Pstr_typext _ -> ()
      | Pstr_exception _ -> ()
      | Pstr_module _ -> ()
      | Pstr_recmodule _ -> ()
      | Pstr_modtype _ -> ()
      | Pstr_open _ -> ()
      | Pstr_class _ -> ()
      | Pstr_class_type _ -> ()
      | Pstr_include _ -> ()
      | Pstr_attribute _ -> ()
      | Pstr_extension _ -> ());
      collect_structure usages mod_name file rest

let collect_usages files =
  let usages = ref [] in
  let ml_files = List.filter (fun f -> Filename.check_suffix f ".ml") files in

  List.iter
    (fun file ->
      match parse_ml_file file with
      | Error _ -> ()
      | Ok ast ->
          let mod_name = module_name_of_file file in
          collect_structure usages mod_name file ast)
    ml_files;
  !usages

(* Find unused functions *)
let find_unused (functions : Types.func_def list) (usages : Types.func_def list)
    : Types.func_def list =
  (* Collect function names that are used in expressions *)
  let used_names =
    List.fold_left
      (fun (acc : StringSet.t) (usage : Types.func_def) ->
        StringSet.add usage.Types.id.name acc)
      StringSet.empty usages
  in

  (* Helper function to check if string contains substring *)
  let contains_substring s sub =
    let sub_len = String.length sub in
    let s_len = String.length s in
    let rec check i =
      if i + sub_len > s_len then false
      else if String.sub s i sub_len = sub then true
      else check (i + 1)
    in
    check 0
  in

  (* Also check if function name appears in its source file (excluding definition line) *)
  (* This catches functions used in patterns/attributes that aren't tracked in exprs *)
  let is_used_in_file (def : Types.func_def) =
    (* First check if used in expressions *)
    if StringSet.mem def.id.name used_names then true
    else
      (* Check if name appears elsewhere in the file *)
      try
        let lines =
          let ic = open_in def.source_file in
          let rec read_lines acc =
            try
              let line = input_line ic in
              read_lines (line :: acc)
            with End_of_file -> acc
          in
          let result = read_lines [] in
          close_in ic;
          List.rev result
        in
        (* Check if function name appears in any line except its definition line *)
        let appears_elsewhere =
          List.mapi (fun i line -> (i + 1, line)) lines
          |> List.exists (fun (line_num, line) ->
              line_num <> def.id.loc.line && contains_substring line def.id.name)
        in
        appears_elsewhere
      with _ -> false
  in

  List.filter (fun def -> not (is_used_in_file def)) functions

(* Report unused functions *)
let report_unused (unused : Types.func_def list) : unit =
  List.iter
    (fun (def : Types.func_def) ->
      Printf.printf "Unused function '%s' in %s:%d\n" def.Types.id.name
        def.Types.source_file def.Types.id.loc.line)
    unused;
  flush stdout

(* Print AST back to source *)
let print_structure out_channel ast =
  let pp = Format.formatter_of_out_channel out_channel in
  Pprintast.structure pp ast

(* Remove unused functions - simple recursive filtering *)
let remove_unused_functions (unused : Types.func_def list)
    (ast : Parsetree.structure) : Parsetree.structure =
  let unused_set =
    List.fold_left
      (fun acc (def : Types.func_def) -> StringSet.add def.Types.id.name acc)
      StringSet.empty unused
  in

  let rec filter_structure = function
    | [] -> []
    | item :: rest ->
        let should_keep, new_desc =
          match item.pstr_desc with
          | Pstr_value (rf, bindings) ->
              let filtered =
                List.filter
                  (fun binding ->
                    match binding.pvb_pat.ppat_desc with
                    | Ppat_var { txt = name; _ } ->
                        not (StringSet.mem name unused_set)
                    | _ -> true)
                  bindings
              in
              if filtered = [] then (false, Pstr_value (rf, []))
              else (true, Pstr_value (rf, filtered))
          | other -> (true, other)
        in
        if should_keep then
          { item with pstr_desc = new_desc } :: filter_structure rest
        else filter_structure rest
  in

  filter_structure ast

(* Apply fix to a file *)
let apply_fix (file : string) (unused : Types.func_def list) : unit =
  match parse_ml_file file with
  | Error _ -> ()
  | Ok ast ->
      let new_ast = remove_unused_functions unused ast in
      (* Write back to file *)
      let oc = open_out file in
      let pp = Format.formatter_of_out_channel oc in
      Pprintast.structure pp new_ast;
      Format.pp_print_flush pp ();
      close_out oc

(* Main run function with config *)
let run ~fix ~config dir =
  let files = find_ocaml_files dir in
  if files = [] then (
    Printf.printf "No OCaml files found in %s\n" dir;
    flush stdout;
    0)
  else
    let exit_code = ref 0 in

    (* Run unused function checker if enabled *)
    if config.Config.unused_enabled then (
      let functions = collect_function_definitions files in
      let usages = collect_usages files in
      let unused = find_unused functions usages in

      if unused = [] then Printf.printf "No unused public functions found.\n"
      else (
        report_unused unused;
        exit_code := 1);

      flush stdout;

      if fix then (
        (* Group unused by source file *)
        let by_file =
          List.fold_left
            (fun (acc : (string * Types.func_def list) list)
                 (def : Types.func_def) ->
              let key = def.Types.source_file in
              let defs = try List.assoc key acc with Not_found -> [] in
              (key, def :: defs) :: List.remove_assoc key acc)
            ([] : (string * Types.func_def list) list)
            unused
        in

        List.iter (fun (file, defs) -> apply_fix file defs) by_file;
        Printf.printf "Fixed %d file(s).\n" (List.length by_file);
        flush stdout));

    (* Run complexity checker if enabled *)
    if config.Config.complexity_enabled then (
      let complexities =
        Complexity.collect_complexity parse_ml_file module_name_of_file files
      in
      let complex_funcs =
        Complexity.find_complex_functions config.Config.complexity_threshold
          complexities
      in
      if complex_funcs = [] then
        Printf.printf "No functions exceed complexity threshold.\n"
      else (
        Complexity.report_complex_with_threshold
          config.Config.complexity_threshold complex_funcs;
        exit_code := 1);
      flush stdout)
    else
      (* If complexity is not enabled, print a message *)
      Printf.printf "Complexity checking disabled.\n";

    !exit_code
