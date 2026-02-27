module Parse = Ppxlib.Parse

(* String set module - use derived ord for comparison *)
module String_set_key = struct
  type t = string [@@deriving ord]
end

module StringSet = Set.Make (String_set_key)

(* String map module *)
module StringMap = Map.Make (String_set_key)

(* Module for (module_name, function_name) pairs - use derived ord *)
module UsagePair = struct
  type t = string * string [@@deriving ord]
end

module UsageSet = Set.Make (UsagePair)

(* Convert Longident.t to string representation *)
(* Longident.t = Lident of string | Ldot of t * string | Lapply of t * t *)
let rec longident_to_string : Ppxlib.Longident.t -> string = function
  | Ppxlib.Longident.Lident s -> s
  | Ppxlib.Longident.Ldot (prefix, s) -> longident_to_string prefix ^ "." ^ s
  | Ppxlib.Longident.Lapply _ -> ""

(* Get the last identifier from a Longident.t *)
(* For Dzarn.Config, returns "Config" *)
let last_ident_of_longident : Ppxlib.Longident.t -> string = function
  | Ppxlib.Longident.Lident s -> s
  | Ppxlib.Longident.Ldot (_, s) -> s
  | Ppxlib.Longident.Lapply _ -> ""

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

(* Find OCaml files in multiple directories with deduplication *)
let find_ocaml_files_in_dirs dirs =
  let files = ref [] in
  let seen = ref StringSet.empty in
  let rec traverse dir =
    try
      let entries = Sys.readdir dir in
      Array.iter
        (fun entry ->
          let path = Filename.concat dir entry in
          (* Skip _build and _dune directories *)
          if entry <> "_build" && entry <> "_dune" then
            try
              if Sys.is_directory path then
                if
                  entry <> "."
                  && (not (String.starts_with ~prefix:"." entry))
                  && not (String.starts_with ~prefix:"systemd" entry)
                then traverse path
                else ()
              else if
                Filename.check_suffix entry ".ml"
                || Filename.check_suffix entry ".mli"
              then
                (* Deduplicate files *)
                if not (StringSet.mem path !seen) then (
                  files := path :: !files;
                  seen := StringSet.add path !seen)
            with Sys_error _ -> ())
        entries
    with Sys_error _ -> ()
  in
  List.iter traverse dirs;
  List.rev !files

(* Location conversion from Location.t to our loc type *)
let loc_of_location (location : Ppxlib.Location.t) : Types.loc =
  Types.make_loc ~file:location.Ppxlib.Location.loc_start.pos_fname
    ~line:location.Ppxlib.Location.loc_start.pos_lnum
    ~column:
      (location.Ppxlib.Location.loc_start.pos_cnum
     - location.Ppxlib.Location.loc_start.pos_bol)

(* AST parsing using ppxlib *)
let parse_ml_file filename : (Ppxlib.Parsetree.structure, exn) result =
  try
    let code =
      let ic = open_in_bin filename in
      let len = in_channel_length ic in
      let s = Bytes.create len in
      really_input ic s 0 len;
      close_in ic;
      Bytes.to_string s
    in
    let lexbuf = Lexing.from_string code in
    let ast = Parse.implementation lexbuf in
    Ok ast
  with e -> Error e

let parse_mli_file filename : (Ppxlib.Parsetree.signature, exn) result =
  try
    let code =
      let ic = open_in_bin filename in
      let len = in_channel_length ic in
      let s = Bytes.create len in
      really_input ic s 0 len;
      close_in ic;
      Bytes.to_string s
    in
    let lexbuf = Lexing.from_string code in
    let ast = Parse.interface lexbuf in
    Ok ast
  with e -> Error e

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
                match item.Ppxlib.Parsetree.psig_desc with
                | Ppxlib.Parsetree.Psig_value
                    { pval_name = { txt = name; _ }; _ } ->
                    StringSet.add name acc
                | _ -> acc)
              StringSet.empty ast
          in
          sigs := StringMap.add mod_name mod_sigs !sigs)
    mli_files;
  !sigs

(* Collect public function signatures from .mli files with location information *)
let collect_mli_signatures_with_loc mli_files : Types.mli_signature list =
  let sigs = ref [] in
  List.iter
    (fun file ->
      match parse_mli_file file with
      | Error _ -> ()
      | Ok ast ->
          let mod_name = module_name_of_file file in
          List.iter
            (fun item ->
              match item.Ppxlib.Parsetree.psig_desc with
              | Ppxlib.Parsetree.Psig_value
                  { pval_name = { txt = name; _ }; pval_loc = val_loc; _ } ->
                  sigs :=
                    Types.make_mli_signature ~module_name:mod_name ~name
                      ~loc:(loc_of_location val_loc) ~mli_file:file
                    :: !sigs
              | _ -> ())
            ast)
    mli_files;
  !sigs

(* Function definition collector - simple traversal *)
let collect_function_definitions files =
  let functions = ref [] in
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
          (fun (item : Ppxlib.Parsetree.structure_item) ->
            match item.Ppxlib.Parsetree.pstr_desc with
            | Ppxlib.Parsetree.Pstr_value (_, bindings) ->
                List.iter
                  (fun binding ->
                    match
                      binding.Ppxlib.Parsetree.pvb_pat
                        .Ppxlib.Parsetree.ppat_desc
                    with
                    | Ppxlib.Parsetree.Ppat_var { txt = name; _ } ->
                        let is_public =
                          match mod_sigs with
                          | Some sigs -> StringSet.mem name sigs
                          | None -> not (is_private_name name)
                        in
                        if is_public then
                          functions :=
                            Types.make_func_def
                              ~id:
                                (Types.make_func_id ~module_name:mod_name ~name
                                   ~loc:
                                     (loc_of_location
                                        binding.Ppxlib.Parsetree.pvb_pat
                                          .Ppxlib.Parsetree.ppat_loc))
                              ~is_public:true ~source_file:file
                            :: !functions
                    | _ -> ())
                  bindings
            | _ -> ())
          ast
  in
  List.iter process_file ml_files;
  List.rev !functions

(* Helper to process identifier and record usage *)
let process_ident usages mod_name file longident =
  match longident with
  | { Ppxlib.Location.txt = Ppxlib.Longident.Lident name; _ } ->
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
          Types.make_func_def
            ~id:
              (Types.make_func_id ~module_name:mod_name ~name
                 ~loc:(Types.make_loc ~file ~line:0 ~column:0))
            ~is_public:true ~source_file:file
          :: !usages
  | { Ppxlib.Location.txt = Ppxlib.Longident.Ldot (prefix, func_name); _ } ->
      (* Qualified call: Module.func or A.B.func *)
      (* Use the last component of the module path to match module_name_of_file *)
      let module_name = last_ident_of_longident prefix in
      (* Track the usage with the actual module being called *)
      usages :=
        Types.make_func_def
          ~id:
            (Types.make_func_id ~module_name ~name:func_name
               ~loc:(Types.make_loc ~file ~line:0 ~column:0))
          ~is_public:true ~source_file:file
        :: !usages
  | { Ppxlib.Location.txt = Ppxlib.Longident.Lapply _; _ } -> ()

(* Usage tracker - simple recursive traversal *)
let rec collect_expr usages mod_name file (expr : Ppxlib.Parsetree.expression) :
    unit =
  (* Printf.eprintf "DEBUG: collect_expr in %s (mod=%s)\n%!" file mod_name; *)
  match expr.Ppxlib.Parsetree.pexp_desc with
  | Ppxlib.Parsetree.Pexp_ident longident ->
      process_ident usages mod_name file longident
  | Ppxlib.Parsetree.Pexp_tuple el ->
      List.iter (fun e -> collect_expr usages mod_name file e) el
  | Ppxlib.Parsetree.Pexp_construct (_, Some e) ->
      collect_expr usages mod_name file e
  | Ppxlib.Parsetree.Pexp_construct (_, None) -> ()
  | Ppxlib.Parsetree.Pexp_variant (_, Some e) ->
      collect_expr usages mod_name file e
  | Ppxlib.Parsetree.Pexp_record (fields, _) ->
      (* Process record field values *)
      List.iter (fun (_, expr) -> collect_expr usages mod_name file expr) fields
  | Ppxlib.Parsetree.Pexp_array el ->
      List.iter (collect_expr usages mod_name file) el
  | Ppxlib.Parsetree.Pexp_sequence (e1, e2) ->
      collect_expr usages mod_name file e1;
      collect_expr usages mod_name file e2
  | Ppxlib.Parsetree.Pexp_while (e1, e2) ->
      collect_expr usages mod_name file e1;
      collect_expr usages mod_name file e2
  | Ppxlib.Parsetree.Pexp_for (_, _, _, _, e) ->
      collect_expr usages mod_name file e
  | Ppxlib.Parsetree.Pexp_let (_, ebs, e) ->
      List.iter
        (fun eb ->
          collect_expr usages mod_name file eb.Ppxlib.Parsetree.pvb_expr)
        ebs;
      collect_expr usages mod_name file e
  | Ppxlib.Parsetree.Pexp_function (_, _, body) -> (
      match body with
      | Ppxlib.Parsetree.Pfunction_body e -> collect_expr usages mod_name file e
      | Ppxlib.Parsetree.Pfunction_cases (cases, _, _) ->
          List.iter
            (fun c ->
              Option.iter
                (collect_expr usages mod_name file)
                c.Ppxlib.Parsetree.pc_guard;
              collect_expr usages mod_name file c.Ppxlib.Parsetree.pc_rhs)
            cases)
  | Ppxlib.Parsetree.Pexp_match (e, cases) ->
      collect_expr usages mod_name file e;
      List.iter
        (fun c ->
          Option.iter
            (collect_expr usages mod_name file)
            c.Ppxlib.Parsetree.pc_guard;
          collect_expr usages mod_name file c.Ppxlib.Parsetree.pc_rhs)
        cases
  | Ppxlib.Parsetree.Pexp_apply (e, args) ->
      collect_expr usages mod_name file e;
      List.iter (fun (_, arg) -> collect_expr usages mod_name file arg) args
  | Ppxlib.Parsetree.Pexp_ifthenelse (e1, e2, e3) ->
      collect_expr usages mod_name file e1;
      collect_expr usages mod_name file e2;
      Option.iter (collect_expr usages mod_name file) e3
  | Ppxlib.Parsetree.Pexp_try (e, cases) ->
      (* try...with and effect handlers: process the body and all handler cases *)
      collect_expr usages mod_name file e;
      List.iter
        (fun c ->
          Option.iter
            (collect_expr usages mod_name file)
            c.Ppxlib.Parsetree.pc_guard;
          collect_expr usages mod_name file c.Ppxlib.Parsetree.pc_rhs)
        cases
  | Ppxlib.Parsetree.Pexp_field (e, _) -> collect_expr usages mod_name file e
  | Ppxlib.Parsetree.Pexp_setfield (e1, _, e2) ->
      collect_expr usages mod_name file e1;
      collect_expr usages mod_name file e2
  | Ppxlib.Parsetree.Pexp_send (e, _) -> collect_expr usages mod_name file e
  | Ppxlib.Parsetree.Pexp_new _ -> ()
  | Ppxlib.Parsetree.Pexp_letmodule (_, _, e) ->
      collect_expr usages mod_name file e
  | Ppxlib.Parsetree.Pexp_letexception (_, e) ->
      collect_expr usages mod_name file e
  | Ppxlib.Parsetree.Pexp_assert e -> collect_expr usages mod_name file e
  | Ppxlib.Parsetree.Pexp_lazy e -> collect_expr usages mod_name file e
  | Ppxlib.Parsetree.Pexp_poly (e, _) -> collect_expr usages mod_name file e
  | Ppxlib.Parsetree.Pexp_object _ ->
      (* Skip class internals for now *)
      ()
  | Ppxlib.Parsetree.Pexp_pack _ ->
      (* Skip module pack for now *)
      ()
  | Ppxlib.Parsetree.Pexp_letop _ -> ()
  | Ppxlib.Parsetree.Pexp_constant _ -> ()
  | Ppxlib.Parsetree.Pexp_open (_, e) -> collect_expr usages mod_name file e
  | Ppxlib.Parsetree.Pexp_extension (_, _) -> ()
  | Ppxlib.Parsetree.Pexp_unreachable -> ()
  | _ -> ()

let rec collect_structure usages mod_name file = function
  | [] -> ()
  | item :: rest ->
      (match item.Ppxlib.Parsetree.pstr_desc with
      | Ppxlib.Parsetree.Pstr_eval (e, _) -> collect_expr usages mod_name file e
      | Ppxlib.Parsetree.Pstr_value (_, bindings) ->
          List.iter
            (fun binding ->
              collect_expr usages mod_name file
                binding.Ppxlib.Parsetree.pvb_expr)
            bindings
      | Ppxlib.Parsetree.Pstr_primitive _ -> ()
      | Ppxlib.Parsetree.Pstr_type _ -> ()
      | Ppxlib.Parsetree.Pstr_typext _ -> ()
      | Ppxlib.Parsetree.Pstr_exception _ -> ()
      | Ppxlib.Parsetree.Pstr_module _ -> ()
      | Ppxlib.Parsetree.Pstr_recmodule _ -> ()
      | Ppxlib.Parsetree.Pstr_modtype _ -> ()
      | Ppxlib.Parsetree.Pstr_open _ -> ()
      | Ppxlib.Parsetree.Pstr_class _ -> ()
      | Ppxlib.Parsetree.Pstr_class_type _ -> ()
      | Ppxlib.Parsetree.Pstr_include _ -> ()
      | Ppxlib.Parsetree.Pstr_attribute _ -> ()
      | Ppxlib.Parsetree.Pstr_extension _ -> ());
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
  (* Collect (module_name, function_name) pairs that are used in expressions *)
  let used_pairs =
    List.fold_left
      (fun (acc : UsageSet.t) (usage : Types.func_def) ->
        UsageSet.add (usage.Types.id.module_name, usage.Types.id.name) acc)
      UsageSet.empty usages
  in

  (* Check if function name appears in its source file (excluding definition line) *)
  (* This catches functions used in patterns/attributes that aren't tracked in exprs *)
  let is_used_in_file (def : Types.func_def) =
    (* First check if used in expressions with matching module and name *)
    if UsageSet.mem (def.id.module_name, def.id.name) used_pairs then true
    else
      (* Fallback: check if name appears elsewhere in the file *)
      (* This catches cases not tracked by AST traversal (e.g., some patterns) *)
      (* We need to exclude qualified calls like Module.func *)
      let ic = open_in def.source_file in
      let rec read_lines acc =
        try
          let line = input_line ic in
          read_lines (line :: acc)
        with End_of_file -> acc
      in
      let lines =
        try
          let result = read_lines [] in
          close_in ic;
          List.rev result
        with e ->
          close_in ic;
          raise e
      in
      (* Check if function name appears unqualified in the file *)
      (* An unqualified use is: "name", "(name", "; name", etc. *)
      (* A qualified use like "Module.name" should NOT count *)
      (* Also exclude 'let name = ...' lines which are definitions, not uses *)
      let name = def.id.name in
      let name_len = String.length name in
      let is_this_function_definition line =
        (* Check if this line defines 'name' (i.e., "let name = ...") *)
        let rec skip_spaces i =
          if i >= String.length line then false
          else
            match String.get line i with
            | ' ' | '\t' -> skip_spaces (i + 1)
            | _ -> (
                (* Check if the line starts with "let " *)
                let line_len = String.length line in
                line_len >= i + 4
                && String.sub line i 4 = "let "
                &&
                (* Check if "let " is followed by the function name *)
                let name_start = i + 4 in
                line_len >= name_start + name_len
                && String.sub line name_start name_len = name
                &&
                (* Check that the name is followed by a space or special char *)
                let name_end = name_start + name_len in
                name_end >= line_len
                ||
                match String.get line name_end with
                | ' ' | '\t' | '=' -> true
                | _ -> false)
        in
        skip_spaces 0
      in
      List.mapi (fun i line -> (i + 1, line)) lines
      |> List.exists (fun (line_num, line) ->
          if line_num = def.id.loc.line || is_this_function_definition line then
            false
          else
            (* Check each position in the line *)
            let line_len = String.length line in
            let rec check_pos pos =
              if pos + name_len > line_len then false
              else if String.sub line pos name_len = name then
                (* Check the character before the match *)
                let is_unqualified =
                  if pos = 0 then true
                    (* At start of line, check that it's not followed by a dot *)
                  else
                    match String.get line (pos - 1) with
                    | ' ' | '(' | ')' | ';' | '=' | ':' | '{' | '}' | '[' | ']'
                    | ',' | '\t' | '\n' ->
                        true
                    | '.' ->
                        false (* This is a qualified call like Module.name *)
                    | _ -> false (* Preceded by an identifier character *)
                in
                if is_unqualified then true else check_pos (pos + 1)
              else check_pos (pos + 1)
            in
            check_pos 0)
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
  Ppxlib.Pprintast.structure pp ast

(* Get location range from Ppxlib.Location.t *)
let get_loc_range (loc : Ppxlib.Location.t) : Types.loc_range =
  let start = loc.Ppxlib.Location.loc_start in
  let end_ = loc.Ppxlib.Location.loc_end in
  Types.make_loc_range ~file:start.pos_fname ~start_line:start.pos_lnum
    ~end_line:end_.pos_lnum
    ~start_char:(start.pos_cnum - start.pos_bol)
    ~end_char:(end_.pos_cnum - end_.pos_bol)

(* Remove unused functions and collect location ranges for text-based deletion *)
let remove_unused_functions_with_loc (unused : Types.func_def list)
    (ast : Ppxlib.Parsetree.structure) :
    Ppxlib.Parsetree.structure * Types.loc_range list =
  let unused_set =
    List.fold_left
      (fun acc (def : Types.func_def) -> StringSet.add def.Types.id.name acc)
      StringSet.empty unused
  in

  let removed_locs = ref [] in

  let rec filter_structure = function
    | [] -> []
    | item :: rest ->
        let should_keep, new_desc =
          match item.Ppxlib.Parsetree.pstr_desc with
          | Ppxlib.Parsetree.Pstr_value (rf, bindings) ->
              let filtered, removed =
                List.fold_left
                  (fun (acc_bindings, acc_locs) binding ->
                    match
                      binding.Ppxlib.Parsetree.pvb_pat
                        .Ppxlib.Parsetree.ppat_desc
                    with
                    | Ppxlib.Parsetree.Ppat_var { txt = name; _ } ->
                        if StringSet.mem name unused_set then
                          ( acc_bindings,
                            get_loc_range binding.Ppxlib.Parsetree.pvb_loc
                            :: acc_locs )
                        else (binding :: acc_bindings, acc_locs)
                    | _ -> (binding :: acc_bindings, acc_locs))
                  ([], []) bindings
              in
              removed_locs := !removed_locs @ List.rev removed;
              if filtered = [] then (false, Ppxlib.Parsetree.Pstr_value (rf, []))
              else (true, Ppxlib.Parsetree.Pstr_value (rf, List.rev filtered))
          | other -> (true, other)
        in
        if should_keep then
          { item with pstr_desc = new_desc } :: filter_structure rest
        else filter_structure rest
  in

  let result = filter_structure ast in
  (result, List.rev !removed_locs)

(* Remove unused functions - simple recursive filtering *)
let remove_unused_functions (unused : Types.func_def list)
    (ast : Ppxlib.Parsetree.structure) : Ppxlib.Parsetree.structure =
  fst (remove_unused_functions_with_loc unused ast)

(* Apply fix while preserving comments using text-based deletion *)
let apply_fix_preserve_comments (file : string) (unused : Types.func_def list) :
    unit =
  match parse_ml_file file with
  | Error _ -> ()
  | Ok ast ->
      (* Get the location ranges of items to remove *)
      let _, removed_locs = remove_unused_functions_with_loc unused ast in
      if removed_locs = [] then ()
      else
        (* Read the original source code *)
        let ic = open_in file in
        let rec read_lines acc =
          try
            let line = input_line ic in
            read_lines (line :: acc)
          with End_of_file -> List.rev acc
        in
        let lines = read_lines [] in
        close_in ic;

        (* Sort removal ranges by end line in descending order (remove from back) *)
        let sorted_removals =
          List.sort
            (fun a b ->
              compare b.Types.end_line a.Types.end_line
              |>
              (* secondary sort by start line descending *)
              fun cmp ->
              if cmp <> 0 then cmp
              else compare b.Types.start_line a.Types.start_line)
            removed_locs
        in

        (* Track which lines to remove *)
        let lines_to_remove = ref StringSet.empty in
        List.iter
          (fun loc ->
            for i = loc.Types.start_line to loc.Types.end_line do
              lines_to_remove :=
                StringSet.add (string_of_int i) !lines_to_remove
            done)
          sorted_removals;

        (* Filter out removed lines *)
        let new_lines =
          List.mapi
            (fun i line ->
              if StringSet.mem (string_of_int (i + 1)) !lines_to_remove then []
              else [ line ])
            lines
          |> List.concat
        in

        (* Write back to file *)
        let oc = open_out file in
        List.iter (fun line -> output_string oc (line ^ "\n")) new_lines;
        close_out oc

(* Apply fix to a file *)
let apply_fix (file : string) (unused_funcs : Types.func_def list) : unit =
  apply_fix_preserve_comments file unused_funcs

(* Apply fix to .mli file - removes unused signatures *)
let apply_fix_to_mli (mli_file : string) (unused_names : StringSet.t) : unit =
  match parse_mli_file mli_file with
  | Error _ -> ()
  | Ok ast ->
      (* Collect location ranges of signatures to remove *)
      let removed_locs = ref [] in
      List.iter
        (fun item ->
          match item.Ppxlib.Parsetree.psig_desc with
          | Ppxlib.Parsetree.Psig_value
              { pval_name = { txt = name; _ }; pval_loc = loc; _ } ->
              if StringSet.mem name unused_names then
                removed_locs := get_loc_range loc :: !removed_locs
          | _ -> ())
        ast;

      if !removed_locs <> [] then (
        (* Read the original source code *)
        let ic = open_in mli_file in
        let rec read_lines acc =
          try
            let line = input_line ic in
            read_lines (line :: acc)
          with End_of_file -> List.rev acc
        in
        let lines = read_lines [] in
        close_in ic;

        (* Sort removal ranges by end line in descending order *)
        let sorted_removals =
          List.sort
            (fun a b ->
              compare b.Types.end_line a.Types.end_line |> fun cmp ->
              if cmp <> 0 then cmp
              else compare b.Types.start_line a.Types.start_line)
            !removed_locs
        in

        (* Track which lines to remove *)
        let lines_to_remove = ref StringSet.empty in
        List.iter
          (fun loc ->
            for i = loc.Types.start_line to loc.Types.end_line do
              lines_to_remove :=
                StringSet.add (string_of_int i) !lines_to_remove
            done)
          sorted_removals;

        (* Filter out removed lines *)
        let new_lines =
          List.mapi
            (fun i line ->
              if StringSet.mem (string_of_int (i + 1)) !lines_to_remove then []
              else [ line ])
            lines
          |> List.concat
        in

        (* Write back to file *)
        let oc = open_out mli_file in
        List.iter (fun line -> output_string oc (line ^ "\n")) new_lines;
        close_out oc)

(* Main run function with config *)
let run ~fix ~config ?(json_output = config.Config.json_output) dirs =
  let files = find_ocaml_files_in_dirs dirs in
  if files = [] then (
    Printf.printf "No OCaml files found in: %s\n" (String.concat ", " dirs);
    flush stdout;
    0)
  else
    let exit_code = ref 0 in
    let all_issues = ref [] in

    (* Create usage tracker if unused_nolint checking is enabled *)
    let usage_tracker =
      if config.Config.unused_nolint_enabled then
        Some (Nolint.create_usage_tracker ())
      else None
    in

    (* Collect nolints and populate tracker *)
    (match usage_tracker with
    | None -> ()
    | Some tracker ->
        List.iter
          (fun file ->
            match parse_ml_file file with
            | Ok ast ->
                ignore (Nolint.collect_suppressions_with_tracker ast tracker)
            | _ -> ())
          files);

    (* Run unused function checker if enabled *)
    if config.Config.unused_enabled then (
      let functions = collect_function_definitions files in
      let usages = collect_usages files in
      let unused = find_unused functions usages in

      if unused = [] then (
        if not json_output then
          Printf.printf "No unused public functions found.\n")
      else (
        if not json_output then report_unused unused;
        List.iter
          (fun u -> all_issues := Json_reporter.unused_to_json u :: !all_issues)
          unused;
        exit_code := 1);

      flush stdout;

      if fix then (
        (* Group unused by source file *)
        let by_file : (string * Types.func_def list) list =
          List.fold_left
            (fun acc (def : Types.func_def) ->
              let key = def.Types.source_file in
              let defs = try List.assoc key acc with Not_found -> [] in
              (key, def :: defs) :: List.remove_assoc key acc)
            [] unused
        in

        List.iter
          (fun (file, defs) ->
            (* Apply fix to .ml file *)
            apply_fix file defs;

            (* Apply fix to corresponding .mli file if it exists *)
            let mli_file =
              let base = Filename.chop_extension (Filename.basename file) in
              let dir = Filename.dirname file in
              Filename.concat dir (base ^ ".mli")
            in
            if Sys.file_exists mli_file then
              let unused_names =
                List.fold_left
                  (fun acc (def : Types.func_def) ->
                    StringSet.add def.Types.id.name acc)
                  StringSet.empty defs
              in
              apply_fix_to_mli mli_file unused_names)
          by_file;
        if not json_output then
          Printf.printf "Fixed %d file(s).\n" (List.length by_file);
        flush stdout));

    (* Run naming checker if enabled *)
    if config.Config.naming_enabled then (
      let violations =
        Naming.collect_naming_violations parse_ml_file ~usage_tracker files
      in
      if violations = [] then (
        if not json_output then
          Printf.printf "No naming convention violations found.\n")
      else (
        if not json_output then Naming.report_naming_violations violations;
        List.iter
          (fun v -> all_issues := Json_reporter.naming_to_json v :: !all_issues)
          violations;
        exit_code := 1);
      flush stdout)
    else if
      (* If naming is not enabled, print a message *)
      not json_output
    then Printf.printf "Naming checking disabled.\n";

    (* Run complexity checker if enabled *)
    if config.Config.complexity_enabled then (
      let complexities : Types.complexity_issue list =
        Complexity.collect_complexity parse_ml_file module_name_of_file
          ~usage_tracker files
      in
      let complex_funcs : Types.complexity_issue list =
        Complexity.find_complex_functions config.Config.complexity_threshold
          complexities
      in
      if complex_funcs = [] then (
        if not json_output then
          Printf.printf "No functions exceed complexity threshold.\n")
      else (
        if not json_output then
          Complexity.report_complex_with_threshold
            config.Config.complexity_threshold complex_funcs;
        List.iter
          (fun f ->
            all_issues :=
              Json_reporter.complexity_to_json
                config.Config.complexity_threshold f
              :: !all_issues)
          complex_funcs;
        exit_code := 1);
      flush stdout)
    else if
      (* If complexity is not enabled, print a message *)
      not json_output
    then Printf.printf "Complexity checking disabled.\n";

    (* Run length checker if enabled *)
    if config.Config.length_enabled then (
      let lengths =
        Length.collect_length parse_ml_file module_name_of_file ~usage_tracker
          files
      in
      let long_funcs =
        Length.find_long_functions config.Config.length_threshold lengths
      in
      if long_funcs = [] then (
        if not json_output then
          Printf.printf "No functions exceed line count threshold.\n")
      else (
        if not json_output then
          Length.report_length_with_threshold config.Config.length_threshold
            long_funcs;
        List.iter
          (fun f ->
            all_issues :=
              Json_reporter.length_to_json config.Config.length_threshold f
              :: !all_issues)
          long_funcs;
        exit_code := 1);
      flush stdout)
    else if
      (* If length is not enabled, print a message *)
      not json_output
    then Printf.printf "Length checking disabled.\n";

    (* Check for unused nolints *)
    (if config.Config.unused_nolint_enabled then
       match usage_tracker with
       | None -> ()
       | Some tracker ->
           let unused = Nolint.find_unused_nolints tracker in
           if unused <> [] then (
             (* Report unused nolints *)
             if not json_output then
               List.iter
                 (fun (line, column, linter_names, _attr_type) ->
                   (* We need to figure out which file this nolint is in *)
                   (* For now, we'll report a generic message *)
                   Printf.printf
                     "Unused nolint directive at line %d:%d\n\
                     \  The [@@@nolint] attribute does not suppress any \
                      violations for linters: %s\n"
                     line column
                     (String.concat ", " linter_names))
                 unused;
             (* Note: unused nolints are not included in JSON output
                as they are metadata about the linting process itself,
                not code issues. *)
             exit_code := 1)
           else if not json_output then
             Printf.printf "No unused nolint directives found.\n");
    flush stdout;

    (* Output JSON if requested *)
    (if json_output then
       let json_output_data =
         {
           Types.issues = List.rev !all_issues;
           Types.summary =
             Types.make_json_output_summary
               ~total_issues:(List.length !all_issues)
               ~unused_functions:
                 (Json_reporter.count_by_type "unused_function" !all_issues)
               ~complexity:
                 (Json_reporter.count_by_type "complexity" !all_issues)
               ~naming:(Json_reporter.count_by_type "naming" !all_issues)
               ~length:(Json_reporter.count_by_type "length" !all_issues);
         }
       in
       Json_reporter.print_json json_output_data);

    !exit_code
