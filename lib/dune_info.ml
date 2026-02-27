(* Module for parsing dune command output *)

(* String set module *)
module String_set_key = struct
  type t = string [@@deriving ord]
end

module StringSet = Set.Make (String_set_key)

(* Check if a string contains a substring *)
let string_contains_substring s sub =
  let sub_len = String.length sub in
  let s_len = String.length s in
  let rec check i =
    if i + sub_len > s_len then false
    else if String.sub s i sub_len = sub then true
    else check (i + 1)
  in
  check 0

type unit_info = {
  unit_name : string;
  module_name : string; (* Full module name: Test_qualified_lib.Subdir.Inner *)
  short_module_name : string; (* Short module name: Subdir.Inner *)
  source_file : string;
  source_dir : string;
}

type dune_info = {
  units : unit_info list;
  source_dirs : string list;
  library_name : string; (* Extract library name from unit names *)
}

(* Parse UNIT_NAME field to extract module name *)
(* e.g., "test_qualified_lib__Module" -> "Test_qualified_lib.Module" *)
(* e.g., "test_qualified_lib__Subdir__Inner" -> "Test_qualified_lib.Subdir.Inner" *)
let unit_name_to_module_name (unit_name : string) : string =
  (* Replace __ with a temporary marker, then split *)
  let temp_marker = "\x00" in
  let with_markers =
    let rec replace s =
      let idx = try String.index_from s 0 '_' with Not_found -> -1 in
      if idx < 0 then s
      else if idx + 1 < String.length s && String.get s (idx + 1) = '_' then
        let before = String.sub s 0 idx in
        let after = String.sub s (idx + 2) (String.length s - idx - 2) in
        replace (before ^ temp_marker ^ after)
      else
        let before = String.sub s 0 idx in
        let after = String.sub s (idx + 1) (String.length s - idx - 1) in
        before ^ "_" ^ replace after
    in
    replace unit_name
  in
  (* Split by temp marker *)
  let parts = String.split_on_char '\x00' with_markers in
  (* Capitalize first letter of each component *)
  let capitalize_component s =
    if String.length s = 0 then s
    else
      let first = String.uppercase_ascii (String.sub s 0 1) in
      let rest =
        if String.length s > 1 then String.sub s 1 (String.length s - 1) else ""
      in
      first ^ rest
  in
  let capitalized_parts = List.map capitalize_component parts in
  String.concat "." capitalized_parts

(* Extract UNIT_NAME from a merlin config line *)
(* Returns the unit name if found, None otherwise *)
let extract_unit_name (line : string) : string option =
  (* Look for UNIT_NAME string in the line *)
  try
    if String.length line = 0 then (
      Printf.eprintf "DEBUG: extract_unit_name: Empty line\n";
      None)
    else if string_contains_substring line "UNIT_NAME " then
      (* Find "UNIT_NAME " in the line *)
      let rec find_unit_name_idx start_idx =
        if start_idx + 10 > String.length line then -1
        else
          let substr = try String.sub line start_idx 10 with _ -> "" in
          if substr = "UNIT_NAME " then start_idx
          else find_unit_name_idx (start_idx + 1)
      in
      let unit_name_idx = find_unit_name_idx 0 in
      if unit_name_idx >= 0 then
        let rest =
          String.sub line unit_name_idx (String.length line - unit_name_idx)
        in
        (* Extract the value between parentheses *)
        let value_start = 10 in
        (* After "UNIT_NAME " *)
        (* Find the closing parenthesis *)
        let rec find_close i =
          if i >= String.length rest then -1
          else if String.get rest i = ')' then i
          else find_close (i + 1)
        in
        let close_end = find_close value_start in
        if close_end >= 0 then (
          let unit_name =
            String.sub rest value_start (close_end - value_start)
          in
          Printf.eprintf "DEBUG: extract_unit_name: Found unit_name: %s\n"
            unit_name;
          Some unit_name)
        else (
          Printf.eprintf "DEBUG: extract_unit_name: No closing paren found\n";
          None)
      else (
        Printf.eprintf
          "DEBUG: extract_unit_name: UNIT_NAME not found (but \
           string_contains_substring said it was there)\n";
        None)
    else (
      Printf.eprintf
        "DEBUG: extract_unit_name: Line does not contain UNIT_NAME\n";
      None)
  with e ->
    Printf.eprintf "DEBUG: extract_unit_name: Exception: %s\n"
      (Printexc.to_string e);
    None

(* Parse dune ocaml merlin dump-config output *)
let parse_merlin_dump_config (output : string) : dune_info =
  let units = ref [] in
  let source_dirs = ref StringSet.empty in

  (* Parse each line *)
  let lines = String.split_on_char '\n' output in
  let rec parse_lines lines =
    match lines with
    | [] -> ()
    | line :: rest ->
        (* Debug: print lines containing UNIT_NAME *)
        (if String.length line > 0 && string_contains_substring line "UNIT" then
           let line_preview =
             if String.length line > 150 then String.sub line 0 150 ^ "..."
             else line
           in
           Printf.eprintf "DEBUG: parse_lines: line_with_UNIT=%s\n" line_preview);
        (* Skip empty lines *)
        if String.length line > 0 then (
          (* Look for (S ...) patterns for source directories *)
          let rec extract_s_dirs start_idx =
            if start_idx >= String.length line - 3 then []
            else
              match
                try Some (String.index_from line start_idx '(')
                with Not_found -> None
              with
              | Some open_idx ->
                  if
                    String.starts_with ~prefix:"(S "
                      (String.sub line open_idx (String.length line - open_idx))
                  then
                    (* Extract the directory path *)
                    let dir_start = open_idx + 3 in
                    let rec find_close i =
                      if i >= String.length line then None
                      else if String.get line i = ')' then Some i
                      else find_close (i + 1)
                    in
                    match find_close dir_start with
                    | Some close_idx ->
                        let dir =
                          String.sub line dir_start (close_idx - dir_start)
                        in
                        dir :: extract_s_dirs (close_idx + 1)
                    | None -> []
                  else extract_s_dirs (open_idx + 1)
              | None -> []
          in
          let dirs = extract_s_dirs 0 in
          List.iter
            (fun dir -> source_dirs := StringSet.add dir !source_dirs)
            dirs;

          (* Look for UNIT_NAME *)
          begin match extract_unit_name line with
          | Some unit_name ->
              Printf.eprintf "DEBUG: parse: Found unit_name: %s\n" unit_name;
              let full_module_name = unit_name_to_module_name unit_name in
              Printf.eprintf "DEBUG: parse: full_module_name: %s\n"
                full_module_name;
              (* Extract short module name by removing library prefix *)
              (* The library name is the first component before the first dot *)
              let short_module_name =
                let parts = String.split_on_char '.' full_module_name in
                match parts with
                | [] -> full_module_name
                | _ :: [] -> full_module_name
                | _ :: rest -> String.concat "." rest
              in
              Printf.eprintf "DEBUG: parse: short_module_name: %s\n"
                short_module_name;
              (* Build source file path from unit name *)
              (* Replace __ with / and add .ml extension *)
              let source_file_part =
                let rec replace_underscore_with_slash s =
                  let idx =
                    try String.index_from s 0 '_' with Not_found -> -1
                  in
                  if idx < 0 then s
                  else if
                    idx + 1 < String.length s && String.get s (idx + 1) = '_'
                  then
                    let before = String.sub s 0 idx in
                    let after =
                      String.sub s (idx + 2) (String.length s - idx - 2)
                    in
                    before ^ "/" ^ replace_underscore_with_slash after
                  else
                    let before = String.sub s 0 idx in
                    let after =
                      String.sub s (idx + 1) (String.length s - idx - 1)
                    in
                    before ^ "/" ^ replace_underscore_with_slash after
                in
                replace_underscore_with_slash unit_name
              in
              Printf.eprintf "DEBUG: parse: source_file_part: %s\n"
                source_file_part;
              units :=
                {
                  unit_name;
                  module_name = full_module_name;
                  short_module_name;
                  source_file = source_file_part ^ ".ml";
                  source_dir = "";
                }
                :: !units
          | None -> ()
          end);

        parse_lines rest
  in
  parse_lines lines;

  (* Extract library name from the first unit *)
  let library_name =
    match !units with
    | [] -> ""
    | first_unit :: _ -> (
        (* Extract library name from module_name *)
        (* The library name is the first component before the first dot *)
        let parts = String.split_on_char '.' first_unit.module_name in
        match parts with [] -> "" | lib_name :: _ -> lib_name)
  in

  {
    units = List.rev !units;
    source_dirs = StringSet.elements !source_dirs;
    library_name;
  }

(* Run dune ocaml merlin dump-config and parse output *)
let get_dune_info (dirs : string list) : dune_info option =
  (* Find the first directory that contains dune files *)
  (* Also check parent directories if the directory itself doesn't have dune files *)
  let rec find_dune_root dir =
    if
      Sys.file_exists (Filename.concat dir "dune")
      || Sys.file_exists (Filename.concat dir "dune-project")
    then Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None (* Reached root *) else find_dune_root parent
  in

  let target_dir = List.find_opt (fun dir -> find_dune_root dir <> None) dirs in
  match target_dir with
  | None -> None
  | Some dir -> (
      (* Use the dune root directory *)
      let root_dir =
        match find_dune_root dir with Some r -> r | None -> dir
      in
      try
        (* Try to find dune-workspace or dune-project to determine root *)
        let cmd =
          Printf.sprintf "cd %s && dune ocaml merlin dump-config 2>&1"
            (Filename.quote root_dir)
        in
        let ic = Unix.open_process_in cmd in
        let rec read_all acc =
          try
            let line = input_line ic in
            read_all (line :: acc)
          with End_of_file -> List.rev acc
        in
        let output = read_all [] in
        let exit_status = Unix.close_process_in ic in
        if exit_status = Unix.WEXITED 0 then
          Some (parse_merlin_dump_config (String.concat "\n" output))
        else None
      with _ -> None)
