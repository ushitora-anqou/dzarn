(* JSON reporter module - converts lint results to JSON format *)

open Types

(* Convert unused function to JSON issue *)
let unused_to_json (def : func_def) : json_issue =
  {
    issue_type = "unused_function";
    file = def.source_file;
    line = def.id.loc.line;
    column = def.id.loc.column;
    message = Printf.sprintf "Unused function '%s'" def.id.name;
    module_name = Some def.id.module_name;
    function_name = Some def.id.name;
    name = None;
    violation_type = None;
    complexity = None;
    threshold = None;
    line_count = None;
  }

(* Convert complexity issue to JSON issue *)
let complexity_to_json threshold (issue : complexity_issue) : json_issue =
  {
    issue_type = "complexity";
    file = issue.source_file;
    line = issue.id.loc.line;
    column = issue.id.loc.column;
    message =
      Printf.sprintf "Function '%s' has complexity %d (threshold: %d)"
        issue.id.name issue.complexity threshold;
    module_name = Some issue.id.module_name;
    function_name = Some issue.id.name;
    name = None;
    violation_type = None;
    complexity = Some issue.complexity;
    threshold = Some threshold;
    line_count = None;
  }

(* Convert naming violation to JSON issue *)
let naming_to_json (violation : naming_violation) : json_issue =
  {
    issue_type = "naming";
    file = violation.source_file;
    line = violation.loc.line;
    column = violation.loc.column;
    message = violation.violation_type;
    module_name = None;
    function_name = None;
    name = Some violation.name;
    violation_type = Some violation.violation_type;
    complexity = None;
    threshold = None;
    line_count = None;
  }

(* Convert length issue to JSON issue *)
let length_to_json threshold (issue : length_issue) : json_issue =
  {
    issue_type = "length";
    file = issue.source_file;
    line = issue.id.loc.line;
    column = issue.id.loc.column;
    message =
      Printf.sprintf "Function '%s' has %d lines (threshold: %d)" issue.id.name
        issue.line_count threshold;
    module_name = Some issue.id.module_name;
    function_name = Some issue.id.name;
    name = None;
    violation_type = None;
    complexity = None;
    threshold = Some threshold;
    line_count = Some issue.line_count;
  }

(* Convert JSON issue to Yojson json *)
let json_issue_to_yojson (issue : json_issue) : Yojson.Safe.t =
  let fields =
    [
      ("type", `String issue.issue_type);
      ("file", `String issue.file);
      ("line", `Int issue.line);
      ("column", `Int issue.column);
      ("message", `String issue.message);
    ]
  in
  let fields =
    match issue.module_name with
    | None -> fields
    | Some m -> ("module", `String m) :: fields
  in
  let fields =
    match issue.function_name with
    | None -> fields
    | Some f -> ("function", `String f) :: fields
  in
  let fields =
    match issue.name with
    | None -> fields
    | Some n -> ("name", `String n) :: fields
  in
  let fields =
    match issue.violation_type with
    | None -> fields
    | Some vt -> ("violation_type", `String vt) :: fields
  in
  let fields =
    match issue.complexity with
    | None -> fields
    | Some c -> ("complexity", `Int c) :: fields
  in
  let fields =
    match issue.threshold with
    | None -> fields
    | Some t -> ("threshold", `Int t) :: fields
  in
  let fields =
    match issue.line_count with
    | None -> fields
    | Some lc -> ("line_count", `Int lc) :: fields
  in
  `Assoc fields

(* Convert JSON summary to Yojson json *)
let json_output_summary_to_yojson (summary : json_output_summary) :
    Yojson.Safe.t =
  `Assoc
    [
      ("total_issues", `Int summary.total_issues);
      ("unused_functions", `Int summary.unused_functions);
      ("complexity", `Int summary.complexity);
      ("naming", `Int summary.naming);
      ("length", `Int summary.length);
    ]

(* Convert full JSON output to Yojson json *)
let json_output_to_yojson (output : json_output) : Yojson.Safe.t =
  `Assoc
    [
      ("issues", `List (List.map json_issue_to_yojson output.issues));
      ("summary", json_output_summary_to_yojson output.summary);
    ]

(* Output JSON to stdout *)
let print_json (output : json_output) : unit =
  let json = json_output_to_yojson output in
  let json_str = Yojson.Safe.to_string json in
  Printf.printf "%s\n" json_str;
  flush stdout

(* Count issues by type for summary *)
let count_by_type issue_type issues =
  List.filter (fun issue -> issue.issue_type = issue_type) issues |> List.length
