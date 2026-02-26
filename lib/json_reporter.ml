(* JSON reporter module - converts lint results to JSON format *)

open Types

(* Convert unused function to JSON issue *)
let unused_to_json (def : func_def) : json_issue =
  make_json_issue ~issue_type:"unused_function" ~file:def.source_file
    ~line:def.id.loc.line ~column:def.id.loc.column
    ~message:(Printf.sprintf "Unused function '%s'" def.id.name)
    ~module_name:(Some def.id.module_name) ~function_name:(Some def.id.name) ()

(* Convert complexity issue to JSON issue *)
let complexity_to_json threshold (issue : complexity_issue) : json_issue =
  make_json_issue ~issue_type:"complexity" ~file:issue.source_file
    ~line:issue.id.loc.line ~column:issue.id.loc.column
    ~message:
      (Printf.sprintf "Function '%s' has complexity %d (threshold: %d)"
         issue.id.name issue.complexity threshold)
    ~module_name:(Some issue.id.module_name) ~function_name:(Some issue.id.name)
    ~complexity:(Some issue.complexity) ~threshold:(Some threshold) ()

(* Convert naming violation to JSON issue *)
let naming_to_json (violation : naming_violation) : json_issue =
  make_json_issue ~issue_type:"naming" ~file:violation.source_file
    ~line:violation.loc.line ~column:violation.loc.column
    ~message:violation.violation_type ~name:(Some violation.name)
    ~violation_type:(Some violation.violation_type) ()

(* Convert length issue to JSON issue *)
let length_to_json threshold (issue : length_issue) : json_issue =
  make_json_issue ~issue_type:"length" ~file:issue.source_file
    ~line:issue.id.loc.line ~column:issue.id.loc.column
    ~message:
      (Printf.sprintf "Function '%s' has %d lines (threshold: %d)" issue.id.name
         issue.line_count threshold)
    ~module_name:(Some issue.id.module_name) ~function_name:(Some issue.id.name)
    ~threshold:(Some threshold) ~line_count:(Some issue.line_count) ()

(* Output JSON to stdout - now using derived yojson functions *)
let print_json (output : json_output) : unit =
  let json = json_output_to_yojson output in
  let json_str = Yojson.Safe.to_string json in
  Printf.printf "%s\n" json_str;
  flush stdout

(* Count issues by type for summary *)
let count_by_type issue_type issues =
  List.filter (fun issue -> issue.issue_type = issue_type) issues |> List.length
