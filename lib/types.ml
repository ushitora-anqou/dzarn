(* Location information *)
type loc = { file : string; line : int; column : int }

(* Function identifier *)
type func_id = { module_name : string; name : string; loc : loc }

(* Function definition *)
type func_def = { id : func_id; is_public : bool; source_file : string }

(* Analysis result *)
type result = { unused_functions : func_def list }

(* Complexity result *)
type complexity_issue = { id : func_id; complexity : int; source_file : string }
type complexity_result = { complex_functions : complexity_issue list }

(* Naming convention result *)
type naming_violation = {
  name : string;
  loc : loc;
  violation_type : string;
  source_file : string;
}

type naming_result = { violations : naming_violation list }

(* Function length result *)
type length_issue = { id : func_id; line_count : int; source_file : string }
type length_result = { long_functions : length_issue list }

(* Unused nolint directive *)
type unused_nolint = {
  loc : loc;
  linter_name : string; (* "naming", "complexity", "length", etc. *)
  source_file : string;
}

type unused_nolint_result = { unused_suppressions : unused_nolint list }

(* JSON output types *)
type json_issue = {
  issue_type : string; (* "unused_function", "complexity", "naming", "length" *)
  file : string;
  line : int;
  column : int;
  message : string;
  (* Optional fields depending on issue type *)
  module_name : string option;
  function_name : string option;
  name : string option; (* for naming violations *)
  violation_type : string option; (* for naming violations *)
  complexity : int option;
  threshold : int option;
  line_count : int option;
}

type json_output_summary = {
  total_issues : int;
  unused_functions : int;
  complexity : int;
  naming : int;
  length : int;
}

type json_output = { issues : json_issue list; summary : json_output_summary }
