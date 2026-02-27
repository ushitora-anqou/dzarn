(* Location information *)
type loc = { file : string; line : int; column : int }
[@@deriving yojson, show, eq, make]

(* Location range for text-based deletion *)
type loc_range = {
  file : string;
  start_line : int;
  end_line : int;
  start_char : int;
  end_char : int;
}
[@@deriving show, make]

(* Function identifier *)
type func_id = { module_name : string; name : string; loc : loc }
[@@deriving yojson, show, eq, make]

(* Function definition *)
type func_def = { id : func_id; is_public : bool; source_file : string }
[@@deriving yojson, show, eq, make]

(* Analysis result *)
type result = { unused_functions : func_def list }
[@@deriving yojson, show, eq, make]

(* Complexity result *)
type complexity_issue = { id : func_id; complexity : int; source_file : string }
[@@deriving yojson, show, eq, make]

type complexity_result = { complex_functions : complexity_issue list }
[@@deriving yojson, show, eq, make]

(* Naming convention result *)
type naming_violation = {
  name : string;
  loc : loc;
  violation_type : string;
  source_file : string;
}
[@@deriving yojson, show, eq, make]

type naming_result = { violations : naming_violation list }
[@@deriving yojson, show, eq, make]

(* Function length result *)
type length_issue = { id : func_id; line_count : int; source_file : string }
[@@deriving yojson, show, eq, make]

type length_result = { long_functions : length_issue list }
[@@deriving yojson, show, eq, make]

(* Unused nolint directive *)
type unused_nolint = {
  loc : loc;
  linter_name : string; (* "naming", "complexity", "length", etc. *)
  source_file : string;
}
[@@deriving yojson, show, eq, make]

type unused_nolint_result = { unused_suppressions : unused_nolint list }
[@@deriving yojson, show, eq, make]

(* JSON output types *)
type json_issue = {
  issue_type : string; (* "unused_function", "complexity", "naming", "length" *)
  file : string;
  line : int;
  column : int;
  message : string;
  (* Optional fields depending on issue type *)
  module_name : string option; [@default None]
  function_name : string option; [@default None]
  name : string option; [@default None] (* for naming violations *)
  violation_type : string option; [@default None] (* for naming violations *)
  complexity : int option; [@default None]
  threshold : int option; [@default None]
  line_count : int option; [@default None]
}
[@@deriving yojson, show, eq, make]

type json_output_summary = {
  total_issues : int;
  unused_functions : int;
  complexity : int;
  naming : int;
  length : int;
}
[@@deriving yojson, show, eq, make]

type json_output = { issues : json_issue list; summary : json_output_summary }
[@@deriving yojson, show, eq, make]
