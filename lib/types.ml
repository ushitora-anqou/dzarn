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
