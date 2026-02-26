(* Support for [@nolint "..."], [@@nolint "..."], [@@@nolint "..."] attributes *)

(* Linter names that can be disabled *)
type linter = Naming | Complexity | Length | Unused | All

(* ================================================================= *)
(* Track nolint attribute usage to detect unused suppressions *)

(* A nolint attribute with its location information *)
type nolint_attr = { line : int; column : int; linters : linter list }

(* Usage tracker for nolint attributes *)
type usage_tracker = {
  mutable all_nolints : nolint_attr list;
  mutable used_nolints : (int * string list) list;
}

(* Create a new usage tracker *)
let create_usage_tracker () : usage_tracker =
  { all_nolints = []; used_nolints = [] }

(* Add a nolint attribute to the tracker *)
let add_nolint_to_tracker (tracker : usage_tracker) line column linters =
  tracker.all_nolints <- { line; column; linters } :: tracker.all_nolints

(* Mark a nolint as used for a specific linter *)
let mark_nolint_used (tracker : usage_tracker) line linter_name =
  let existing =
    try Some (Stdlib.List.assoc line tracker.used_nolints)
    with Not_found -> None
  in
  match existing with
  | None ->
      tracker.used_nolints <- (line, [ linter_name ]) :: tracker.used_nolints
  | Some names ->
      if not (Stdlib.List.mem linter_name names) then
        tracker.used_nolints <-
          (line, linter_name :: names)
          :: Stdlib.List.remove_assoc line tracker.used_nolints

(* Convert a linter type to its string name *)
let linter_to_string = function
  | Naming -> "naming"
  | Complexity -> "complexity"
  | Length -> "length"
  | Unused -> "unused"
  | All -> "all"

(* Find unused nolint attributes *)
let find_unused_nolints (tracker : usage_tracker) :
    (int * int * string list * string) list =
  Stdlib.List.filter
    (fun attr ->
      let used_linters =
        try Stdlib.List.assoc attr.line tracker.used_nolints
        with Not_found -> []
      in
      not
        (Stdlib.List.exists
           (fun l ->
             let linter_name = linter_to_string l in
             Stdlib.List.mem linter_name used_linters)
           attr.linters))
    tracker.all_nolints
  |> Stdlib.List.map (fun attr ->
      ( attr.line,
        attr.column,
        Stdlib.List.map linter_to_string attr.linters,
        "nolint" ))

(* Parse linter name from string *)
let parse_linter s =
  match String.lowercase_ascii s with
  | "naming" -> Some Naming
  | "complexity" -> Some Complexity
  | "length" -> Some Length
  | "unused" -> Some Unused
  | "all" -> Some All
  | _ -> None

(* Check if a specific linter should be disabled *)
let is_linter_disabled disabled_linters linter =
  (* Check if All is in the disabled list, or if the specific linter is *)
  Stdlib.List.mem All disabled_linters
  || Stdlib.List.mem linter disabled_linters

(* Extract nolint linters from a structure item's attribute payload *)
(* Handles Pstr_attribute items which represent [@@nolint "..."] and [@@@nolint "..."] *)
let rec extract_nolint_from_attribute_payload = function
  | Ppxlib.Parsetree.PStr structure ->
      (* Extract string constants from structure *)
      Stdlib.List.fold_left
        (fun acc item ->
          match item.Ppxlib.Parsetree.pstr_desc with
          | Ppxlib.Parsetree.Pstr_eval (expr, _) ->
              extract_linters_from_expr expr acc
          | _ -> acc)
        [] structure
  | Ppxlib.Parsetree.PSig _ -> []
  | Ppxlib.Parsetree.PTyp _ -> []
  | Ppxlib.Parsetree.PPat (_, _) -> []

and extract_linters_from_expr expr acc =
  match expr.Ppxlib.Parsetree.pexp_desc with
  | Ppxlib.Parsetree.Pexp_constant (Ppxlib.Parsetree.Pconst_string (s, _, _))
    -> (
      match parse_linter s with Some linter -> linter :: acc | None -> acc)
  | _ -> acc

(* Extract nolint linters from an attribute list *)
(* Used for [@nolint "..."] on expressions and patterns *)
let extract_nolint_from_attrs attributes =
  Stdlib.List.fold_left
    (fun acc (attr : Ppxlib.Parsetree.attribute) ->
      match attr.Ppxlib.Parsetree.attr_name.txt with
      | "nolint" ->
          extract_nolint_from_attribute_payload
            attr.Ppxlib.Parsetree.attr_payload
      | _ -> acc)
    [] attributes

(* Check if a specific linter is disabled in an attribute list *)
let is_linter_disabled_in_attrs attrs linter =
  (* Check if any nolint attribute disables this linter *)
  let has_nolint_for_linter =
    Stdlib.List.exists
      (fun (attr : Ppxlib.Parsetree.attribute) ->
        match attr.Ppxlib.Parsetree.attr_name.txt with
        | "nolint" -> (
            match
              extract_nolint_from_attribute_payload
                attr.Ppxlib.Parsetree.attr_payload
            with
            | [] -> false
            | linters -> is_linter_disabled linters linter)
        | _ -> false)
      attrs
  in
  has_nolint_for_linter
  || is_linter_disabled (extract_nolint_from_attrs attrs) linter

(* Get the line number from a location *)
let line_of_loc loc = loc.Ppxlib.Location.loc_start.pos_lnum

(* Check if a location is suppressed by any nolint attribute *)
(* Returns true if the linter should be disabled for the given location *)
let is_location_suppressed suppressed_lines loc linter =
  let line = line_of_loc loc in
  try
    let disabled_linters = Stdlib.List.assoc line suppressed_lines in
    is_linter_disabled disabled_linters linter
  with Not_found -> false

(* Collect all nolint suppressions from a structure *)
(* Returns a list of (line_number, linter list) pairs *)
let collect_suppressions structure =
  let rec collect_from_structure acc active_nolints items =
    (* active_nolints: [@@@nolint "..."] attributes that apply to all subsequent items *)
    match items with
    | [] -> acc
    | item :: rest ->
        (* Check if this item has inline [@@nolint "..."] attributes in pvb_attributes *)
        let has_inline_nolint, acc =
          match item.Ppxlib.Parsetree.pstr_desc with
          | Ppxlib.Parsetree.Pstr_value (_, bindings) ->
              (* Check for inline [@@nolint "..."] in pvb_attributes *)
              let nolints =
                Stdlib.List.fold_left
                  (fun acc binding ->
                    Stdlib.List.fold_left
                      (fun acc attr ->
                        match attr.Ppxlib.Parsetree.attr_name.txt with
                        | "nolint" -> extract_nolint_from_attrs [ attr ] @ acc
                        | _ -> acc)
                      acc binding.Ppxlib.Parsetree.pvb_attributes)
                  [] bindings
                |> Stdlib.List.rev
              in
              if nolints = [] then (false, acc)
              else
                (* Apply these nolints to this item's pattern locations *)
                let acc =
                  Stdlib.List.fold_left
                    (fun acc binding ->
                      let line =
                        line_of_loc
                          binding.Ppxlib.Parsetree.pvb_pat
                            .Ppxlib.Parsetree.ppat_loc
                      in
                      (line, nolints) :: acc)
                    acc bindings
                in
                (true, acc)
          | _ -> (false, acc)
        in
        (* Apply active nolints to this item (unless it has inline nolint) *)
        let acc =
          if has_inline_nolint then acc
          else
            match active_nolints with
            | [] -> acc
            | nolints ->
                (* Mark this item's line as suppressed *)
                let line = line_of_loc item.Ppxlib.Parsetree.pstr_loc in
                (line, nolints) :: acc
        in
        (* Check if this item is a [@@@nolint "..."] attribute *)
        let new_active_nolints =
          match item.Ppxlib.Parsetree.pstr_desc with
          | Ppxlib.Parsetree.Pstr_attribute attr -> (
              match attr.Ppxlib.Parsetree.attr_name.txt with
              | "nolint" ->
                  extract_nolint_from_attribute_payload
                    attr.Ppxlib.Parsetree.attr_payload
              | _ -> active_nolints)
          | _ -> active_nolints
        in
        (* Also check for expression-level attributes within structure items *)
        let acc =
          match item.Ppxlib.Parsetree.pstr_desc with
          | Ppxlib.Parsetree.Pstr_value (_, bindings) ->
              (* Don't re-check pvb_attributes here, already handled above *)
              Stdlib.List.fold_left collect_from_binding acc bindings
          | Ppxlib.Parsetree.Pstr_type (_, type_decls) ->
              Stdlib.List.fold_left collect_from_type_decl acc type_decls
          | _ -> acc
        in
        collect_from_structure acc new_active_nolints rest
  and collect_from_binding acc binding =
    (* Check pattern attributes for [@nolint "..."] *)
    let acc =
      Stdlib.List.fold_left
        (fun acc attr ->
          let nolints = extract_nolint_from_attrs [ attr ] in
          if nolints = [] then acc
          else
            let line = line_of_loc attr.Ppxlib.Parsetree.attr_name.loc in
            (line, nolints) :: acc)
        acc binding.Ppxlib.Parsetree.pvb_pat.Ppxlib.Parsetree.ppat_attributes
    in
    (* Check expression attributes for [@@nolint "..."] *)
    (* These are item-level attributes that should apply to the entire binding *)
    let acc =
      Stdlib.List.fold_left
        (fun acc attr ->
          match attr.Ppxlib.Parsetree.attr_name.txt with
          | "nolint" ->
              let nolints = extract_nolint_from_attrs [ attr ] in
              if nolints = [] then acc
              else
                (* Use pattern location for item-level expression attributes *)
                let line =
                  line_of_loc
                    binding.Ppxlib.Parsetree.pvb_pat.Ppxlib.Parsetree.ppat_loc
                in
                (line, nolints) :: acc
          | _ -> acc)
        acc binding.Ppxlib.Parsetree.pvb_expr.Ppxlib.Parsetree.pexp_attributes
    in
    (* Recurse into the expression for other attributes *)
    collect_from_expression acc binding.Ppxlib.Parsetree.pvb_expr
  and collect_from_expression acc expr =
    (* Check expression attributes for [@nolint "..."] *)
    let acc =
      Stdlib.List.fold_left
        (fun acc attr ->
          let nolints = extract_nolint_from_attrs [ attr ] in
          if nolints = [] then acc
          else
            let line = line_of_loc attr.Ppxlib.Parsetree.attr_name.loc in
            (line, nolints) :: acc)
        acc expr.Ppxlib.Parsetree.pexp_attributes
    in
    (* Recurse into sub-expressions *)
    match expr.Ppxlib.Parsetree.pexp_desc with
    | Ppxlib.Parsetree.Pexp_tuple items ->
        Stdlib.List.fold_left collect_from_expression acc items
    | Ppxlib.Parsetree.Pexp_record (fields, _) ->
        Stdlib.List.fold_left
          (fun acc (_, expr) -> collect_from_expression acc expr)
          acc fields
    | Ppxlib.Parsetree.Pexp_array items ->
        Stdlib.List.fold_left collect_from_expression acc items
    | Ppxlib.Parsetree.Pexp_sequence (e1, e2) ->
        collect_from_expression (collect_from_expression acc e1) e2
    | Ppxlib.Parsetree.Pexp_while (e1, e2) ->
        collect_from_expression (collect_from_expression acc e1) e2
    | Ppxlib.Parsetree.Pexp_for (_, _, _, _, e) -> collect_from_expression acc e
    | Ppxlib.Parsetree.Pexp_let (_, bindings, e) ->
        let acc = Stdlib.List.fold_left collect_from_binding acc bindings in
        collect_from_expression acc e
    | Ppxlib.Parsetree.Pexp_function (_, _, body) -> (
        match body with
        | Ppxlib.Parsetree.Pfunction_body e -> collect_from_expression acc e
        | Ppxlib.Parsetree.Pfunction_cases (cases, _, _) ->
            Stdlib.List.fold_left
              (fun acc c ->
                let acc =
                  match c.Ppxlib.Parsetree.pc_guard with
                  | Some e -> collect_from_expression acc e
                  | None -> acc
                in
                collect_from_expression acc c.Ppxlib.Parsetree.pc_rhs)
              acc cases)
    | Ppxlib.Parsetree.Pexp_apply (e, args) ->
        let acc = collect_from_expression acc e in
        Stdlib.List.fold_left
          (fun acc (_, arg) -> collect_from_expression acc arg)
          acc args
    | Ppxlib.Parsetree.Pexp_ifthenelse (e1, e2, e3) -> (
        let acc = collect_from_expression acc e1 in
        let acc = collect_from_expression acc e2 in
        match e3 with Some e -> collect_from_expression acc e | None -> acc)
    | Ppxlib.Parsetree.Pexp_try (e, cases) ->
        let acc = collect_from_expression acc e in
        Stdlib.List.fold_left
          (fun acc c ->
            let acc =
              match c.Ppxlib.Parsetree.pc_guard with
              | Some e -> collect_from_expression acc e
              | None -> acc
            in
            collect_from_expression acc c.Ppxlib.Parsetree.pc_rhs)
          acc cases
    | Ppxlib.Parsetree.Pexp_field (e, _) -> collect_from_expression acc e
    | Ppxlib.Parsetree.Pexp_setfield (e1, _, e2) ->
        collect_from_expression (collect_from_expression acc e1) e2
    | Ppxlib.Parsetree.Pexp_send (e, _) -> collect_from_expression acc e
    | Ppxlib.Parsetree.Pexp_letmodule (_, _, e) -> collect_from_expression acc e
    | Ppxlib.Parsetree.Pexp_letexception (_, e) -> collect_from_expression acc e
    | Ppxlib.Parsetree.Pexp_assert e -> collect_from_expression acc e
    | Ppxlib.Parsetree.Pexp_lazy e -> collect_from_expression acc e
    | Ppxlib.Parsetree.Pexp_poly (e, _) -> collect_from_expression acc e
    | Ppxlib.Parsetree.Pexp_open (_, e) -> collect_from_expression acc e
    | Ppxlib.Parsetree.Pexp_construct (_, Some e) ->
        collect_from_expression acc e
    | Ppxlib.Parsetree.Pexp_variant (_, Some e) -> collect_from_expression acc e
    | Ppxlib.Parsetree.Pexp_match (e, cases) ->
        let acc = collect_from_expression acc e in
        Stdlib.List.fold_left
          (fun acc c ->
            let acc =
              match c.Ppxlib.Parsetree.pc_guard with
              | Some e -> collect_from_expression acc e
              | None -> acc
            in
            collect_from_expression acc c.Ppxlib.Parsetree.pc_rhs)
          acc cases
    | _ -> acc
  and collect_from_type_decl acc type_decl =
    (* Check type declaration attributes *)
    let acc =
      Stdlib.List.fold_left
        (fun acc attr ->
          let nolints = extract_nolint_from_attrs [ attr ] in
          if nolints = [] then acc
          else
            let line = line_of_loc attr.Ppxlib.Parsetree.attr_name.loc in
            (line, nolints) :: acc)
        acc type_decl.Ppxlib.Parsetree.ptype_attributes
    in
    (* Check variant constructors for attributes *)
    match type_decl.Ppxlib.Parsetree.ptype_kind with
    | Ppxlib.Parsetree.Ptype_variant constructors ->
        Stdlib.List.fold_left
          (fun acc ctor ->
            Stdlib.List.fold_left
              (fun acc attr ->
                let nolints = extract_nolint_from_attrs [ attr ] in
                if nolints = [] then acc
                else
                  let line = line_of_loc attr.Ppxlib.Parsetree.attr_name.loc in
                  (line, nolints) :: acc)
              acc ctor.Ppxlib.Parsetree.pcd_attributes)
          acc constructors
    | _ -> acc
  in
  collect_from_structure [] [] structure

(* Collect all nolint suppressions and populate a usage tracker *)
let collect_suppressions_with_tracker structure (tracker : usage_tracker) =
  (* Helper to get column from location *)
  let column_of_loc loc =
    loc.Ppxlib.Location.loc_start.pos_cnum
    - loc.Ppxlib.Location.loc_start.pos_bol
  in
  let rec collect_from_structure acc active_nolints items =
    match items with
    | [] -> acc
    | item :: rest ->
        (* Check if this item has inline [@@nolint "..."] attributes in pvb_attributes *)
        let has_inline_nolint, acc =
          match item.Ppxlib.Parsetree.pstr_desc with
          | Ppxlib.Parsetree.Pstr_value (_, bindings) ->
              (* Check for inline [@@nolint "..."] in pvb_attributes *)
              let nolints =
                Stdlib.List.fold_left
                  (fun acc binding ->
                    Stdlib.List.fold_left
                      (fun acc attr ->
                        match attr.Ppxlib.Parsetree.attr_name.txt with
                        | "nolint" -> extract_nolint_from_attrs [ attr ] @ acc
                        | _ -> acc)
                      acc binding.Ppxlib.Parsetree.pvb_attributes)
                  [] bindings
                |> Stdlib.List.rev
              in
              if nolints = [] then (false, acc)
              else
                (* Apply these nolints to this item's pattern locations *)
                let acc =
                  Stdlib.List.fold_left
                    (fun acc binding ->
                      let line =
                        line_of_loc
                          binding.Ppxlib.Parsetree.pvb_pat
                            .Ppxlib.Parsetree.ppat_loc
                      in
                      let column =
                        column_of_loc
                          binding.Ppxlib.Parsetree.pvb_pat
                            .Ppxlib.Parsetree.ppat_loc
                      in
                      (* Add to tracker *)
                      add_nolint_to_tracker tracker line column nolints;
                      (line, nolints) :: acc)
                    acc bindings
                in
                (true, acc)
          | _ -> (false, acc)
        in
        (* Apply active nolints to this item (unless it has inline nolint) *)
        let acc =
          if has_inline_nolint then acc
          else
            match active_nolints with
            | [] -> acc
            | nolints ->
                let line = line_of_loc item.Ppxlib.Parsetree.pstr_loc in
                let column = column_of_loc item.Ppxlib.Parsetree.pstr_loc in
                (* Add to tracker *)
                add_nolint_to_tracker tracker line column nolints;
                (line, nolints) :: acc
        in
        (* Check if this item is a [@@@nolint "..."] attribute *)
        let new_active_nolints =
          match item.Ppxlib.Parsetree.pstr_desc with
          | Ppxlib.Parsetree.Pstr_attribute attr -> (
              match attr.Ppxlib.Parsetree.attr_name.txt with
              | "nolint" ->
                  let linters =
                    extract_nolint_from_attribute_payload
                      attr.Ppxlib.Parsetree.attr_payload
                  in
                  let line = line_of_loc attr.Ppxlib.Parsetree.attr_name.loc in
                  let column =
                    column_of_loc attr.Ppxlib.Parsetree.attr_name.loc
                  in
                  (* Add to tracker *)
                  add_nolint_to_tracker tracker line column linters;
                  linters
              | _ -> active_nolints)
          | _ -> active_nolints
        in
        (* Also check for expression-level attributes within structure items *)
        let acc =
          match item.Ppxlib.Parsetree.pstr_desc with
          | Ppxlib.Parsetree.Pstr_value (_, bindings) ->
              (* Don't re-check pvb_attributes here, already handled above *)
              Stdlib.List.fold_left
                (collect_from_binding_with_tracker tracker)
                acc bindings
          | Ppxlib.Parsetree.Pstr_type (_, type_decls) ->
              Stdlib.List.fold_left
                (collect_from_type_decl_with_tracker tracker)
                acc type_decls
          | _ -> acc
        in
        collect_from_structure acc new_active_nolints rest
  and collect_from_binding_with_tracker tracker acc binding =
    (* Check pattern attributes for [@nolint "..."] *)
    let acc =
      Stdlib.List.fold_left
        (fun acc attr ->
          let nolints = extract_nolint_from_attrs [ attr ] in
          if nolints = [] then acc
          else
            let line = line_of_loc attr.Ppxlib.Parsetree.attr_name.loc in
            let column = column_of_loc attr.Ppxlib.Parsetree.attr_name.loc in
            (* Add to tracker *)
            add_nolint_to_tracker tracker line column nolints;
            (line, nolints) :: acc)
        acc binding.Ppxlib.Parsetree.pvb_pat.Ppxlib.Parsetree.ppat_attributes
    in
    (* Check expression attributes for [@@nolint "..."] *)
    let acc =
      Stdlib.List.fold_left
        (fun acc attr ->
          match attr.Ppxlib.Parsetree.attr_name.txt with
          | "nolint" ->
              let nolints = extract_nolint_from_attrs [ attr ] in
              if nolints = [] then acc
              else
                let line =
                  line_of_loc
                    binding.Ppxlib.Parsetree.pvb_pat.Ppxlib.Parsetree.ppat_loc
                in
                let column =
                  column_of_loc
                    binding.Ppxlib.Parsetree.pvb_pat.Ppxlib.Parsetree.ppat_loc
                in
                (* Add to tracker *)
                add_nolint_to_tracker tracker line column nolints;
                (line, nolints) :: acc
          | _ -> acc)
        acc binding.Ppxlib.Parsetree.pvb_expr.Ppxlib.Parsetree.pexp_attributes
    in
    (* Recurse into the expression for other attributes *)
    collect_from_expression_with_tracker tracker acc
      binding.Ppxlib.Parsetree.pvb_expr
  and collect_from_expression_with_tracker tracker acc expr =
    (* Check expression attributes for [@nolint "..."] *)
    let acc =
      Stdlib.List.fold_left
        (fun acc attr ->
          let nolints = extract_nolint_from_attrs [ attr ] in
          if nolints = [] then acc
          else
            let line = line_of_loc attr.Ppxlib.Parsetree.attr_name.loc in
            let column = column_of_loc attr.Ppxlib.Parsetree.attr_name.loc in
            (* Add to tracker *)
            add_nolint_to_tracker tracker line column nolints;
            (line, nolints) :: acc)
        acc expr.Ppxlib.Parsetree.pexp_attributes
    in
    (* Recurse into sub-expressions *)
    match expr.Ppxlib.Parsetree.pexp_desc with
    | Ppxlib.Parsetree.Pexp_tuple items ->
        Stdlib.List.fold_left
          (collect_from_expression_with_tracker tracker)
          acc items
    | Ppxlib.Parsetree.Pexp_record (fields, _) ->
        Stdlib.List.fold_left
          (fun acc (_, expr) ->
            collect_from_expression_with_tracker tracker acc expr)
          acc fields
    | Ppxlib.Parsetree.Pexp_array items ->
        Stdlib.List.fold_left
          (collect_from_expression_with_tracker tracker)
          acc items
    | Ppxlib.Parsetree.Pexp_sequence (e1, e2) ->
        collect_from_expression_with_tracker tracker
          (collect_from_expression_with_tracker tracker acc e1)
          e2
    | Ppxlib.Parsetree.Pexp_while (e1, e2) ->
        collect_from_expression_with_tracker tracker
          (collect_from_expression_with_tracker tracker acc e1)
          e2
    | Ppxlib.Parsetree.Pexp_for (_, _, _, _, e) ->
        collect_from_expression_with_tracker tracker acc e
    | Ppxlib.Parsetree.Pexp_let (_, bindings, e) ->
        let acc =
          Stdlib.List.fold_left
            (collect_from_binding_with_tracker tracker)
            acc bindings
        in
        collect_from_expression_with_tracker tracker acc e
    | Ppxlib.Parsetree.Pexp_function (_, _, body) -> (
        match body with
        | Ppxlib.Parsetree.Pfunction_body e ->
            collect_from_expression_with_tracker tracker acc e
        | Ppxlib.Parsetree.Pfunction_cases (cases, _, _) ->
            Stdlib.List.fold_left
              (fun acc c ->
                let acc =
                  match c.Ppxlib.Parsetree.pc_guard with
                  | Some e -> collect_from_expression_with_tracker tracker acc e
                  | None -> acc
                in
                collect_from_expression_with_tracker tracker acc
                  c.Ppxlib.Parsetree.pc_rhs)
              acc cases)
    | Ppxlib.Parsetree.Pexp_apply (e, args) ->
        let acc = collect_from_expression_with_tracker tracker acc e in
        Stdlib.List.fold_left
          (fun acc (_, arg) ->
            collect_from_expression_with_tracker tracker acc arg)
          acc args
    | Ppxlib.Parsetree.Pexp_ifthenelse (e1, e2, e3) -> (
        let acc = collect_from_expression_with_tracker tracker acc e1 in
        let acc = collect_from_expression_with_tracker tracker acc e2 in
        match e3 with
        | Some e -> collect_from_expression_with_tracker tracker acc e
        | None -> acc)
    | Ppxlib.Parsetree.Pexp_try (e, cases) ->
        let acc = collect_from_expression_with_tracker tracker acc e in
        Stdlib.List.fold_left
          (fun acc c ->
            let acc =
              match c.Ppxlib.Parsetree.pc_guard with
              | Some e -> collect_from_expression_with_tracker tracker acc e
              | None -> acc
            in
            collect_from_expression_with_tracker tracker acc
              c.Ppxlib.Parsetree.pc_rhs)
          acc cases
    | Ppxlib.Parsetree.Pexp_field (e, _) ->
        collect_from_expression_with_tracker tracker acc e
    | Ppxlib.Parsetree.Pexp_setfield (e1, _, e2) ->
        collect_from_expression_with_tracker tracker
          (collect_from_expression_with_tracker tracker acc e1)
          e2
    | Ppxlib.Parsetree.Pexp_send (e, _) ->
        collect_from_expression_with_tracker tracker acc e
    | Ppxlib.Parsetree.Pexp_letmodule (_, _, e) ->
        collect_from_expression_with_tracker tracker acc e
    | Ppxlib.Parsetree.Pexp_letexception (_, e) ->
        collect_from_expression_with_tracker tracker acc e
    | Ppxlib.Parsetree.Pexp_assert e ->
        collect_from_expression_with_tracker tracker acc e
    | Ppxlib.Parsetree.Pexp_lazy e ->
        collect_from_expression_with_tracker tracker acc e
    | Ppxlib.Parsetree.Pexp_poly (e, _) ->
        collect_from_expression_with_tracker tracker acc e
    | Ppxlib.Parsetree.Pexp_open (_, e) ->
        collect_from_expression_with_tracker tracker acc e
    | Ppxlib.Parsetree.Pexp_construct (_, Some e) ->
        collect_from_expression_with_tracker tracker acc e
    | Ppxlib.Parsetree.Pexp_variant (_, Some e) ->
        collect_from_expression_with_tracker tracker acc e
    | Ppxlib.Parsetree.Pexp_match (e, cases) ->
        let acc = collect_from_expression_with_tracker tracker acc e in
        Stdlib.List.fold_left
          (fun acc c ->
            let acc =
              match c.Ppxlib.Parsetree.pc_guard with
              | Some e -> collect_from_expression_with_tracker tracker acc e
              | None -> acc
            in
            collect_from_expression_with_tracker tracker acc
              c.Ppxlib.Parsetree.pc_rhs)
          acc cases
    | _ -> acc
  and collect_from_type_decl_with_tracker tracker acc type_decl =
    (* Check type declaration attributes *)
    let acc =
      Stdlib.List.fold_left
        (fun acc attr ->
          let nolints = extract_nolint_from_attrs [ attr ] in
          if nolints = [] then acc
          else
            let line = line_of_loc attr.Ppxlib.Parsetree.attr_name.loc in
            let column = column_of_loc attr.Ppxlib.Parsetree.attr_name.loc in
            (* Add to tracker *)
            add_nolint_to_tracker tracker line column nolints;
            (line, nolints) :: acc)
        acc type_decl.Ppxlib.Parsetree.ptype_attributes
    in
    (* Check variant constructors for attributes *)
    match type_decl.Ppxlib.Parsetree.ptype_kind with
    | Ppxlib.Parsetree.Ptype_variant constructors ->
        Stdlib.List.fold_left
          (fun acc ctor ->
            Stdlib.List.fold_left
              (fun acc attr ->
                let nolints = extract_nolint_from_attrs [ attr ] in
                if nolints = [] then acc
                else
                  let line = line_of_loc attr.Ppxlib.Parsetree.attr_name.loc in
                  let column =
                    column_of_loc attr.Ppxlib.Parsetree.attr_name.loc
                  in
                  (* Add to tracker *)
                  add_nolint_to_tracker tracker line column nolints;
                  (line, nolints) :: acc)
              acc ctor.Ppxlib.Parsetree.pcd_attributes)
          acc constructors
    | _ -> acc
  in
  collect_from_structure [] [] structure
