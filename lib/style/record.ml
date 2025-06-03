open Parsetree
open Canonical (* For Pctxt, Hint, and Warn (which is Warnloc) *)

module Record : Check.STRUCTURECHECK = struct
  type t = Parsetree.structure_item_desc

  let name = "Record"
  let violation = "Function declaration with multi-line record arguments. Consider destructuring the record inside the function body, for example: `let { field1; field2; _ } = record_arg_name`."
  let fix_msg = "Refactor manually. Suggestion: Destructure the record argument inside the function body."

  (* Helper Functions *)

  (** Counts named fields in a record pattern.
      Ignores fields specified with `_` as the name directly (e.g., `{_}`).
      Counts fields like `{ field = _ }` as named fields. *)
  let count_named_fields (p : Parsetree.pattern) : int =
    let rec count_fields_recursive pattern_desc =
      match pattern_desc with
      | Ppat_record (fields, _) ->
          List.fold_left
            (fun acc_count (fname_loc, _) ->
              if fname_loc.Location.txt <> Longident.Lident "_" then acc_count + 1 else acc_count)
            0 fields
      | Ppat_constraint (constrained_p, _) -> count_fields_recursive constrained_p.ppat_desc
      | Ppat_alias (aliased_p, _) -> count_fields_recursive aliased_p.ppat_desc
      | _ -> 0
    in
    count_fields_recursive p.ppat_desc
  ;;

  (** Traverses curried function arguments, collecting details of record patterns.
      @param current_arg_pattern The pattern of the current function argument being processed.
      @param next_expression_after_arg The expression that follows the current_arg_pattern (could be another Pexp_fun or the function body).
      @param acc_field_counts Accumulator for (field_count, location) tuples for record patterns found so far.
      @return A tuple containing the final list of (field_count, location) for record arguments (in order of appearance)
              and the location of the final function body expression. *)
  let rec collect_record_args_recursive
    (current_arg_pattern : Parsetree.pattern)
    (next_expression_after_arg : Parsetree.expression)
    (acc_field_counts : (int * Location.t) list)
    : ((int * Location.t) list * Location.t) =

    let current_arg_loc = current_arg_pattern.ppat_loc in
    (* Directly call count_named_fields from the same module scope *)
    let fields_in_current_arg = count_named_fields current_arg_pattern in
    let updated_acc =
      if fields_in_current_arg > 0 then (fields_in_current_arg, current_arg_loc) :: acc_field_counts
      else acc_field_counts
    in

    match next_expression_after_arg.pexp_desc with
    | Pexp_fun (_, _, next_arg_pat, deeper_fun_body_expr) ->
        collect_record_args_recursive next_arg_pat deeper_fun_body_expr updated_acc
    | _ -> (List.rev updated_acc, next_expression_after_arg.pexp_loc)
  ;;

  (** Finds the end line number of the last argument pattern in a sequence of curried arguments.
      @param current_arg_end_line The end line number of the argument pattern currently being considered.
      @param current_expr_desc The description of the expression that follows the current argument (e.g., another Pexp_fun or the function body).
      @return The end line number of the pattern of the final argument in the curried sequence. *)
  let rec get_end_line_of_last_arg (current_arg_end_line : int) (current_expr_desc : Parsetree.expression_desc) : int =
    match current_expr_desc with
    | Pexp_fun (_, _, arg_pat, next_expr) ->
        get_end_line_of_last_arg arg_pat.ppat_loc.loc_end.pos_lnum next_expr.pexp_desc
    | _ ->
        current_arg_end_line
  ;;

  let check issues (Pctxt.P ctxt_record : t Pctxt.pctxt) =
    match ctxt_record.pattern with
    | Pstr_value (_, vbs) ->
        List.iter (fun vb ->
          match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
          | Ppat_var _, Pexp_fun (_, _, arg_pattern, _fun_body_expr) ->
              (* Determine if the function signature (name + arguments) is multi-line *)
              let function_name_start_line = vb.pvb_pat.ppat_loc.loc_start.pos_lnum in
              let arguments_end_line =
                get_end_line_of_last_arg arg_pattern.ppat_loc.loc_end.pos_lnum _fun_body_expr.pexp_desc
              in
              let is_multiline_signature = function_name_start_line <> arguments_end_line in

              (* Collect details of all record arguments in the function *)
              let (record_arg_details, _) =
                (* Call updated function without count_named_fields parameter *)
                collect_record_args_recursive arg_pattern _fun_body_expr []
              in

              (* Iterate over found record arguments and issue warnings if criteria are met *)
              List.iter (fun (field_count, (_ : Location.t)) -> (* arg_loc from record_arg_details is not used here *)
                if field_count >= 2 then
                  if is_multiline_signature then
                    let warn_loc_for_hint = Warn.warn_loc_of_loc ctxt_record.source vb.pvb_loc in
                    issues := Hint.mk_hint warn_loc_for_hint ctxt_record.source fix_msg violation :: !issues
              ) record_arg_details
          | _ -> ()
        ) vbs
    | _ -> ()
  ;;

end
;;
