open Parsetree
open Canonical (* For Pctxt, Hint, and Warn (which is Warnloc) *)

module Record : Check.STRUCTURECHECK = struct
  type t = Parsetree.structure_item_desc

  let name = "Record"
  let violation = "Function declaration with multi-line record arguments. Consider destructuring the record inside the function body, for example: `let { field1; field2; _ } = record_arg_name`."
  let fix_msg = "Refactor manually. Suggestion: Destructure the record argument inside the function body."

  let max_number_of_fields = 1 (* Maximum number of fields in a record pattern to avoid triggering the warning *)

  (* Helper Functions *)

  (** Counts named fields in a record pattern.
      Ignores fields specified with `_` as the name directly (e.g., `{_}`).
      Counts fields like `{ field = _ }` as named fields. *)
  let count_named_fields (p : pattern) : int =
    let rec aux = function
      | Ppat_record (fields, _) ->
          List.fold_left
            (fun acc_count (fname_loc, _) ->
              if fname_loc.Location.txt <> Longident.Lident "_"
              then acc_count + 1
              else acc_count)
            0 fields
      | Ppat_constraint (constrained_p, _) -> aux constrained_p.ppat_desc
      | Ppat_alias (aliased_p, _) -> aux aliased_p.ppat_desc
      | _ -> 0
    in
    aux p.ppat_desc
  ;;

  (** Traverses curried function arguments, seeking record patterns that contain multiple named fields.
      @param pfun The expression being analyzed, expected to be initially a function definition (Pexp_fun)
      @return whether such a record pattern is part of the arguments of the function *)
  let rec args_contains_record_pattern pfun =
    match pfun.pexp_desc with
    | Pexp_fun (_, _, pat, fun_body) ->
        count_named_fields pat > max_number_of_fields ||
        args_contains_record_pattern fun_body (* handle curryfication *)
    | _ -> false
  ;;

  (** Finds the end line number of the last argument pattern in a sequence of curried arguments.
      @param pat The pattern for the argument being considered
      @param body The body of the function expression, possibly curryfied
      @return The pattern for the final argument. *)
  let rec get_last_arg (pat: pattern) (body : expression) : pattern =
    match body.pexp_desc with
    | Pexp_fun (_, _, arg_pat, next_expr) -> get_last_arg arg_pat next_expr
    | _ -> pat
  ;;

  let check issues (Pctxt.P ctxt_record : t Pctxt.pctxt) =
    match ctxt_record.pattern with
    | Pstr_value (_, vbs) ->
        List.iter (fun vb ->
          match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
          | Ppat_var _, Pexp_fun (_, _, arg_pattern, fun_body_expr) ->
              (* Determine if the function signature (name + arguments) is multi-line *)
              let function_name_start_line = vb.pvb_pat.ppat_loc.loc_start.pos_lnum in
              let last_arg = get_last_arg arg_pattern fun_body_expr in
              let arguments_end_line = last_arg.ppat_loc.loc_end.pos_lnum in
              (* Check if the function signature spans multiple lines *)
              if function_name_start_line <> arguments_end_line
                && args_contains_record_pattern vb.pvb_expr
              then
                let warn_loc_for_hint = Warn.warn_loc_of_loc ctxt_record.source vb.pvb_loc in
                issues := Hint.mk_hint warn_loc_for_hint ctxt_record.source fix_msg violation :: !issues
          | _ -> ()
        ) vbs
    | _ -> ()
  ;;

end
;;
