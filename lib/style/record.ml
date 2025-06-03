open Parsetree
open Canonical (* For Pctxt, Hint, and Warn (which is Warnloc) *)
(* Check if Check needs to be opened or if Check.STRUCTURECHECK is automatically found if style is the library *)
(* It's likely Check.STRUCTURECHECK will need to be Style.Check.STRUCTURECHECK or open Style.Check *)
(* For now, let's assume it's visible or adjust later if there's a build error *)

module Record : Check.STRUCTURECHECK = struct
  type t = Parsetree.structure_item_desc

  let name = "Record" (* Updated name field *)
  let violation = "Function declaration with multi-line record arguments. Consider destructuring the record inside the function body, for example: `let { field1; field2; _ } = record_arg_name`."
  let fix_msg = "Refactor manually. Suggestion: Destructure the record argument inside the function body." (* This can remain for internal use *)

  let check issues (Pctxt.P ctxt_record : t Pctxt.pctxt) =
    match ctxt_record.pattern with
    | Pstr_value (_, vbs) ->
        List.iter (fun vb ->
          match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
          | Ppat_var _, Pexp_fun (_, _, arg_pattern, _fun_body_expr) ->
              (* Helper to iterate over potentially curried arguments *)
              let rec collect_record_args pat_expr_list acc_field_counts main_arg_loc =
                match pat_expr_list with
                | (arg_pat, next_expr) ->
                    let current_main_arg_loc = if Location.none = main_arg_loc then arg_pat.ppat_loc else main_arg_loc in
                    let rec count_named_fields p =
                      match p.ppat_desc with
                      | Ppat_record (fields, _) ->
                          List.fold_left
                            (fun acc_count (fname_loc, _) ->
                              if fname_loc.Location.txt <> Longident.Lident "_" then acc_count + 1 else acc_count)
                            0 fields
                      | Ppat_constraint (constrained_p, _) -> count_named_fields constrained_p
                       | Ppat_alias (aliased_p, _) -> count_named_fields aliased_p
                      | _ -> 0 (* Not a record or not the kind we're interested in counting fields for *)
                    in
                    let fields_in_current_arg = count_named_fields arg_pat in
                    let updated_acc = if fields_in_current_arg > 0 then (fields_in_current_arg, current_main_arg_loc) :: acc_field_counts else acc_field_counts in

                    (* Recurse if the function body is another function (currying) *)
                    match next_expr.pexp_desc with
                    | Pexp_fun (_, _, next_arg_pat, deeper_fun_body_expr) ->
                        collect_record_args (next_arg_pat, deeper_fun_body_expr) updated_acc current_main_arg_loc
                    | _ -> (updated_acc, next_expr.pexp_loc) (* Return accumulated counts and final body loc *)
              in

              let (record_arg_details, final_body_loc) = collect_record_args (arg_pattern, vb.pvb_expr) [] Location.none in

              List.iter (fun (field_count, (arg_loc : Location.t)) ->
                if field_count >= 2 then
                  let let_binding_start_line = vb.pvb_loc.loc_start.pos_lnum in
                  let patterns_section_end_line = arg_loc.loc_end.pos_lnum in
                  let body_start_line = final_body_loc.loc_start.pos_lnum in

                  let is_multiline_declaration =
                    let_binding_start_line <> patterns_section_end_line || patterns_section_end_line <> body_start_line
                  in

                  if is_multiline_declaration then
                    let warn_loc_for_hint = Warn.warn_loc_of_loc ctxt_record.source vb.pvb_loc in
                    issues := Hint.mk_hint warn_loc_for_hint ctxt_record.source fix_msg violation :: !issues
              ) record_arg_details
          | _ -> ()
        ) vbs
    | _ -> ()

end
