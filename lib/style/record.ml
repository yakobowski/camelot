open Canonical
open Canonical.Pctxt
open Check
open Parsetree
open Asttypes

(** ------------ Checks rules: use of record update syntax ------------------ *)
module UseRecordUpdateSyntax : EXPRCHECK = struct
  type t = expression_desc
  let violation = "Use record update syntax ({ r with ... }) when copying multiple fields."
  let name = "UseRecordUpdateSyntax"

  module StringMap = Map.Make(String)

  (* Type for righ-hand part of a record creation *)
  type assigned_fields = (Longident.t loc * expression) list

  (* Given the assignments for a record creation, gather the fields that are
    being initialized from the same field from another record, from the same field.
     The key is the string representation of the source record, and the value is
     - the source record expression
     - the list of fields that are being copied. *)
  let gather_source_fields (fields: assigned_fields) =
    List.fold_left
    (fun acc ({ txt = lbl; _}, assigned_expr) ->
      match assigned_expr.pexp_desc with
      | Pexp_field (src_rec, {txt = lbl'; _}) ->
        (* Do source and assigned fields names match? *)
        if lbl = lbl' then
          (* Use the string representation of the source expression as the key *)
          let src_rec_str = Pprintast.string_of_expression src_rec in
          let lbls =
            try snd (StringMap.find src_rec_str acc)
            with Not_found -> []
          in
          StringMap.add src_rec_str (src_rec, lbl :: lbls) acc
        else
          acc
      | _ -> acc
  ) StringMap.empty fields

  (* Given a list of fields, return the fields that are not in the given labels.
     This is used to determine which fields are _not_ being copied from the source record. *)
  let fields_to_update (fields: assigned_fields) labels =
    List.fold_left (fun acc_diff (lbl, expr) ->
      if not (List.mem lbl.txt labels) then
        (lbl, expr) :: acc_diff
      else
        acc_diff
    ) [] fields

  let check st (E {location; source; pattern}) =
    match pattern with
    | Pexp_record (original_fields, None) -> (* None -> only apply to full record creation, not { base with ... } *)
      let source_rec_map = gather_source_fields original_fields in
      StringMap.iter (fun _ (src_rec, labels) ->
        if List.length labels >= 2 then (* This source is used for >= 2 fields *)
          let other_fields = List.rev (fields_to_update original_fields labels) in
          (* Build replacement expression *)
          let new_rec =
            if other_fields = []
            then src_rec (* full copy *)
            else Ast_helper.Exp.mk (Pexp_record (other_fields, Some src_rec))
          in
          let fix = Pprintast.string_of_expression new_rec in
          st := Hint.mk_hint location source fix violation :: !st
      ) source_rec_map
    | _ -> ()
end

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
