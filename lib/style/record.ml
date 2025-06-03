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
