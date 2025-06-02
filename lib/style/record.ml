open Canonical
open Canonical.Pctxt (* Added for direct use of E *)
open Check
open Parsetree
open Asttypes

(*
  Note: Pprintast is used with Pprintast.string_of_expression, so an open is not strictly needed.
*)

(** ------------ Checks rules: use of record update syntax ------------------ *)
module UseRecordUpdateSyntax : EXPRCHECK = struct
  type t = expression_desc (* Parsetree.expression_desc -> expression_desc due to open Parsetree *)
  let violation = "Use record update syntax ({ r with ... }) when copying multiple fields."
  let name = "UseRecordUpdateSyntax"

  module StringMap = Map.Make(String)

  let check st (E {location; source; pattern}) =  (* Now E can be used directly *)
    match pattern with
    | Pexp_record (original_fields, None) -> (* Only apply to full record creation, not { base with ... } *)
      (* First pass: Group fields by their source expression string to find candidates for update syntax *)
      let source_to_copied_fields_map =
        List.fold_left (fun acc (_defined_label, (assigned_expr : expression)) -> (* assigned_expr is Parsetree.expression *)
          match assigned_expr.pexp_desc with
          | Pexp_field (source_rec_expr, _source_field_label) ->
            let source_rec_expr_str = Pprintast.string_of_expression source_rec_expr in
            let current_list = try StringMap.find source_rec_expr_str acc with Not_found -> [] in
            StringMap.add source_rec_expr_str (source_rec_expr :: current_list) acc
          | _ -> acc
        ) StringMap.empty original_fields
      in

      StringMap.iter (fun source_expr_str_for_update source_expr_list ->
        if List.length source_expr_list >= 2 then (* This source is used for >= 2 fields *)
          let representative_source_expr = List.hd source_expr_list in 
          let differing_field_strings =
            List.fold_left (fun acc_diff ((defined_label_lid_loc : Longident.t loc), (assigned_expr : expression)) ->
              let is_copied_from_current_source =
                match assigned_expr.pexp_desc with
                | Pexp_field (src_expr, _src_lbl) ->
                  (Pprintast.string_of_expression src_expr) = source_expr_str_for_update
                | _ -> false
              in

              if not is_copied_from_current_source then
                (* Use Longident.last to get the string name from Lident *)
                let field_name_str = Longident.last defined_label_lid_loc.txt in (* .txt should now be resolved from open Asttypes *)
                let field_expr_str = Pprintast.string_of_expression assigned_expr in
                (field_name_str ^ " = " ^ field_expr_str) :: acc_diff
              else
                acc_diff
            ) [] original_fields
          in

          let base_record_str = Pprintast.string_of_expression representative_source_expr in
          let final_fix_str =
            match differing_field_strings with
            | [] -> base_record_str (* All fields were copied, suggest just the base record *)
            | nonEmpty_diff_fields ->
              "{ " ^ base_record_str ^ " with " ^ (String.concat "; " (List.rev nonEmpty_diff_fields)) ^ " }"
          in
          st := Hint.mk_hint location source final_fix_str violation :: !st
      ) source_to_copied_fields_map
    | _ -> ()
end
