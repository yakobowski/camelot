open Canonical
open Canonical.Pctxt
open Check
open Free_vars

module Lift : STRUCTURECHECK = struct
  type t = Parsetree.structure_item_desc

  let _ = "lift the nested function to the top-level"
  let violation = "nested function does not depend on its context"
  let min_lines = 5

  let check st (P {location; source; pattern}) =
    let open Parsetree in

    let rec traverse_expr context_vars expr =
        match expr.pexp_desc with
        | Pexp_fun (_, _, pat, body) ->
            let bound_vars = get_bound_vars_from_pattern pat in
            let new_context = SSet.union bound_vars context_vars in
            traverse_expr new_context body
        | Pexp_let (rec_flag, vbs, body) ->
            let bound_vars = List.fold_left (fun acc vb -> SSet.union (get_bound_vars_from_pattern vb.pvb_pat) acc) SSet.empty vbs in
            let context_for_bindings = if rec_flag = Recursive then SSet.union bound_vars context_vars else context_vars in

            List.iter (fun vb ->
              let is_function = match vb.pvb_expr.pexp_desc with | Pexp_fun _ -> true | _ -> false in
              if is_function then (
                let free_vars = SSet.of_list (free_variables_expr vb.pvb_expr) in
                let inner_bound_vars = get_bound_vars_from_pattern vb.pvb_pat in
                let free_vars_without_self = if rec_flag = Recursive then SSet.diff free_vars inner_bound_vars else free_vars in
                let intersection = SSet.inter free_vars_without_self context_for_bindings in

                if SSet.is_empty intersection && (vb.pvb_loc.loc_end.pos_lnum - vb.pvb_loc.loc_start.pos_lnum + 1) >= min_lines then
                    let hint_loc = Warn.warn_loc_of_loc location.file vb.pvb_loc in
                    let fix_message =
                      Printf.sprintf "lift the nested function from lines %d:%d-%d:%d to before line %d:%d"
                        hint_loc.line_start
                        hint_loc.col_start
                        hint_loc.line_end
                        hint_loc.col_end
                        location.line_start
                        location.col_start
                    in
                    st := Hint.mk_hint hint_loc source fix_message violation :: !st
              );
              traverse_expr context_for_bindings vb.pvb_expr
            ) vbs;

            let context_for_body = SSet.union bound_vars context_vars in
            traverse_expr context_for_body body
        | _ -> ()
    in

    match pattern with
    | Pstr_value (rec_flag, vbs) ->
        let bound_vars = List.fold_left (fun acc vb -> SSet.union (get_bound_vars_from_pattern vb.pvb_pat) acc) SSet.empty vbs in
        let context = if rec_flag = Recursive then bound_vars else SSet.empty in
        List.iter (fun vb -> traverse_expr context vb.pvb_expr) vbs;
    | _ -> ()

  let name = "Lift"
end
