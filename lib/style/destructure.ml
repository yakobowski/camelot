open Canonical
open Canonical.Pctxt
open Check
open Parsetree
open Asttypes
open Utils
open Ast_iterator
open Free_vars
open Side_effects

module Destructure : EXPRCHECK = struct
  type t = expression_desc
  let violation = "Unnecessary record destructuring. This variable is only used once."
  let name = "Destructure"

  (** Counts the number of occurrences of a variable in an expression. *)
  let count_occurrences var_name expr =
    let count = ref 0 in
    let bound_vars = ref SSet.empty in
    let iterator =
      {
        Ast_iterator.default_iterator with
        expr =
          (fun iterator expr ->
            match expr.pexp_desc with
            | Pexp_ident { txt = Lident x; _ } when x = var_name && not (SSet.mem x !bound_vars) ->
                count := !count + 1;
                Ast_iterator.default_iterator.expr iterator expr
            | Pexp_fun (_, _, pat, body) ->
                let original_bound = !bound_vars in
                let bindings = Free_vars.get_bound_vars_from_pattern pat in
                bound_vars := SSet.union bindings !bound_vars;
                iterator.expr iterator body;
                bound_vars := original_bound
            | Pexp_let (rec_flag, vbs, body) ->
                let original_bound = !bound_vars in
                let bindings = List.fold_left (fun acc vb -> SSet.union (Free_vars.get_bound_vars_from_pattern vb.pvb_pat) acc) SSet.empty vbs in

                let rhs_bound_vars = if rec_flag = Recursive then SSet.union bindings !bound_vars else !bound_vars in
                bound_vars := rhs_bound_vars;
                List.iter (fun vb -> iterator.expr iterator vb.pvb_expr) vbs;

                let body_bound_vars = SSet.union bindings original_bound in
                bound_vars := body_bound_vars;
                iterator.expr iterator body;

                bound_vars := original_bound
            | _ -> Ast_iterator.default_iterator.expr iterator expr);
      }
    in
    iterator.expr iterator expr;
    !count

    (** Checks if a variable is shadowed in an expression. *)
    let is_var_shadowed var_name expr =
        let shadowed = ref false in
        let iterator =
          {
            Ast_iterator.default_iterator with
            pat =
              (fun iterator p ->
                match p.ppat_desc with
                | Ppat_var { txt; _ } when txt = var_name -> shadowed := true
                | Ppat_alias (_, { txt; _ }) when txt = var_name -> shadowed := true
                | _ -> Ast_iterator.default_iterator.pat iterator p);
          }
        in
        iterator.expr iterator expr;
        !shadowed

  (** Checks if any of the free variables in a record expression are shadowed in the body. *)
  let is_shadowed record_expr body =
      let free_vars = Free_vars.free_variables_expr record_expr in
      List.exists (fun var -> is_var_shadowed var body) free_vars

  (** Extracts the variable bindings from a record pattern. *)
  let extract_bindings record_expr_str pattern =
    let rec aux prefix acc pat =
      match pat.ppat_desc with
      | Ppat_record (fields, _) ->
          List.fold_left
            (fun acc (field, pat) ->
              let field_str =
                match field.txt with
                | Longident.Lident s -> s
                | Longident.Ldot (l, s) ->
                    let l_str = String.concat "." (Longident.flatten l) in
                    l_str ^ "." ^ s
                | _ -> ""
              in
              let new_prefix = if prefix = "" then field_str else prefix ^ "." ^ field_str in
              match pat.ppat_desc with
              | Ppat_var { txt; _ } ->
                  (txt, record_expr_str ^ "." ^ new_prefix) :: acc
              | Ppat_alias (sub_pat, { txt; _ }) ->
                  let acc' = (txt, record_expr_str ^ "." ^ new_prefix) :: acc in
                  aux new_prefix acc' sub_pat
              | Ppat_record _ ->
                  aux new_prefix acc pat
              | _ -> acc)
            acc
            fields
      | Ppat_alias (sub_pat, _) ->
          aux prefix acc sub_pat
      | _ -> acc
    in
    aux "" [] pattern

  (** The main check function. *)
  let check st ~rules:_ (E {location; source; pattern}) =
    let process_let_binding vb body =
        match vb.pvb_pat.ppat_desc with
        | Ppat_record _ ->
            if not (has_side_effects vb.pvb_expr) then
              let record_expr_str = Pprintast.string_of_expression vb.pvb_expr in
              let bindings = extract_bindings record_expr_str vb.pvb_pat in
              List.iter
                (fun (var_name, replacement) ->
                  if count_occurrences var_name body = 1 && not (is_shadowed vb.pvb_expr body) then
                    let fix = "Replace " ^ var_name ^ " with " ^ replacement in
                    let warn_loc = Warn.warn_loc_of_loc location.file vb.pvb_pat.ppat_loc in
                    st := Hint.mk_hint warn_loc source fix violation :: !st)
                bindings
        | _ -> ()
    in

    let process_fun_arg arg body =
        match arg.ppat_desc with
        | Ppat_alias ({ppat_desc = Ppat_record _; _}, {txt = record_name; _}) ->
            let bindings = extract_bindings record_name arg in
            List.iter
              (fun (var_name, replacement) ->
                if count_occurrences var_name body = 1 && not (is_var_shadowed record_name body) then
                  let fix = "Replace " ^ var_name ^ " with " ^ replacement in
                  let warn_loc = Warn.warn_loc_of_loc location.file arg.ppat_loc in
                  st := Hint.mk_hint warn_loc source fix violation :: !st)
              bindings
        | Ppat_record _ ->
            let record_name = "record" in (* Placeholder name *)
            let bindings = extract_bindings record_name arg in
            List.iter
              (fun (var_name, replacement) ->
                if count_occurrences var_name body = 1 && not (is_var_shadowed record_name body) then
                  let fix = "Replace " ^ var_name ^ " with " ^ replacement ^ " (you'll need to name the record argument)" in
                  let warn_loc = Warn.warn_loc_of_loc location.file arg.ppat_loc in
                  st := Hint.mk_hint warn_loc source fix violation :: !st)
              bindings
        | _ -> ()
    in

    match pattern with
    | Pexp_let (_, vbs, body) -> List.iter (fun vb -> process_let_binding vb body) vbs
    | Pexp_fun (_, _, arg, body) -> process_fun_arg arg body
    | _ -> ()
end
