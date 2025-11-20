open Parsetree
open Ast_iterator
module SSet = Set.Make(String)

let get_bound_vars_from_pattern pat =
  let vars = ref SSet.empty in
  let iterator =
    {
      default_iterator with
      pat =
        (fun iterator p ->
          match p.ppat_desc with
          | Ppat_var { txt; _ } -> vars := SSet.add txt !vars
          | Ppat_alias (sub_pat, { txt; _ }) ->
              vars := SSet.add txt !vars;
              iterator.pat iterator sub_pat
          | _ -> default_iterator.pat iterator p);
      expr = (fun _ _ -> ());
    }
  in
  iterator.pat iterator pat;
  !vars

let free_variables_expr expr =
  let free_vars = ref SSet.empty in
  let initial_bound_vars = SSet.of_list ["+"; "-"; "*"; "/"; "="; "<>"; "<"; ">"; "<="; ">="; "&&"; "||"; "not"] in
  let bound_vars = ref initial_bound_vars in
  let iterator =
    {
      default_iterator with
      expr =
        (fun iterator expr ->
          match expr.pexp_desc with
          | Pexp_ident { txt = Lident x; _ } ->
              if not (SSet.mem x !bound_vars) then
                free_vars := SSet.add x !free_vars
          | Pexp_fun (_, _, pat, body) ->
              let original_bound = !bound_vars in
              let bindings = get_bound_vars_from_pattern pat in
              bound_vars := SSet.union bindings !bound_vars;
              iterator.expr iterator body;
              bound_vars := original_bound
          | Pexp_let (rec_flag, vbs, body) ->
              let bindings = List.fold_left (fun acc vb -> SSet.union (get_bound_vars_from_pattern vb.pvb_pat) acc) SSet.empty vbs in
              let original_bound = !bound_vars in

              let rhs_bound_vars = if rec_flag = Recursive then SSet.union bindings original_bound else original_bound in
              bound_vars := rhs_bound_vars;
              List.iter (fun vb -> iterator.expr iterator vb.pvb_expr) vbs;

              let body_bound_vars = SSet.union bindings original_bound in
              bound_vars := body_bound_vars;
              iterator.expr iterator body;

              bound_vars := original_bound
          | _ -> default_iterator.expr iterator expr);
      case = (fun iterator case ->
        let original_bound = !bound_vars in
        let bindings = get_bound_vars_from_pattern case.pc_lhs in
        bound_vars := SSet.union bindings !bound_vars;
        Option.iter (iterator.expr iterator) case.pc_guard;
        iterator.expr iterator case.pc_rhs;
        bound_vars := original_bound
      )
    }
  in
  iterator.expr iterator expr;
  SSet.elements !free_vars
