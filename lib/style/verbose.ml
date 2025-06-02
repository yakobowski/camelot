open Canonical
open Utils
open Astutils
open Check

(** ----------------------- Checks rules: [_] \@ _ ---------------------------- *)
module LitPrepend : EXPRCHECKCALLBACK = struct
  (* List of nodes that should be ignored when checking for this rule.
     We want to avoid warnings on code such as `x @ [v] @ y`, which is nicely
     symmetric when dealing with e.g. binary trees. *)
  let ignore_nodes = ref []

  (* Find all expressions directly under a @, to mark them as being skipped. *)
  let rec list_concat_nodes (exp: Parsetree.expression) =
    list_concat_nodes_desc exp.pexp_desc
  and list_concat_nodes_desc desc =
    match desc with
    | Pexp_apply (f, [_; (_, e2)]) ->
      if f =~ "@" then
        e2.pexp_desc :: list_concat_nodes e2
      else []
    | _ -> []

  (* Mark nodes under @ as being skipped *)
  let pre_callback e =
    let to_skip = list_concat_nodes e in
    (* Push on the stack if some nodes must be skipped.
       Push ourselves as the first element of the pair to
       make the post callback easier to write. *)
    if to_skip <> [] then
      ignore_nodes := (e, to_skip) :: !ignore_nodes

  let post_callback e =
    match !ignore_nodes with
    | [] -> ()
    | (e', _) :: rest ->
      (* Pop the top of the stack if it has been pushed by us. *)
      if e == e' then
        ignore_nodes := rest

  let callback = { pre_callback; post_callback }        

  (* Should [e] be ignored while testing this rule? *)
  let should_be_ignored e =
    List.exists (fun (_, l) -> List.exists ((==) e) l) !ignore_nodes

  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using `::` instead"
  let violation = "using `@` to prepend an element to a list"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_apply (application, [(_, lop); _]) ->
        if application =~ "@" && is_singleton_list lop && not (should_be_ignored pattern) then
          let raw = IOUtils.code_at_loc location source in
          st := Hint.mk_hint location raw fix violation :: !st
      | _ -> ()
    end
  let name = "LitPrepend", check
end


(** ----------------------- Checks rules: fst/snd t -------------------------- *)
module TupleProj : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using a let pattern match statement instead"
  let violation = "using fst / snd to project values out of a tuple"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_apply (application, [_]) ->
        if application =~ "fst" || application =~ "snd" then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "TupleProj", check
end

(** ----------------------- Checks rules: Nesting if >= 3 levels ------------- *)
module NestedIf : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using let statements or helper methods / rethinking logic"
  let violation = "using nested if statements more than three layers deep"
  let check st (E {location; source; pattern} : ctxt) =
    let rec find_nesting (p: Parsetree.expression_desc) depth= 
      depth = 0 ||
      begin match p with 
        | Pexp_ifthenelse (_, bthen, Some belse) -> 
          if depth = 1 then true else 
            find_nesting ((skip_seq_let bthen).pexp_desc) (depth - 1) ||
            find_nesting ((skip_seq_let belse).pexp_desc) (depth - 1) 
        | _ -> false
      end 
    in 
    if find_nesting pattern 4 then st := Hint.mk_hint location source fix violation :: !st
  let name = "NestedIf", check
end

(** -------------------- Checks rules: Nesting match >= 3 levels ------------- *)
module NestedMatch : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using let statements or helper methods / rethinking logic"
  let violation = "using nested match statements three or more layers deep"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      (* Layer one *)
      | Pexp_match (_, cs) ->
        let matches_three = List.map (fun (c: Parsetree.case) -> c.pc_rhs |> skip_seq_let) cs |>
                            (* Grab the second layer *)
                            List.map (fun (e: Parsetree.expression) ->
                                match e.pexp_desc with
                                | Pexp_match (_, cs) ->
                                  List.map (fun (c: Parsetree.case) -> c.pc_rhs |> skip_seq_let) cs
                                | _ -> []
                              ) |> List.flatten |>
                            (* Grab the third layer *)
                            List.exists (fun (e: Parsetree.expression) ->
                                match e.pexp_desc with
                                | Pexp_match _ -> true
                                | _ -> false ) in
        if matches_three then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "NestedMatch", check
end

(** ------------ Checks rules: if _ then/else true | false  ------------------ *)
module IfReturnsLit : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "returning just the condition (+ some tweaks)"
  let violation = "using an if statement to return `true | false` literally"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_ifthenelse (_, bthen, Some belse) ->
        if (bthen =| "true" && belse =| "false") ||
           (bthen =| "false" && belse =| "true") then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "IfReturnsLit", check
end

(** ------------ Checks rules: if cond then cond | if cond then _ else cond -- *)
module IfCondThenCond : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "returning just the condition or simplifying further"
  let violation = "returning the condition of an if statement on success and a boolean literal otherwise"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_ifthenelse (cond, bthen, Some belse) ->
        if e_eq cond bthen && (belse =| "false" || belse =| "true") then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "IfCondThenCond", check
end


(** ------------ Checks rules: if not cond then x else y --------------------- *)
module IfNotCond : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "swapping the then and else branches of the if statement"
  let violation = "checking negation in the if condition"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_ifthenelse (cond, _, Some _) ->
        begin match cond.pexp_desc with
          | Pexp_apply (func, [_]) -> if func =~ "not" then
              st := Hint.mk_hint location source fix violation :: !st
          | _ -> ()
        end
      | _ -> ()
    end
  let name = "IfNotCond", check
end

(** ------------ Checks rules: if x then true else y ------------------------- *)
module IfToOr : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "rewriting using a boolean operator like `||`"
  let violation = "overly verbose if statement that can be simplified"

  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_ifthenelse (_cond, bthen, Some belse) ->
        if not (belse =| "true") &&
           not (belse =| "false") &&
           bthen =| "true" then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end

  let name = "IfToOr", check
end

(** ------------ Checks rules: if x then y else false ------------------------ *)
module IfToAnd : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "rewriting using a boolean operator like `&&`"
  let violation = "overly verbose if statement that can be simplified"

  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_ifthenelse (cond, bthen, Some belse) ->
        if not (bthen =| "true") &&
           not (bthen =| "false") &&
           belse =| "false" &&
           e_neq cond bthen
        then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "IfToAnd", check
end

(** ------------ Checks rules: if x then false else y ------------------------ *)
module IfToAndInv : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "rewriting using a boolean operator like `&&` and `not`"
  let violation = "overly verbose if statement that can be simplified"
  let check st (E {location;source;pattern} : ctxt) =
    begin match pattern with
      | Pexp_ifthenelse (_cond, bthen, Some belse) ->
        if not (belse =| "true") && not (belse =| "false")  && bthen =| "false" then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "IfToAndInv", check
end

(** ------------ Checks rules: if x then y else true ------------------------ *)
module IfToOrInv : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "rewriting using a boolean operator like `||` and `not`"
  let violation = "overly verbose if statement that can be simplified"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_ifthenelse (_cond, bthen, Some belse) ->
        if not (bthen =| "true") && not (bthen =| "false") && belse =| "true" then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "IfToOrInv", check
end

(** ------------ Checks rules: ... || true | true || ... | false || ... | ... || false -- *)
module RedundantOr : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "simplifying further"
  let violation = "Usage of the `||` is redundant"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_apply (appl, [(_, l);(_, r)]) ->
        if appl =~ "||" && (e_eq_any (smash_boolean_tree pattern) ||
                            l =| "true" ||
                            l =| "false" ||
                            r =| "true" ||
                            r =| "false") then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "RedundantOr", check
end

(** ------------ Checks rules: ... && true | true && ... | false && ... | ... && false -- *)
module RedundantAnd : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "simplifying further"
  let violation = "Usage of the `&&` is redundant"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_apply (appl, [(_, l);(_, r)]) ->
        if appl =~ "&&" && (e_eq_any (smash_boolean_tree pattern) ||
                            l =| "true" ||
                            l =| "false" ||
                            r =| "true" ||
                            r =| "false") then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "RedundantAnd", check
end

(** ------------ Checks rules: if c then () else e --------------------- *)
module IfEmptyThenElse : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "replace with `if not c then e`"
  let violation = "using `if c then () else e`"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_ifthenelse (cond, then_branch, Some else_branch) ->
        begin match then_branch.pexp_desc with
          | Pexp_construct ({ txt = Lident "()"; _ }, None) ->
            let _ = cond and _ = else_branch in
            st := Hint.mk_hint location source fix violation :: !st
          | _ -> ()
        end
      | _ -> ()
    end
  let name = "IfEmptyThenElse", check
end


(** ------------ Checks rules: "foo" ^ "bar" ^ "baz" ----------------------- *)
module SuccessiveStringConcat : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let violation = "successive string concatenations using the `^` operator"

  type concat_part =
    | Literal of string
    | Identifier of string
    | OtherExpression of Parsetree.expression_desc (* Store the whole expression *)

  let rec collect_concat_parts_desc (expr_desc : Parsetree.expression_desc) : concat_part list =
    match expr_desc with
    | Pexp_apply (op, [(_, lhs_expr); (_, rhs_expr)]) when op =~ "^" ->
        collect_concat_parts lhs_expr @ collect_concat_parts rhs_expr
    | Pexp_constant (Pconst_string (s, _, None)) -> [Literal s]
    | Pexp_ident { txt = Lident name; _ } -> [Identifier name]
    | _ -> [OtherExpression expr_desc] (* Store the expression itself *)
  and collect_concat_parts (expr : Parsetree.expression) : concat_part list =
    collect_concat_parts_desc expr.pexp_desc

  let check st (Pctxt.E {location; source; pattern}) = (* Explicitly qualify E with Pctxt *)
    match pattern with
    | Pexp_apply (op, [(_, lhs_expr); (_, rhs_expr)]) when op =~ "^" ->
        if is_concat_application lhs_expr.pexp_desc || is_concat_application rhs_expr.pexp_desc then
          let formats, args =
            List.fold_right (fun arg (format, args) ->
              match arg with
              | Literal s ->
                (* Escape '%' in literals for the format string *)
                let escaped_s = String.split_on_char '%' s |> String.concat "%%" in
                (escaped_s :: format, args)
              | Identifier name ->
                ("%s" :: format, name :: args)
              | OtherExpression desc ->
                let expr = Ast_helper.Exp.mk desc in
                let expr_str = Printf.sprintf "(%s)" (Pprintast.string_of_expression expr) in
                ("%s" :: format, expr_str :: args)
            ) (collect_concat_parts_desc pattern) ([], [])
          in
          let format_string = String.concat "" (formats) in
          let fix = Printf.sprintf "Use Printf.sprintf %S %s" format_string (String.concat " " args) in
          st := Hint.mk_hint location source fix violation :: !st
    | _ -> ()
  let name = "SuccessiveStringConcat", check
end
