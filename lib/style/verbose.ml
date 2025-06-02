open Canonical
open Canonical.Pctxt
open Utils
open Astutils
open Check

(** ----------------------- Checks rules: [_] \@ _ ---------------------------- *)
module LitPrepend : Check_ignore.EXPRCHECKIGNORE = struct

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

  let children_to_ignore e = list_concat_nodes_desc e

  type t = Parsetree.expression_desc

  let fix = "using `::` instead"
  let violation = "using `@` to prepend an element to a list"
  let check st (E {location; source; pattern}) =
    begin match pattern with
      | Pexp_apply (application, [(_, lop); _]) ->
        if application =~ "@" && is_singleton_list lop then
          let raw = IOUtils.code_at_loc location source in
          st := Hint.mk_hint location raw fix violation :: !st
      | _ -> ()
    end
  let name = "LitPrepend"
end


(** ----------------------- Checks rules: fst/snd t -------------------------- *)
module TupleProj : EXPRCHECK = struct
  type t = Parsetree.expression_desc
  let fix = "using a let pattern match statement instead"
  let violation = "using fst / snd to project values out of a tuple"
  let check st (E {location; source; pattern}) =
    begin match pattern with
      | Pexp_apply (application, [_]) ->
        if application =~ "fst" || application =~ "snd" then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "TupleProj"
end

(** ----------------------- Checks rules: Nesting if >= 3 levels ------------- *)
module NestedIf : EXPRCHECK = struct
  type t = Parsetree.expression_desc
  let fix = "using let statements or helper methods / rethinking logic"
  let violation = "using nested if statements more than three layers deep"
  let check st (E {location; source; pattern}) =
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
  let name = "NestedIf"
end

(** -------------------- Checks rules: Nesting match >= 3 levels ------------- *)
module NestedMatch : EXPRCHECK = struct
  type t = Parsetree.expression_desc
  let fix = "using let statements or helper methods / rethinking logic"
  let violation = "using nested match statements three or more layers deep"
  let check st (E {location; source; pattern}) =
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
  let name = "NestedMatch"
end

(** ------------ Checks rules: if _ then/else true | false  ------------------ *)
module IfReturnsLit : EXPRCHECK = struct
  type t = Parsetree.expression_desc
  let fix = "returning just the condition (+ some tweaks)"
  let violation = "using an if statement to return `true | false` literally"
  let check st (E {location; source; pattern}) =
    begin match pattern with
      | Pexp_ifthenelse (_, bthen, Some belse) ->
        if (bthen =| "true" && belse =| "false") ||
           (bthen =| "false" && belse =| "true") then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "IfReturnsLit"
end

(** ------------ Checks rules: if cond then cond | if cond then _ else cond -- *)
module IfCondThenCond : EXPRCHECK = struct
  type t = Parsetree.expression_desc
  let fix = "returning just the condition or simplifying further"
  let violation = "returning the condition of an if statement on success and a boolean literal otherwise"
  let check st (E {location; source; pattern}) =
    begin match pattern with
      | Pexp_ifthenelse (cond, bthen, Some belse) ->
        if e_eq cond bthen && (belse =| "false" || belse =| "true") then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "IfCondThenCond"
end


(** ------------ Checks rules: if not cond then x else y --------------------- *)
module IfNotCond : EXPRCHECK = struct
  type t = Parsetree.expression_desc
  let fix = "swapping the then and else branches of the if statement"
  let violation = "checking negation in the if condition"
  let check st (E {location; source; pattern}) =
    begin match pattern with
      | Pexp_ifthenelse (cond, _, Some _) ->
        begin match cond.pexp_desc with
          | Pexp_apply (func, [_]) -> if func =~ "not" then
              st := Hint.mk_hint location source fix violation :: !st
          | _ -> ()
        end
      | _ -> ()
    end
  let name = "IfNotCond"
end

(** ------------ Checks rules: if x then true else y ------------------------- *)
module IfToOr : EXPRCHECK = struct
  type t = Parsetree.expression_desc
  let fix = "rewriting using a boolean operator like `||`"
  let violation = "overly verbose if statement that can be simplified"

  let check st (E {location; source; pattern}) =
    begin match pattern with
      | Pexp_ifthenelse (_cond, bthen, Some belse) ->
        if not (belse =| "true") &&
           not (belse =| "false") &&
           bthen =| "true" then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end

  let name = "IfToOr"
end

(** ------------ Checks rules: if x then y else false ------------------------ *)
module IfToAnd : EXPRCHECK = struct
  type t = Parsetree.expression_desc
  let fix = "rewriting using a boolean operator like `&&`"
  let violation = "overly verbose if statement that can be simplified"

  let check st (E {location; source; pattern}) =
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
  let name = "IfToAnd"
end

(** ------------ Checks rules: if x then false else y ------------------------ *)
module IfToAndInv : EXPRCHECK = struct
  type t = Parsetree.expression_desc
  let fix = "rewriting using a boolean operator like `&&` and `not`"
  let violation = "overly verbose if statement that can be simplified"
  let check st (E {location;source;pattern}) =
    begin match pattern with
      | Pexp_ifthenelse (_cond, bthen, Some belse) ->
        if not (belse =| "true") && not (belse =| "false")  && bthen =| "false" then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "IfToAndInv"
end

(** ------------ Checks rules: if x then y else true ------------------------ *)
module IfToOrInv : EXPRCHECK = struct
  type t = Parsetree.expression_desc
  let fix = "rewriting using a boolean operator like `||` and `not`"
  let violation = "overly verbose if statement that can be simplified"
  let check st (E {location; source; pattern}) =
    begin match pattern with
      | Pexp_ifthenelse (_cond, bthen, Some belse) ->
        if not (bthen =| "true") && not (bthen =| "false") && belse =| "true" then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "IfToOrInv"
end

(** ------------ Checks rules: ... || true | true || ... | false || ... | ... || false -- *)
module RedundantOr : EXPRCHECK = struct
  type t = Parsetree.expression_desc
  let fix = "simplifying further"
  let violation = "Usage of the `||` is redundant"
  let check st (E {location; source; pattern}) =
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
  let name = "RedundantOr"
end

(** ------------ Checks rules: ... && true | true && ... | false && ... | ... && false -- *)
module RedundantAnd : EXPRCHECK = struct
  type t = Parsetree.expression_desc
  let fix = "simplifying further"
  let violation = "Usage of the `&&` is redundant"
  let check st (E {location; source; pattern}) =
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
  let name = "RedundantAnd"
end

(** ------------ Checks rules: if c then () else e --------------------- *)
module IfEmptyThenElse : EXPRCHECK = struct
  type t = Parsetree.expression_desc
  let fix = "replace with `if not c then e`"
  let violation = "using `if c then () else e`"
  let check st (E {location; source; pattern}) =
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
  let name = "IfEmptyThenElse"
end


(** ------------ Checks rules: "foo" ^ "bar" ^ "baz" ----------------------- *)
module SuccessiveStringConcat : Check_ignore.EXPRCHECKIGNORE = struct
  type t = Parsetree.expression_desc
  let violation = "successive string concatenations using the `^` operator"

  (* Find all string concatenations directly under a ^, to mark them as being skipped.
     The root expression can optionally be skipped, which is useful for ignoring the
     top-level concatenation *)
  let rec string_concat_nodes ?skip_root (exp: Parsetree.expression) =
    string_concat_nodes_desc ?skip_root exp.pexp_desc
  and string_concat_nodes_desc ?(skip_root=false) desc =
    match desc with
    | Pexp_apply (f, [(_, e1); (_, e2)]) ->
      if f =~ "^" then
        (if skip_root then [] else [desc]) @ string_concat_nodes e1 @ string_concat_nodes e2
      else
        []
    | _ -> []

  let children_to_ignore e = string_concat_nodes_desc ~skip_root:true e

  type concat_part =
    | Literal of string
    | IntToStringArg of Parsetree.expression
    | OtherExpression of Parsetree.expression_desc

  let rec collect_concat_parts_desc (expr_desc : Parsetree.expression_desc) : concat_part list =
    match expr_desc with
    | Pexp_apply (op, [(_, lhs_expr); (_, rhs_expr)]) when op =~ "^" ->
        collect_concat_parts lhs_expr @ collect_concat_parts rhs_expr
    | Pexp_constant (Pconst_string (s, _, None)) -> [Literal s]
    | Pexp_apply (func_expr, [(Asttypes.Nolabel, arg_expr)]) -> (
        match func_expr.pexp_desc with
        | Pexp_ident {txt = Ldot (Lident "Int", "to_string"); _} -> [IntToStringArg arg_expr]
        | Pexp_ident {txt = Lident "string_of_int"; _} -> [IntToStringArg arg_expr]
        | _ -> [OtherExpression expr_desc] (* Store the expression itself *)
      )
    | _ -> [OtherExpression expr_desc] (* Store the expression itself *)
  and collect_concat_parts (expr : Parsetree.expression) : concat_part list =
    collect_concat_parts_desc expr.pexp_desc

  let build_fix_string format_string args =
    (* exp for Printf.sprintf *)
    let printf = Location.mknoloc (Option.get (Longident.unflatten ["Printf"; "sprintf"])) in
    (* all the arguments for sprintf *)
    let args = Ast_helper.(Exp.constant (Const.string format_string)) :: args in
    (* sprintf applied *)
    let app = Ast_helper.Exp.apply (Ast_helper.Exp.ident printf) (List.map (fun arg -> (Asttypes.Nolabel, arg)) args) in
    (* convert to string *)
    Printf.sprintf "Use %s" (Pprintast.string_of_expression app)

  let check st (Pctxt.E {location; source; pattern}) = (* Explicitly qualify E with Pctxt *)
    match pattern with
    | Pexp_apply (op, [(_, lhs_expr); (_, rhs_expr)]) when op =~ "^" ->
        if is_concat_application lhs_expr.pexp_desc || is_concat_application rhs_expr.pexp_desc then
          let concat = collect_concat_parts_desc pattern in
          if List.length concat > 3 then
            let format, args =
              List.fold_right (fun arg (format, args) ->
                match arg with
                | Literal s ->
                  (* Escape '%' in literals for the format string *)
                  let escaped_s = String.split_on_char '%' s |> String.concat "%%" in
                  (escaped_s ^ format, args)
                | IntToStringArg e ->
                  ("%d" ^ format, e :: args)
                | OtherExpression desc ->
                  let expr = Ast_helper.Exp.mk desc in
                  ("%s" ^ format, expr :: args)
              ) concat ("", [])
            in
            let fix = build_fix_string format args in
            st := Hint.mk_hint location source fix violation :: !st
    | _ -> ()
  let name = "SuccessiveStringConcat"
end
