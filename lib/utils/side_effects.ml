open Parsetree
open Ast_iterator

let has_side_effects expr =
  let found = ref false in
  let iterator =
    {
      Ast_iterator.default_iterator with
      expr =
        (fun iterator expr ->
          match expr.pexp_desc with
          (* A function application can have side effects *)
          | Pexp_apply _ ->
              found := true
          (* Method call *)
          | Pexp_send _ ->
              found := true
          (* Object instantiation *)
          | Pexp_new _ ->
              found := true
          (* Assignments are side effects *)
          | Pexp_setfield _ ->
              found := true
          (* Setting an instance variable *)
          | Pexp_setinstvar _ ->
              found := true
          | _ -> Ast_iterator.default_iterator.expr iterator expr);
    }
  in
  iterator.expr iterator expr;
  !found
