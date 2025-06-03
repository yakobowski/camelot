(** Expression checks *)
let expr_checks : (module Check.EXPRCHECKCALLBACK) list =
  List.map Check.expr_check_with_callbacks
    [ (module Equality.EqList)
    ; (module Equality.EqOption)
    ; (module Equality.EqBool)
    ; (module Equality.EqPhysical)
    ; (module Match.MatchBool)
    ; (module Match.MatchInt)
    ; (module Match.MatchListVerbose)
    ; (module Match.MatchRecord)
    ; (module Match.MatchTuple)
    ; (module Verbose.TupleProj)
    ; (module Verbose.IfReturnsLit)
    ; (module Verbose.IfCondThenCond)
    ; (module Verbose.IfNotCond)
    ; (module Verbose.IfToOr)
    ; (module Verbose.IfToAnd)
    ; (module Verbose.IfToAndInv)
    ; (module Verbose.IfToOrInv)
    ; (module Verbose.NestedIf)
    ; (module Verbose.NestedMatch)
    ; (module Verbose.RedundantOr)
    ; (module Verbose.RedundantAnd)
    ; (module Verbose.IfEmptyThenElse)
    ] @
  (* Rules with automatic handling of ignored nodes *)
  List.map Check_ignore.ignore_expr_check_with_callbacks
    [ (module Verbose.LitPrepend)
    ; (module Verbose.SuccessiveStringConcat)
    ]

(** Top-level structure checks *)
let struct_checks : (module Check.STRUCTURECHECK) list  = [
  (module Hof.UseMap)
; (module Hof.UseFold)
; (module Hof.UseIter)
; (module Record.Record)
]

(** Lexical checks *)
let lexical_checks : (module Check.LEXICALCHECK) list = [
  (module Lexical.LineLength)
]
