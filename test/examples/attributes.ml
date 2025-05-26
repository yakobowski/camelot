(* Global deactivation*)
[@@@camelot.warning "-LitPrepend"]

(* No warning here *)
let f t = ["n1"] @ t

(* Global re-activation*)
[@@@camelot.warning "+LitPrepend"]

(* Warning here *)
let f t = ["w2"] @ t

(* Local attribute. No warning *)
let f t = (["n3"] @ t)[@camelot.warning "-LitPrepend"]

(* Local attributes do not propagate to expressions underneath. Warning here *)
let f t = ("" :: (["w4"] @ t))[@camelot.warning "-LitPrepend"]

(* Local attributes have no global effect, Warning here *)
let f t = ["w5"] @ t

(* Scoping *)
[@@@camelot.warning "-LitPrepend"]

module M = struct
  (* Inherited from enclosing scope, no warning here*)
  let f t = ["n6"] @ t

  [@@@camelot.warning "+LitPrepend"]

  (* Reactivated, warning *)
  let f t = ["w7"] @ t
end

(* Back to the state in the enclosing module, no warning *)
let f t = ["n8"] @ t