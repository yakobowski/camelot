let x = true
let y = false

let e = x

let beta = e

let f _x = ()


(* if e then true else false --> e *)
let t1 = if e then true else false

(* if e then false else true --> not e *)
let t2 = if e then false else true

(* if beta then beta else false -> beta *)
let t3 = if beta then beta else false

(* if not e then x else y -> if not e then y else x *)
let t4 = if not e then x else y

(* if not e then v -> should not be flagged *)
let t4' = if not e then f x

(* if x then true else y --> x || y *)
let t5 = if x then true else y

(* if x then y else false --> x && y *)
let t6 = if x then y else false

(* if x then false else y --> not x && y *)
let t7 = if x then false else y

(* if x then y else true *)
let t8 = if x then y else true

(* Should ding them twice - encouragement to fix *)
let double_points = if x then x else true

(* Slightly more complex examples *)

let t9 = if 3 > 0 then 3 > 0 else false

(* More complex examples *)

let rec exists (l : int list) (i : int) =
  match l with
  | [] -> false
  | h :: t -> if h = i then true else exists t i

let rec forall (p: 'a -> bool) (l : 'a list) =
  match l with
  | [] -> true
  | h :: t -> if p h then forall p t else false

let rec none (p: 'a -> bool) (l: 'a list) =
  match l with
  | [] -> true
  | h :: t -> if p h then false else none p t

let rec nonsense (p: 'a -> bool) (l : 'a list) =
  match l with
  | [] -> false
  | h :: t -> if p h then nonsense p t else true

(* Tests for IfEmptyThenElse *)

(* Should be flagged: if c then () else e *)
let f x = if x > 0 then () else print_endline "negative"

(* Should be flagged: complex condition, if c then () else e *)
let g y z = if y && z || x then () else print_endline "action"

(* Should NOT be flagged: then branch is not unit *)
let h a = if a then print_endline "action" else print_endline "false"

(* Should NOT be flagged: else branch is unit, not then branch *)
let i b = if b then print_endline "true" else ()

(* Should NOT be flagged: no else branch *)
let j c = if c then ()
