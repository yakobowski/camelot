
(* Prepending with list construction and append, where cons should be used *)
let f t = [1] @ t

(* Likewise, but even more verbose *)
let f t = 1 :: [] @ t

(* No warning here *)
let f x y = x @ [1] @ y


(* Tuple projection bad! *)

let f (t: int * int) = fst t + snd t

(* Nested ifs :( - we skip local lets and sequencing to get the actual return type for now *)
let x = true
let y = false

let dounit () = ()
(* No Flagging here - only two levels deep *)
let f () = if x then (if x then x else y) else y
let f () = if x then
    (if x then x
     else let z = 3 in
       dounit();
       (if z = 2 then x else y) )
  else y


(* No Flagging here - only 2 match levels deep *)
let f () =
  let l = [] in
  begin match l with
    | [] ->
      begin match l with
        | [] -> true
        | _ -> false
      end
    | _ -> true
  end

(* Nested matched bad as well *)

let f () =
  let l = [] in
  begin match l with
    | [] ->
      begin match l with
        | [] ->
          let z = [] in
        begin match z with
          | _ -> true
        end
      | _ -> false
    end
  | _ -> true
  end


(* If statement three layers deep *)
let z = if x then 1 else if y then 2 else if x & y then 3 else 4

(* If statement four layers deep *)
let z = if x then 1 else if y then 2 else if x & y then 3 else if z = 4 then 3 else 9

(* shouldn't trigger *)
let z = (x = TConstr 3 || x = TConstr 4)
let z = (x = [12] || x = [50])
let z = (x = 5 || x = 6)
let z = (x = TConstr 3 && x = TConstr 4)
let z = (x = [12] && x = [50])
let z = (x = 5 && x = 6)

(* TODO: perhaps this one should trigger and say just change to true *)
let z = (x = TConstr 3 || not (x = TConstr 3))

(* should trigger *)
let z = (x = [] || x = [])
let z = (x = None || x = None)
let z = (x = 5 || x = 5)
let z = (x = TConstr 3 || x = TConstr 3)
let z = (x = TConstr 3 || x = TConstr 3 || x = TConstr 4)
let z = (x = TConstr 3 || x = TConstr 4 || x = TConstr 3)
let z = (x = [] && x = [])
let z = (x = None && x = None)
let z = (x = 5 && x = 5)
let z = (x = TConstr 3 && x = TConstr 3)
let z = (x = TConstr 3 && x = TConstr 3 && x = TConstr 4)
let z = (x = TConstr 3 && x = TConstr 4 && x = TConstr 3)


(* ----------------------- SuccessiveStringConcat -------------------------- *)
(* New logic: (LHS^RHS) flagged if is_concat_application(LHS/RHS) *)

(* Test: ("a" ^ "b") ^ "c" *)
(* Expect: 1 warn on ("a"^"b")^"c". No warn on "a"^"b". *)
let _ = ("a" ^ "b") ^ "c"

(* Test: "a" ^ ("b" ^ "c") *)
(* Expect: 1 warn on "a"^("b"^"c"). No warn on "b"^"c". *)
let _ = "a" ^ ("b" ^ "c")

(* Test: "a" ^ "b" ^ "c" ^ "d" (parsed ("a"^"b")^"c")^"d" *)
(* Expect:
   - Warn for (("a"^"b")^"c")^"d"
   - Warn for ("a"^"b")^"c"
   - No warn for "a"^"b"
   (2 warnings total) *)
let _ = "a" ^ "b" ^ "c" ^ "d"

(* Test: "a" ^ ("b" ^ ("c" ^ "d")) (explicit right-assoc) *)
(* Expect:
   - Warn for "a" ^ ("b"^("c"^"d"))
   - Warn for "b" ^ ("c"^"d")
   - No warn for "c"^"d"
   (2 warnings total) *)
let _ = "a" ^ ("b" ^ ("c" ^ "d"))


let s1 = "hello"
let s2 = " "
let s3 = "world"
let s4 = "!"

(* Test with identifiers: (s1 ^ s2) ^ s3 *)
(* Expect: 1 warn on (s1^s2)^s3. No warn on s1^s2. *)
let _ = (s1 ^ s2) ^ s3

(* Test with identifiers: s1 ^ (s2 ^ s3) *)
(* Expect: 1 warn on s1^(s2^s3). No warn on s2^s3. *)
let _ = s1 ^ (s2 ^ s3)

(* Test with identifiers: s1 ^ s2 ^ s3 ^ s4 (parsed (s1^s2)^s3)^s4) *)
(* Expect:
   - Warn for ((s1^s2)^s3)^s4
   - Warn for (s1^s2)^s3
   - No warn for s1^s2
   (2 warnings total) *)
let _ = s1 ^ s2 ^ s3 ^ s4

(* Complex case testing the suggested fix *)
let _ = "foo%d " ^ s1 ^ " bar " ^ (f v2) ^ " baz " ^ (g (h v3)) ^ " qux"


(* --- Cases that should NOT trigger the rule --- *)

(* Simple concatenation: "a" ^ "b" *)
(* Expect: No warning *)
let _ = "a" ^ "b"

(* Not string concatenation *)
let _ = 1 + 2 + 3

(* Function call, not an operator *)
let my_concat x y = x ^ y
let _ = my_concat "a" "b"

(* Single operand (edge case for the checker, should not be flagged) *)
let _ = "a"
