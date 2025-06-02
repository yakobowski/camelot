
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

(* Expect: 1 warn on the entire expression. No warning on inner expressions *)
let _ = ((("a" ^ "b") ^ "c") ^ "d") ^ "e"
let _ = "a" ^ "b" ^ "c" ^ "d" ^ "e"
let _ = "a" ^ ("b" ^ ("c" ^ ("d" ^ "e")))
let _ = ("a" ^ "b") ^ ("c" ^ ("d" ^ "e"))


let s1 = "hello"
let s2 = " "
let s3 = "world"
let s4 = "!"
let s5 = "!!"

(* Expect: 1 warn on the entire expression. No warning on inner expressions *)
let _ = ((s1 ^ s2) ^ s3 ^ s4) ^ s5
let _ = s1 ^ (s2 ^ s3 ^ s4) ^ s5
let _ = s1 ^ s2 ^ s3 ^ s4 ^ s5
let _ = s1 ^ (s2 ^ (s3 ^ (s4 ^ s5)))


let f x = x
let h x = x
let g x = x
(* Complex case testing the suggested fix message *)
let _ = "foo%d " ^ s1 ^ " bar " ^ (f s2) ^ " baz " ^ (g (h s3)) ^ " qux"

(* --- New test cases for Int.to_string and string_of_int --- *)
let _ = let count = 10 in "count: " ^ Int.to_string count ^ " items" ^ " total"
let _ = let i = 5 in "value: " ^ string_of_int i ^ " units" ^ " available"
let _ = let num = 42 in let str = "example" in "prefix: " ^ str ^ " int: " ^ Int.to_string num ^ " call: " ^ (List.hd ["val:" ^ string_of_int 123]) ^ " suffix"
let _ = let x = 10 in "Value is " ^ (if x > 5 then "val:" ^ string_of_int x else "small") ^ " status" ^ " end"
let _ = let k = 20 in "Result: " ^ Int.to_string (k * 2) ^ " units" ^ " calculated"

(* --- Cases that should NOT trigger the rule --- *)
(* No warning (too small) *)
let _ = s1 ^ (s2 ^ s3)
let _ = s1 ^ s2 ^ s3
let _ = (s1 ^ s2) ^ s3

let _ = ("a" ^ "b") ^ "c"
let _ = "a" ^ "b" ^ "c"
let _ = "a" ^ ("b" ^ "c")


(* Not string concatenation *)
let _ = 1 + 2 + 3 + 4 + 5


(* Function call, not an operator *)
let my_concat x y = x ^ y
let _ = my_concat (my_concat (my_concat "a" "b") (my_concat "c" "d")) "e"
