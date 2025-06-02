
let display_verbose = Report.Display.student_display

(* Unfortunately, I have to copy most of the library code from camelot.ml *)

let fail msg = prerr_endline msg; exit 1

let safe_open src =
  try src, open_in src
  with Sys_error msg -> fail msg

let to_ast file =
  let src, f = safe_open file in
  src, ( f |> Lexing.from_channel |> Parse.implementation )

let line_lint : bool ref = ref false 

let lint_and_hint : (string * Parsetree.structure) -> unit = fun (file, ast) ->
  let store : Canonical.Hint.hint list ref = ref [] in
  let line_length_lint : string -> unit = fun file ->
    if not !line_lint then ()
    else
      let chan = open_in file in
      let lref : int ref = ref 1 in
      try
        while true; do
          let line = input_line chan in
          (if (String.length line > 80) then store := Canonical.Hint.line_hint file !lref line :: !store;);
          incr lref
        done; ()
      with End_of_file ->
        close_in chan; () in
  line_length_lint file;
  file |>
  Traverse.Iter.make_linterator store |>
  Traverse.Iter.apply_iterator ast;
  display_verbose !store


(* Run the tests in lexical.ml *)
let%expect_test _ =
  line_lint := true;
  let file : string = "./examples/lexical.ml" in
  let to_lint = to_ast file in
  lint_and_hint to_lint;
  line_lint := false;
  [%expect{|
    (* ------------------------------------------------------------------------ *)
    File ./examples/lexical.ml, line 5, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let verylongvariablenamethisispainful = [1;2;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1]
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/lexical.ml, line 2, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let verylongvariablenamethisispainful = [1;2;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1]
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/lexical.ml, line 5, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let verylongvariablenamethisispainful = [1;2;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1]
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/lexical.ml, line 2, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let verylongvariablenamethisispainful = [1;2;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1]
    Consider:
    	indenting to avoid exceeding the 80 character line limit |}]

(* Run the tests in equality.ml *)
let%expect_test _ =
  let file : string = "./examples/equality.ml" in
  let to_lint = to_ast file in
  lint_and_hint to_lint;
  [%expect{|
    (* ------------------------------------------------------------------------ *)
    File ./examples/equality.ml, line 27, columns: 8-26
    Warning:
    	using `==` when structural equality is intended
    You wrote:
    	 "ocaml" == "ocaml"
    Consider:
    	using `=` to evaluate structural equality

    (* ------------------------------------------------------------------------ *)
    File ./examples/equality.ml, line 26, columns: 8-14
    Warning:
    	using `==` when structural equality is intended
    You wrote:
    	 1 == 1
    Consider:
    	using `=` to evaluate structural equality

    (* ------------------------------------------------------------------------ *)
    File ./examples/equality.ml, line 23, columns: 9-23
    Warning:
    	using `=` with a boolean literal
    You wrote:
    	 false = bfalse
    Consider:
    	using the variable itself to represent the value

    (* ------------------------------------------------------------------------ *)
    File ./examples/equality.ml, line 22, columns: 9-21
    Warning:
    	using `=` with a boolean literal
    You wrote:
    	 true = btrue
    Consider:
    	using the variable itself to represent the value

    (* ------------------------------------------------------------------------ *)
    File ./examples/equality.ml, line 21, columns: 9-23
    Warning:
    	using `=` with a boolean literal
    You wrote:
    	 bfalse = false
    Consider:
    	using the variable itself to represent the value

    (* ------------------------------------------------------------------------ *)
    File ./examples/equality.ml, line 20, columns: 9-21
    Warning:
    	using `=` with a boolean literal
    You wrote:
    	 btrue = true
    Consider:
    	using the variable itself to represent the value

    (* ------------------------------------------------------------------------ *)
    File ./examples/equality.ml, line 15, columns: 8-41
    Warning:
    	using `=` with lists as a condition in an if statement
    You wrote:
    	 if [1; 2; 3] = q then None else x
    Consider:
    	using a pattern match to check whether a list has a certain value

    (* ------------------------------------------------------------------------ *)
    File ./examples/equality.ml, line 14, columns: 8-37
    Warning:
    	using `=` with lists as a condition in an if statement
    You wrote:
    	 if q = [1] then x else None
    Consider:
    	using a pattern match to check whether a list has a certain value

    (* ------------------------------------------------------------------------ *)
    File ./examples/equality.ml, line 5, columns: 8-23
    Warning:
    	using `=` with options
    You wrote:
    	 (Some 1) = (Some 1)
    Consider:
    	using a pattern match to check the presence of an option
  |}]

(* Run the tests in verbose.ml *)
let%expect_test _ =
  let file : string = "./examples/verbose.ml" in
  let to_lint = to_ast file in
  lint_and_hint to_lint;
  [%expect{|
    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 127, columns: 22-83
    Warning:
    	successive string concatenations using the `^` operator
    You wrote:
    	 "Result: " ^ ((Int.to_string (k * 2)) ^ (" units" ^ " calculated"))
    Consider:
    	Use Printf.sprintf "Result: %d units calculated" (k * 2)

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 126, columns: 22-110
    Warning:
    	successive string concatenations using the `^` operator
    You wrote:
    	 "Value is " ^
      ((if x > 5 then "val:" ^ (string_of_int x) else "small") ^
         (" status" ^ " end"))
    Consider:
    	Use Printf.sprintf "Value is %s status end"
      (if x > 5 then "val:" ^ (string_of_int x) else "small")

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 125, columns: 47-159
    Warning:
    	successive string concatenations using the `^` operator
    You wrote:
    	 "prefix: " ^
      (str ^
         (" int: " ^
            ((Int.to_string num) ^
               (" call: " ^
                  ((List.hd ["val:" ^ (string_of_int 123)]) ^ " suffix")))))
    Consider:
    	Use Printf.sprintf "prefix: %s int: %d call: %s suffix" str num
      (List.hd ["val:" ^ (string_of_int 123)])

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 124, columns: 21-74
    Warning:
    	successive string concatenations using the `^` operator
    You wrote:
    	 "value: " ^ ((string_of_int i) ^ (" units" ^ " available"))
    Consider:
    	Use Printf.sprintf "value: %d units available" i

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 123, columns: 26-79
    Warning:
    	successive string concatenations using the `^` operator
    You wrote:
    	 "count: " ^ ((Int.to_string count) ^ (" items" ^ " total"))
    Consider:
    	Use Printf.sprintf "count: %d items total" count

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 120, columns: 8-72
    Warning:
    	successive string concatenations using the `^` operator
    You wrote:
    	 "foo%d " ^ (s1 ^ (" bar " ^ ((f s2) ^ (" baz " ^ ((g (h s3)) ^ " qux")))))
    Consider:
    	Use Printf.sprintf "foo%%d %s bar %s baz %s qux" s1 (f s2) (g (h s3))

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 113, columns: 8-36
    Warning:
    	successive string concatenations using the `^` operator
    You wrote:
    	 s1 ^ (s2 ^ (s3 ^ (s4 ^ s5)))
    Consider:
    	Use Printf.sprintf "%s%s%s%s%s" s1 s2 s3 s4 s5

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 112, columns: 8-30
    Warning:
    	successive string concatenations using the `^` operator
    You wrote:
    	 s1 ^ (s2 ^ (s3 ^ (s4 ^ s5)))
    Consider:
    	Use Printf.sprintf "%s%s%s%s%s" s1 s2 s3 s4 s5

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 111, columns: 8-32
    Warning:
    	successive string concatenations using the `^` operator
    You wrote:
    	 s1 ^ ((s2 ^ (s3 ^ s4)) ^ s5)
    Consider:
    	Use Printf.sprintf "%s%s%s%s%s" s1 s2 s3 s4 s5

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 110, columns: 8-34
    Warning:
    	successive string concatenations using the `^` operator
    You wrote:
    	 ((s1 ^ s2) ^ (s3 ^ s4)) ^ s5
    Consider:
    	Use Printf.sprintf "%s%s%s%s%s" s1 s2 s3 s4 s5

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 100, columns: 8-41
    Warning:
    	successive string concatenations using the `^` operator
    You wrote:
    	 ("a" ^ "b") ^ ("c" ^ ("d" ^ "e"))
    Consider:
    	Use Printf.sprintf "abcde"

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 99, columns: 8-41
    Warning:
    	successive string concatenations using the `^` operator
    You wrote:
    	 "a" ^ ("b" ^ ("c" ^ ("d" ^ "e")))
    Consider:
    	Use Printf.sprintf "abcde"

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 98, columns: 8-35
    Warning:
    	successive string concatenations using the `^` operator
    You wrote:
    	 "a" ^ ("b" ^ ("c" ^ ("d" ^ "e")))
    Consider:
    	Use Printf.sprintf "abcde"

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 97, columns: 8-41
    Warning:
    	successive string concatenations using the `^` operator
    You wrote:
    	 ((("a" ^ "b") ^ "c") ^ "d") ^ "e"
    Consider:
    	Use Printf.sprintf "abcde"

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 90, columns: 8-57
    Warning:
    	Usage of the `&&` is redundant
    You wrote:
    	 (x = (TConstr 3)) && ((x = (TConstr 4)) && (x = (TConstr 3)))
    Consider:
    	simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 89, columns: 8-57
    Warning:
    	Usage of the `&&` is redundant
    You wrote:
    	 (x = (TConstr 3)) && ((x = (TConstr 3)) && (x = (TConstr 4)))
    Consider:
    	simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 88, columns: 8-40
    Warning:
    	Usage of the `&&` is redundant
    You wrote:
    	 (x = (TConstr 3)) && (x = (TConstr 3))
    Consider:
    	simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 87, columns: 8-24
    Warning:
    	Usage of the `&&` is redundant
    You wrote:
    	 (x = 5) && (x = 5)
    Consider:
    	simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 86, columns: 8-30
    Warning:
    	Usage of the `&&` is redundant
    You wrote:
    	 (x = None) && (x = None)
    Consider:
    	simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 85, columns: 8-26
    Warning:
    	Usage of the `&&` is redundant
    You wrote:
    	 (x = []) && (x = [])
    Consider:
    	simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 84, columns: 8-57
    Warning:
    	Usage of the `||` is redundant
    You wrote:
    	 (x = (TConstr 3)) || ((x = (TConstr 4)) || (x = (TConstr 3)))
    Consider:
    	simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 83, columns: 8-57
    Warning:
    	Usage of the `||` is redundant
    You wrote:
    	 (x = (TConstr 3)) || ((x = (TConstr 3)) || (x = (TConstr 4)))
    Consider:
    	simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 82, columns: 8-40
    Warning:
    	Usage of the `||` is redundant
    You wrote:
    	 (x = (TConstr 3)) || (x = (TConstr 3))
    Consider:
    	simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 81, columns: 8-24
    Warning:
    	Usage of the `||` is redundant
    You wrote:
    	 (x = 5) || (x = 5)
    Consider:
    	simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 80, columns: 8-30
    Warning:
    	Usage of the `||` is redundant
    You wrote:
    	 (x = None) || (x = None)
    Consider:
    	simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 79, columns: 8-26
    Warning:
    	Usage of the `||` is redundant
    You wrote:
    	 (x = []) || (x = [])
    Consider:
    	simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 65, columns: 8-85
    Warning:
    	using nested if statements more than three layers deep
    You wrote:
    	 if x then 1 else if y then 2 else if x & y then 3 else if z = 4 then 3 else 9
    Consider:
    	using let statements or helper methods / rethinking logic

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, lines 47-58, columns: 2-5
    Warning:
    	using nested match statements three or more layers deep
    You wrote:
    	 match l with
    | [] ->
        (match l with
         | [] -> let z = [] in (match z with | _ -> true)
         | _ -> false)
    | _ -> true
    Consider:
    	using let statements or helper methods / rethinking logic

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 14, columns: 31-36
    Warning:
    	using fst / snd to project values out of a tuple
    You wrote:
    	 snd t
    Consider:
    	using a let pattern match statement instead

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 14, columns: 23-28
    Warning:
    	using fst / snd to project values out of a tuple
    You wrote:
    	 fst t
    Consider:
    	using a let pattern match statement instead

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 6, columns: 10-21
    Warning:
    	using `@` to prepend an element to a list
    You wrote:
    	 1 :: [] @ t
    Consider:
    	using `::` instead

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 3, columns: 10-17
    Warning:
    	using `@` to prepend an element to a list
    You wrote:
    	 [1] @ t
    Consider:
    	using `::` instead

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 127, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let _ = let k = 20 in "Result: " ^ Int.to_string (k * 2) ^ " units" ^ " calculated"
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 126, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let _ = let x = 10 in "Value is " ^ (if x > 5 then "val:" ^ string_of_int x else "small") ^ " status" ^ " end"
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 125, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let _ = let num = 42 in let str = "example" in "prefix: " ^ str ^ " int: " ^ Int.to_string num ^ " call: " ^ (List.hd ["val:" ^ string_of_int 123]) ^ " suffix"
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 65, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let z = if x then 1 else if y then 2 else if x & y then 3 else if z = 4 then 3 else 9
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 16, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 (* Nested ifs :( - we skip local lets and sequencing to get the actual return type for now *)
    Consider:
    	indenting to avoid exceeding the 80 character line limit
  |}]

(* Run the tests in if.ml *)
let%expect_test _ =
  let file : string = "./examples/if.ml" in
  let to_lint = to_ast file in
  lint_and_hint to_lint;
  [%expect{|
    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 73, columns: 12-62
    Warning:
    	using `if c then () else e`
    You wrote:
    	 if (y && z) || x then () else print_endline "action"
    Consider:
    	replace with `if not c then e`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 70, columns: 10-56
    Warning:
    	using `if c then () else e`
    You wrote:
    	 if x > 0 then () else print_endline "negative"
    Consider:
    	replace with `if not c then e`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 65, columns: 14-48
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if p h then nonsense p t else true
    Consider:
    	rewriting using a boolean operator like `||` and `not`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 60, columns: 14-45
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if p h then false else none p t
    Consider:
    	rewriting using a boolean operator like `&&` and `not`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 55, columns: 14-47
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if p h then forall p t else false
    Consider:
    	rewriting using a boolean operator like `&&`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 50, columns: 14-48
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if h = i then true else exists t i
    Consider:
    	rewriting using a boolean operator like `||`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 43, columns: 9-39
    Warning:
    	returning the condition of an if statement on success and a boolean literal otherwise
    You wrote:
    	 if 3 > 0 then 3 > 0 else false
    Consider:
    	returning just the condition or simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 39, columns: 20-41
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if x then x else true
    Consider:
    	rewriting using a boolean operator like `||` and `not`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 39, columns: 20-41
    Warning:
    	returning the condition of an if statement on success and a boolean literal otherwise
    You wrote:
    	 if x then x else true
    Consider:
    	returning just the condition or simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 36, columns: 9-30
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if x then y else true
    Consider:
    	rewriting using a boolean operator like `||` and `not`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 33, columns: 9-31
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if x then false else y
    Consider:
    	rewriting using a boolean operator like `&&` and `not`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 30, columns: 9-31
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if x then y else false
    Consider:
    	rewriting using a boolean operator like `&&`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 27, columns: 9-30
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if x then true else y
    Consider:
    	rewriting using a boolean operator like `||`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 21, columns: 9-31
    Warning:
    	checking negation in the if condition
    You wrote:
    	 if not e then x else y
    Consider:
    	swapping the then and else branches of the if statement

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 18, columns: 9-37
    Warning:
    	returning the condition of an if statement on success and a boolean literal otherwise
    You wrote:
    	 if beta then beta else false
    Consider:
    	returning just the condition or simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 15, columns: 9-34
    Warning:
    	using an if statement to return `true | false` literally
    You wrote:
    	 if e then false else true
    Consider:
    	returning just the condition (+ some tweaks)

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 12, columns: 9-34
    Warning:
    	using an if statement to return `true | false` literally
    You wrote:
    	 if e then true else false
    Consider:
    	returning just the condition (+ some tweaks)
  |}]

(* Run the tests in match.ml *)
let%expect_test _ =
  let file : string = "./examples/match.ml" in
  let to_lint = to_ast file in
  lint_and_hint to_lint;
  [%expect{|
    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, line 112, columns: 13-20
    Warning:
    	using an overly complex match clause
    You wrote:
    	 | y :: [] -> ...
    Consider:
    	expressing this match case more compactly, such as: | [y] -> ...

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, line 112, columns: 4-11
    Warning:
    	using an overly complex match clause
    You wrote:
    	 | x :: [] -> ...
    Consider:
    	expressing this match case more compactly, such as: | [x] -> ...

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, line 106, columns: 7-14
    Warning:
    	using an overly complex match clause
    You wrote:
    	 | x :: [] -> ...
    Consider:
    	expressing this match case more compactly, such as: | [x] -> ...

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, line 100, columns: 4-11
    Warning:
    	using an overly complex match clause
    You wrote:
    	 | x :: [] -> ...
    Consider:
    	expressing this match case more compactly, such as: | [x] -> ...

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, lines 81-82, columns: 2-15
    Warning:
    	using pattern matching on a tuple (for fewer than 2 cases)
    You wrote:
    	 match r with | (x, y) -> ()
    Consider:
    	using a let statement to extract tuple fields

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, lines 69-71, columns: 2-16
    Warning:
    	using pattern matching on a record (for fewer than 3 cases)
    You wrote:
    	 match r with | { x; y } -> () | { x;_} -> ()
    Consider:
    	using a let statement to extract record fields

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, lines 65-66, columns: 2-16
    Warning:
    	using pattern matching on a record (for fewer than 3 cases)
    You wrote:
    	 match r with | { x; y } -> ()
    Consider:
    	using a let statement to extract record fields

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, line 48, columns: 4-13
    Warning:
    	using an overly complex match clause
    You wrote:
    	 | abc :: [] -> ...
    Consider:
    	expressing this match case more compactly, such as: | [abc] -> ...

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, line 47, columns: 4-11
    Warning:
    	using an overly complex match clause
    You wrote:
    	 | x :: [] -> ...
    Consider:
    	expressing this match case more compactly, such as: | [x] -> ...

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, line 42, columns: 4-11
    Warning:
    	using an overly complex match clause
    You wrote:
    	 | _ :: [] -> ...
    Consider:
    	expressing this match case more compactly, such as: | [_] -> ...

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, lines 12-15, columns: 15-5
    Warning:
    	using integer pattern matching on fewer than 3 cases
    You wrote:
    	 match b with | 2 -> true | 3 -> false
    Consider:
    	using an if statement and `=`

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, lines 4-7, columns: 15-5
    Warning:
    	using pattern matching on boolean literals
    You wrote:
    	 match b with | false -> true | true -> false
    Consider:
    	using an if statement or boolean operators
  |}]

(* Run the tests in hof.ml *)
let%expect_test _ =
  let file : string = "./examples/hof.ml" in
  let to_lint = to_ast file in
  lint_and_hint to_lint;
  [%expect{|
    (* ------------------------------------------------------------------------ *)
    File ./examples/hof.ml, lines 17-20, columns: 0-31
    Warning:
    	overly verbose function implementation
    You wrote:
    	 let rec sum_verbose (l : int list) =
      match l with | [] -> 0 | h::t -> h + (sum_verbose t)
    Consider:
    	using a higher order function like fold

    (* ------------------------------------------------------------------------ *)
    File ./examples/hof.ml, lines 11-15, columns: 0-13
    Warning:
    	overly verbose function implementation
    You wrote:
    	 let rec print_l (l : int list) =
      match l with
      | [] -> ()
      | h::t -> ((h |> string_of_int) |> print_endline; print_l t)
    Consider:
    	using a higher order function like iter

    (* ------------------------------------------------------------------------ *)
    File ./examples/hof.ml, lines 6-9, columns: 0-33
    Warning:
    	overly verbose function implementation
    You wrote:
    	 let rec plus_n n l =
      match l with | [] -> [] | h::t -> (h + n) :: (plus_n n t)
    Consider:
    	using a higher order function like transform

    (* ------------------------------------------------------------------------ *)
    File ./examples/hof.ml, lines 1-4, columns: 0-34
    Warning:
    	overly verbose function implementation
    You wrote:
    	 let rec plus_one l =
      match l with | [] -> [] | h::t -> (h + 1) :: (plus_one t)
    Consider:
    	using a higher order function like transform
  |}]

(* Run the tests in attributes.ml *)
let%expect_test _ =
  let file : string = "./examples/attributes.ml" in
  let to_lint = to_ast file in
  lint_and_hint to_lint;
  [%expect{|
    (* ------------------------------------------------------------------------ *)
    File ./examples/attributes.ml, line 32, columns: 12-22
    Warning:
    	using `@` to prepend an element to a list
    You wrote:
    	 ["w7"] @ t
    Consider:
    	using `::` instead

    (* ------------------------------------------------------------------------ *)
    File ./examples/attributes.ml, line 20, columns: 10-20
    Warning:
    	using `@` to prepend an element to a list
    You wrote:
    	 ["w5"] @ t
    Consider:
    	using `::` instead

    (* ------------------------------------------------------------------------ *)
    File ./examples/attributes.ml, line 17, columns: 17-29
    Warning:
    	using `@` to prepend an element to a list
    You wrote:
    	 (["w4"] @ t)
    Consider:
    	using `::` instead

    (* ------------------------------------------------------------------------ *)
    File ./examples/attributes.ml, line 11, columns: 10-20
    Warning:
    	using `@` to prepend an element to a list
    You wrote:
    	 ["w2"] @ t
    Consider:
    	using `::` instead
  |}]

(* Run the tests in record_update.ml *)
let%expect_test _ =
  let file : string = "./examples/record.ml" in
  let to_lint = to_ast file in
  lint_and_hint to_lint;
  [%expect{|
    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 96, columns: 26-82
    Warning:
    	Use record update syntax ({ r with ... }) when copying multiple fields.
    You wrote:
    	 { f1 = (r2_base1.f1); f2 = (r2_base1.f2); f3 = (r2_base2.f3) }
    Consider:
    	{ r2_base1 with f3 = r2_base2.f3 }

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 89, columns: 29-123
    Warning:
    	Use record update syntax ({ r with ... }) when copying multiple fields.
    You wrote:
    	 {
      a = ((get_base_record ()).a);
      b = ((get_base_record ()).b);
      c = "new_c_func";
      d = "new_d_func"
    }
    Consider:
    	{ get_base_record () with c = "new_c_func"; d = "new_d_func" }

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 83, columns: 34-116
    Warning:
    	Use record update syntax ({ r with ... }) when copying multiple fields.
    You wrote:
    	 {
      c = "new_c_ordered";
      a = (base_record.a);
      d = "new_d_ordered";
      b = (base_record.b)
    }
    Consider:
    	{ base_record with c = "new_c_ordered"; d = "new_d_ordered" }

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 79, columns: 46-103
    Warning:
    	Use record update syntax ({ r with ... }) when copying multiple fields.
    You wrote:
    	 { a = (base_record.a); b = 10; c = "c"; d = (base_record.d) }
    Consider:
    	{ base_record with b = 10; c = "c" }

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 75, columns: 31-104
    Warning:
    	Use record update syntax ({ r with ... }) when copying multiple fields.
    You wrote:
    	 { new_a = (base_record.a); new_b = 10; new_c = "c"; new_d = (base_record.d) }
    Consider:
    	{ base_record with new_b = 10; new_c = "c" }

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, lines 28-33, columns: 25-1
    Warning:
    	Use record update syntax ({ r with ... }) when copying multiple fields.
    You wrote:
    	 {
      a = (base_record.a);
      b = (base_record.b);
      c = (String.uppercase_ascii "new_c_complex");
      d = (Printf.sprintf "%s_complex" "new_d")
    }
    Consider:
    	{ base_record with c = String.uppercase_ascii "new_c_complex"; d = Printf.sprintf "%s_complex" "new_d" }

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 24, columns: 28-106
    Warning:
    	Use record update syntax ({ r with ... }) when copying multiple fields.
    You wrote:
    	 {
      a = (base_record.a);
      b = (base_record.b);
      c = (base_record.c);
      d = (base_record.d)
    }
    Consider:
    	base_record

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 20, columns: 14-67
    Warning:
    	Use record update syntax ({ r with ... }) when copying multiple fields.
    You wrote:
    	 { x = (base_s.x); y = (base_s.y); z = (base_s.z); w = 500 }
    Consider:
    	{ base_s with w = 500 }

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 16, columns: 19-85
    Warning:
    	Use record update syntax ({ r with ... }) when copying multiple fields.
    You wrote:
    	 { a = (base_record.a); b = (base_record.b); c = "new_c"; d = "new_d" }
    Consider:
    	{ base_record with c = "new_c"; d = "new_d" }

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 96, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let new_r2_multi_source = { f1 = r2_base1.f1; f2 = r2_base1.f2; f3 = r2_base2.f3 }
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 91, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 (* Multiple different records being updated from, but one of them multiple times *)
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 89, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let new_record_func_source = { a = (get_base_record ()).a; b = (get_base_record ()).b; c = "new_c_func"; d = "new_d_func" }
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 88, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 (* EXPECT: Warning, Fix: { (get_base_record ()) with c = "new_c_func"; d = "new_d_func" } *)
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 83, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let new_record_fields_reordered = { c = "new_c_ordered"; a = base_record.a; d = "new_d_ordered"; b = base_record.b }
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 82, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 (* EXPECT: Warning, Fix: { base_record with c = "new_c_ordered"; d = "new_d_ordered" } (order in 'with' might vary but should be "; " separated) *)
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 79, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let warn_different_target_names_same_source = { a = base_record.a; b = 10; c = "c"; d = base_record.d }
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 75, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let no_warn_4_actually_warns = { new_a = base_record.a; new_b = 10; new_c = "c"; new_d = base_record.d }
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 71, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	    The values for 'a' and 'd' in the fix are taken implicitly from base_record. This is correct.
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 70, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	    However, the original record was { a = base_record.a; b = 10; c = "c"; d = base_record.d }
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 66, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	    If `a = base_record.a` and `d = base_record.d`, then `base_record` is used twice.
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 64, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	    If new_a = r.a and new_d = r.d, and these are the only two from r, then it's {r with new_b = 10; new_c = "c"}.
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 62, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	    Let's refine: the current rule counts source expressions. So if r.a and r.d are used, that's two uses of 'r'.
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 61, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 (* EXPECT: No Warning (or if it warns, it's because 'a = base_record.a' is one copied field, and 'd = base_record.d' is another, making two - this depends on how "copied" is strictly defined regarding field names)
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 59, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 (* 10. Case where a field is copied but to a different field name (should not count as "copied" for this rule) *)
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 57, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let already_updated_self_ref_multiple = { base_record with a = base_record.a + 1; b = base_record.b + 2 }
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 54, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 (* This is a valid use of update syntax and should not be confused with verbose copying *)
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 53, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 (* 9. Record update syntax where the 'with' part itself refers to the base record's field *)
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 47, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let no_warn_3 = { a = base_record.a; b = other_record.b; c = "mixed"; d = "source" }
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 39, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let no_warn_1 = { a = base_record.a; b = 3; c = "new_c_prime"; d = "new_d_prime" }
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 27, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 (* EXPECT: Warning, Fix: { base_record with c = String.uppercase_ascii "new_c_complex"; d = Printf.sprintf "%s_complex" "new_d" } *)
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 24, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let new_record_all_copied = { a = base_record.a; b = base_record.b; c = base_record.c; d = base_record.d }
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/record.ml, line 16, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let new_record_1 = { a = base_record.a; b = base_record.b; c = "new_c"; d = "new_d" }
    Consider:
    	indenting to avoid exceeding the 80 character line limit |}]