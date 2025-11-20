type r = { a : int; b : int; c : string }
let my_r = { a = 1; b = 2; c = "hello" }

(* ====== LET BINDINGS ====== *)

(* Should trigger: a is used once *)
let () =
  let { a; b } = my_r in
  print_int a

(* Should not trigger: b is used twice *)
let () =
  let { a; b } = my_r in
  print_int b;
  print_int b

(* Should trigger: a_alias is used once *)
let () =
  let { a = a_alias; b } = my_r in
  print_int a_alias

(* Should not trigger: a_alias is used twice *)
let () =
  let { a = a_alias; b } = my_r in
  print_int a_alias;
  print_int a_alias

module M = struct
  type r = { foo : int; bar : int }
  let my_r = { foo = 1; bar = 2 }
end

(* Should trigger: foo is used once *)
let () =
  let { M.foo; bar } = M.my_r in
  print_int foo

(* Should not trigger: foo is used once, but get_r has side effects *)
let get_r () = print_endline "getting r"; M.my_r
let () =
    let { M.foo; bar } = get_r () in
    print_int foo

(* Should not trigger: my_r is shadowed *)
let () =
  let { a; b } = my_r in
  let my_r = { a = 3; b = 4; c = "world" } in
  print_int a

(* Should not trigger: get_r is shadowed *)
let () =
    let { M.foo; bar } = get_r () in
    let get_r = fun () -> { M.foo = 3; M.bar = 4 } in
    print_int foo

(* Should not trigger: x is shadowed in let body *)
let x = 1
let () =
    let { a } = my_r in
    let x = a in
    print_int x

type nested = { d : r }
let my_nested = { d = { a = 1; b = 2; c = "nested" } }

(* Should trigger: a is used once *)
let () =
  let { d = { a; b } } = my_nested in
  print_int a

(* ====== FUNCTION ARGUMENTS ====== *)

(* Should trigger: a is used once *)
let my_func ({ a; b } as r) =
  print_int a

(* Should not trigger: b is used twice *)
let my_func_2 ({ a; b } as r) =
  print_int b;
  print_int b

(* Should trigger: a_alias is used once *)
let my_func_3 ({ a = a_alias; b } as r) =
  print_int a_alias

(* Should not trigger: a_alias is used twice *)
let my_func_4 ({ a = a_alias; b } as r) =
  print_int a_alias;
  print_int a_alias

(* Should trigger: foo is used once *)
let my_func_5 ({ M.foo; bar } as r) =
  print_int foo

(* Should trigger: a is used once, no 'as' alias *)
let my_func_6 { a; b } =
  print_int a
