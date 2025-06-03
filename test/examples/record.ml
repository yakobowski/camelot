type my_record_fields = {
  f1: int;
  f2: string;
  f3: int;
  one_very_long_field_name: int;
  another_very_long_field_name: int
};;

(* Definition for type inference tests *)
type another_record = { a: int; b: string };;
let _r_val = { a = 1; b = "hi" };; (* Use _r_val to avoid unused warning, helps type context *)

(* Cases that SHOULD trigger the warning *)

let func_triggers_1
  ({ f1; f2 } : my_record_fields) =
  f1 + String.length f2
;;

let func_triggers_2 x
  ({ f1; f2; f3; _ } : my_record_fields) y =
  x + f1 + (String.length f2) + f3 + y
;;

let func_triggers_3
  ({ one_very_long_field_name;
     another_very_long_field_name; _ } : my_record_fields) =
  one_very_long_field_name + another_very_long_field_name
;;

let func_triggers_4
  (({ f1; f2; _ } as r) : my_record_fields) =
  r.f1 + String.length r.f2
;;

let func_triggers_5
  ({ f1 = _; f2 = _; _ } : my_record_fields) =
  0
;;

let func_triggers_6
  ({f1;f2;_} : my_record_fields)
  ({f3;_} : my_record_fields) =
  f1 + (String.length f2) + f3
;;

let func_triggers_7
  { a; b } =
  a + String.length b
;;

(* Cases that SHOULD NOT trigger the warning *)

let func_no_trigger_1 ({ f1; f2; _ } : my_record_fields) =
  f1 + String.length f2
;;

let func_no_trigger_2
  ({ f1 } : my_record_fields) =
  f1
;;

let func_no_trigger_3
  x y z = x + y + z
;;

let func_no_trigger_4
    x
    ({ f1; _ } : my_record_fields)
    y =
    x + f1 + y
;;

let func_no_trigger_5
 ({ f1 } : my_record_fields) =
 f1
;;

let func_no_trigger_6
  { a } =
  a
;;
