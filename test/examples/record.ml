(* Test cases for the UseRecordUpdateSyntax check *)

(* Type definitions *)
type r = { a : int; b : int; c : string; d : string }
type s = { x : int; y : int; z : int; w : int }

(* Base records for testing *)
let base_record = { a = 1; b = 2; c = "c"; d = "d" }
let other_record = { a = 5; b = 6; c = "oc"; d = "od" }
let base_s = { x = 10; y = 20; z = 30; w = 40 }

(* ---- CASES THAT SHOULD TRIGGER THE WARNING ---- *)

(* 1. Basic case: two fields copied, two new *)
(* EXPECT: Warning, Fix: { base_record with c = "new_c"; d = "new_d" } *)
let new_record_1 =
  { a = base_record.a; b = base_record.b; c = "new_c"; d = "new_d" }

(* 2. More copied fields: three fields copied, one new *)
(* EXPECT: Warning, Fix: { base_s with w = 500 } *)
let new_s_1 = { x = base_s.x; y = base_s.y; z = base_s.z; w = 500 }

(* 3. All fields copied from the same source *)
(* EXPECT: Warning, Fix: base_record *)
let new_record_all_copied =
  { a = base_record.a; b = base_record.b;
    c = base_record.c; d = base_record.d }

(* 4. Complex expressions for non-copied fields *)
(* EXPECT: Warning,
Fix: { base_record with
         c = String.uppercase_ascii "new_c_complex";
         d = Printf.sprintf "%s_complex" "new_d" }
*)
let new_record_complex = { 
  a = base_record.a; 
  b = base_record.b; 
  c = String.uppercase_ascii "new_c_complex"; 
  d = Printf.sprintf "%s_complex" "new_d" 
}

(* ---- CASES THAT SHOULD NOT TRIGGER THE WARNING ---- *)

(* 5. Only one field copied *)
(* EXPECT: No Warning *)
let no_warn_1 =
  { a = base_record.a; b = 3; c = "new_c_prime"; d = "new_d_prime" }

(* 6. No fields copied (all new values) *)
(* EXPECT: No Warning *)
let no_warn_2 = { a = 100; b = 200; c = "hello"; d = "world" }

(* 7. Fields copied from DIFFERENT records *)
(* EXPECT: No Warning *)
let no_warn_3 =
  { a = base_record.a; b = other_record.b; c = "mixed"; d = "source" }

(* 8. Already using record update syntax *)
(* EXPECT: No Warning *)
let already_updated = { base_record with c = "already_updated" }

(* 9. Record update syntax where the 'with' part itself refers
   to the base record's field *)
(* This is a valid use of update syntax and should not be confused with
   verbose copying *)
(* EXPECT: No Warning *)
let already_updated_self_ref = { base_record with b = base_record.b + 1 }
let already_updated_self_ref_multiple =
  { base_record with a = base_record.a + 1; b = base_record.b + 2 }

(* 10. Case where a field is copied but to a different field name
   (should not count as "copied" for this rule) *)
(* This is not what the rule targets; it targets {f1 = r.f1; f2 = r.f2, ...}*)
(* EXPECT: No Warning *)
type r_prime = { new_a : int; new_b : int; new_c : string; new_d : string }
let no_warn_4 =
  { new_a = base_record.a; new_b = 10; new_c = "c"; new_d = base_record.d }

(* Let's add a clear case for the above, matching the type 'r' *)
(* EXPECT: Warning, Fix: { base_record with b = 10; c = "c" } *)
let warn_different_target_names_same_source =
  { a = base_record.a; b = 10; c = "c"; d = base_record.d }

(* Ensure field order doesn't matter for differing fields *)
(* EXPECT: Warning,
   Fix: { base_record with c = "new_c_ordered"; d = "new_d_ordered" }
   (order in 'with' might vary but should be "; " separated) *)
let new_record_fields_reordered =
  { c = "new_c_ordered";
    a = base_record.a;
    d = "new_d_ordered";
    b = base_record.b }

(* What if the source is complex? e.g. (get_record()).f1 *)
(* The Pprintast.string_of_expression should handle this. *)
let get_base_record () = base_record
(* EXPECT: Warning,
   Fix: { (get_base_record ()) with c = "new_c_func"; d = "new_d_func" } *)
let new_record_func_source =
  { a = (get_base_record ()).a;
    b = (get_base_record ()).b;
    c = "new_c_func"; d = "new_d_func" }

(* Multiple different records being updated from,
   but one of them multiple times *)
type r2 = { f1: int; f2: int; f3: int }
let r2_base1 = { f1=1; f2=2; f3=3 }
let r2_base2 = { f1=10; f2=20; f3=30 }
(* EXPECT: Warning for r2_base1. Fix: { r2_base1 with f3 = r2_base2.f3 } *)
let new_r2_multi_source =
  { f1 = r2_base1.f1; f2 = r2_base1.f2; f3 = r2_base2.f3 }

let new_r2_different_fields =
  { f1 = r2_base1.f1; f2 = r2_base1.f2;
    f3 = r2_base1.f2 (* not copied from same field *)}


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
