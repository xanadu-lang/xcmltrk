#staload
"prelude/DATS/gint.dats"
#staload
"prelude/DATS/bool.dats"
#staload
"prelude/DATS/list.dats"

#extern
fun foo: int

fun 
f1() = foo
where
{
implement
foo = 1
}

fun 
f2() = 
let
val x
var y = 1
val () = y := 2
in
foo
end
where
{
implement
foo = 2
}

fun
main0() = 
let
val x1 = f1()
val x2 = f2()
in
end
 
(* fun *)
(* count1{a:type}( ls: list(a) ): int = *)
(* let *)
(* val x = lam(list_cons(a, ls), b, c) => a *)
(* and y = lam => 3 *)
(* (\* val () = x := 2 *\) *)
(* in *)
(* case ls of *)
(* | list_cons(_, ls) => *)
(*   count2(ls) *)
(* | _ => *)
(*   count2(ls) *)
(* end *)

(* and *)

(* count2{a:type}( ls: list(a) ): int = *)
(* case ls of *)
(* | list_cons(_, ls) => *)
(*   count1(ls) *)
(* | _ => 0 *)
  
