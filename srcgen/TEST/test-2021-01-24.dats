#include
"share/xatsopt_prelude.hats"
(* ****** ****** *)

fun foo x y z = z

val test0 = foo 1 2 3
val test1 = foo 1
val test2 = test1 2
val test3 = test1 3
