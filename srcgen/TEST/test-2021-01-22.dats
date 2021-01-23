#include
"share/xatsopt_prelude.hats"
(* ****** ****** *)

fun
option2bool(opt) =
case opt of
| some(_) => true
| none() => false

val test = option2bool(none())
val test = test

(* ****** ****** *)

(* end of [test-2021-01-22.dats] *)
