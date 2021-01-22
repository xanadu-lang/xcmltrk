#include
"share/xatsopt_prelude.hats"
(* ****** ****** *)

fun
option2bool(opt) =
case opt of
| some(_) => true
| none() => false

(* ****** ****** *)

(* end of [test-2021-01-22.dats] *)
