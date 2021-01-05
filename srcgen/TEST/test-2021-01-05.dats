(* ****** ****** *)
#include
"share\
/xatsopt_prelude.hats"
(* ****** ****** *)

fun
fact(n: int): int =
if n > 0 then n * fact(n-1) else 1

(* ****** ****** *)

(* end of [test-2021-01-05.dats] *)
