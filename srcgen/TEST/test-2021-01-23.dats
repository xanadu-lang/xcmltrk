#include
"share/xatsopt_prelude.hats"
(* ****** ****** *)

fun foo1(): void = foo3()
and foo2 = foo1
and foo3(): void = foo2()
