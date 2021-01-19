#include "share/atspre_staload.hats"


val f = lam(_,_,_) => 4
val g = lam x (list_cons(y, ys)) z => z

implement
main0() = 
let
val x = f(print(1), print(2), print(3))
in
print(x)
end
