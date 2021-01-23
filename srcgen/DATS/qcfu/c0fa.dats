//
#include
"share/atspre_staload.hats"
#staload
UN="prelude/SATS/unsafe.sats"
//
#include
"./../../HATS/libxcmltrk.hats"
#staload $XATSOPT(*open-it*)
//
#staload $LOC
#staload $LEX
#staload $SYM
#staload $INTREP0
//
#staload "./../../SATS/xcps.sats"
#staload "./../../SATS/c0fa.sats"
//
(* ****** ****** *)

implement
c0fa_c0val_1(v, env) =
let
val node = v.node()
in
case node of
| C0Vvar(x) => env.lookup(x)
| C0Vvknd(_,x) => env.lookup(x)
//
| C0Vfcon(_) => c0set_sing(T_VAL(v))
| C0Vtcon(_,_) => c0set_sing(T_VAL(v))
//
| C0Vfcst(x) => env.lookup(x)
| C0Vtcst(x,_) => env.lookup(x)
//
| C0Vlam_hfarg(_,_,_) => c0set_sing(T_VAL(v))
//
| C0Vlam_hdcst(_,_,_) => c0set_sing(T_VAL(v))
//
| C0Vlam_hdvar(_,_,_) => c0set_sing(T_VAL(v))
//
| _ => c0set_empty()
end

(* ****** ****** *)

implement
c0fa_c0val_n(v, env) =
let
val node = v.node()
in
case node of
| C0Vfix(fdcl) =>
  list_vt2t(list_map<cfundecl><c0set>(fdcl))
  where
  {
  implement
  list_map$fopr<cfundecl><c0set>(fdcl) =
  c0set_sing(T_FUN(fdcl))
  }
| _ => 
  list_vt2t(list_make_sing<c0set>(c0fa_c0val_1(v, env)))
end

(* ****** ****** *)

implement
c0fa_c0valst(vs, env) =
let
val xss = list_vt2t(list_map<c0val><c0setlst>(vs))
where
{
implement
list_map$fopr<c0val><c0setlst>(v) =
c0fa_c0val_n(v, env)
}
in
list_vt2t(list_concat<c0set>(xss))
end

(* ****** ****** *)

implement
c0fa_c0nt(k, env) =
let
val node = k.node()
in
case node of
| C0HALT() => c0set_empty()
| C0VAR(k) => env.lookup(k)
| C0NT(_,_) => c0set_sing(T_KNT(k))
end

(* ****** ****** *)

implement
c0fa_c0clau(clau, env) =
c0set_sing(T_CLAU(clau))

(* ****** ****** *)

implement
c0fa_c0claulst(claulst, env) =
list_vt2t(list_map<c0clau><c0set>(claulst))
where
{
implement
list_map$fopr<c0clau><c0set>(clau) =
c0fa_c0clau(clau, env)
}

(* ****** ****** *)

implement
c0fa_c0exp(e, env) =
let
val node = e.node()
in
case node of
| C0Eret(k, v) =>
  let
  val l = k.label()
  val K = c0fa_c0nt(k, env)
  val V = c0fa_c0val_1(v, env)
  val cache = c0fa_C0Eret(K, V, env)
  in
  cache
  end
| C0Edapp(f, vs, k) =>
  let
  val l = f.label()
  val F = c0fa_c0val_1(f, env)
  val VS = c0fa_c0valst(vs, env)
  val K = c0fa_c0nt(k, env)
  val cache = c0fa_C0Edapp(F, VS, K, env)
  in
  cache
  end
| C0Eprimop(p, vs, k) =>
  let
  val VS = c0fa_c0valst(vs, env)
  val K = c0fa_c0nt(k, env)
  val cache = c0fa_C0Eprimop(p, VS, K, env)
  in
  cache
  end
| C0Eif0(v, k1, k2) =>
  let
  val l1 = k1.label()
  val l2 = k2.label()
  val V = c0fa_c0val_1(v, env)
  val K1 = c0fa_c0nt(k1, env)
  val K2 = c0fa_c0nt(k2, env)
  val cache1 = c0fa_C0Eret(K1, V, env)
  val cache2 = c0fa_C0Eret(K2, V, env)
  val cache = union(cache1, cache2)
  in
  cache
  end
| C0Ecase(_, v, clau, k) =>
  let
  val V = c0fa_c0val_1(v, env)
  val CLAU = c0fa_c0claulst(clau, env)
  val K = c0fa_c0nt(k, env)
  val cache = c0fa_C0Ecase(V, CLAU, K, env)
  in
  cache
  end
| C0Etry0(_, v, clau, k) =>
  let
  val V = c0fa_c0val_1(v, env)
  val CLAU = c0fa_c0claulst(clau, env)
  val K = c0fa_c0nt(k, env)
  val cache = c0fa_C0Etry0(V, CLAU, K, env)
  in
  cache
  end
end

(* ****** ****** *)
