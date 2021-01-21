//
#include
"share/atspre_staload.hats"
#staload
UN="prelude/SATS/unsafe.sats"
//
#include
"./../HATS/libxcmltrk.hats"
#staload $XATSOPT(*open-it*)
//
#staload $LOC
#staload $LEX
#staload $SYM
#staload $INTREP0
//
#staload "./../SATS/xlambda.sats"
#staload "./../SATS/xcps.sats"
//
(* ****** ****** *)

implement
fresh_hdvar(s) =
let
val loc = $UN.cast{loc_t}(0)
val sym = symbol_make(s)
val knd = $UN.cast{tnode}(0)
val htp = $UN.cast{h0typ}(0)
in
hdvar_make_idtp(loc, sym, knd, htp)
end

(* ****** ****** *)

local

val kvar_stamp = ref<int>(0)
assume kvar = '(string, int)

in

implement
fresh_kvar(s) =
let
val n = kvar_stamp[]
val () = kvar_stamp[] := n + 1
in
'(s, n)
end

implement 
eq_kvar(v1, v2) =
let
val '(_, n1) = v1
val '(_, n2) = v2
in
n1 = n2
end

implement
tostring_kvar(v) =
let
val '(s, i) = v
in
s
end

implement
tostamp_kvar(v) =
let
val '(s, i) = v
in
i
end
 
end

(* ****** ****** *)
  
implement
xcps_l0explst(es, c) =
case es of
| list_cons(e, es) =>
  xcps_l0exp(e, lam(e) =<cloref1>
  xcps_l0explst(es, lam(es) =<cloref1>
  c(list_cons(e, es))))
| list_nil() => c(list_nil())

(* ****** ****** *)

implement
xcps_l0exp(e, c) =
case e of
| L0Ei00(x) => c(C0Vi00(x))
| L0Eb00(x) => c(C0Vb00(x))
| L0Es00(x) => c(C0Vs00(x))
//
| L0Eint(t) => c(C0Vint(t))
| L0Ebtf(t) => c(C0Vbtf(t))
| L0Echr(t) => c(C0Vchr(t))
| L0Eflt(t) => c(C0Vflt(t))
| L0Estr(t) => c(C0Vstr(t))
//
| L0Etop(t) => c(C0Vtop(t))
//
| L0Evar(v) => c(C0Vvar(v))
| L0Evknd(i, v) => c(C0Vvknd(i, v))
//
| L0Efcon(hc) => c(C0Vfcon(hc))
| L0Etcon(hc, ht) => c(C0Vtcon(hc, ht))
//
| L0Efcst(hc) => c(C0Vfcst(hc))
| L0Etcst(hc, ht) => c(C0Vtcst(hc, ht))
//
| L0Edapp(e, es) =>
  let
  val x = fresh_hdvar("x")
  val xbod = C0Vvar(x)
  val k = C0NT(x,  c(xbod))
  in
  xcps(e, lam(e) =<cloref1> 
  xcps(es, lam(es) =<cloref1> 
  C0Edapp(e, es, k) 
  ))
  end
//
| L0Epcon(e, l) =>
  xcps(e, lam(e) =<cloref1> 
  let
  val x = fresh_hdvar("x")
  val xbod = C0Vvar(x)
  val k = C0NT(x,  c(xbod))
  val k = 
  list_vt2t(list_make_sing<c0nt>(k))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  in
  C0Eprimop(C0Ppcon(l), e, k) 
  end)
| L0Epbox(e, l, i) =>
  xcps(e, lam(e) =<cloref1> 
  let
  val x = fresh_hdvar("x")
  val xbod = C0Vvar(x)
  val k = C0NT(x,  c(xbod))
  val k = 
  list_vt2t(list_make_sing<c0nt>(k))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  in
  C0Eprimop(C0Ppbox(l, i), e, k) 
  end)
//
| L0Eproj(e, l, i) =>
  xcps(e, lam(e) =<cloref1> 
  let
  val x = fresh_hdvar("x")
  val xbod = C0Vvar(x)
  val k = C0NT(x,  c(xbod))
  val k = 
  list_vt2t(list_make_sing<c0nt>(k))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  in
  C0Eprimop(C0Pproj(l, i), e, k) 
  end)
| L0Eplft(e, l, i) =>
  xcps(e, lam(e) =<cloref1> 
  let
  val x = fresh_hdvar("x")
  val xbod = C0Vvar(x)
  val k = C0NT(x,  c(xbod))
  val k = 
  list_vt2t(list_make_sing<c0nt>(k))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  in
  C0Eprimop(C0Pplft(l, i), e, k) 
  end)
| L0Epptr(e, l, i) =>
  xcps(e, lam(e) =<cloref1> 
  let
  val x = fresh_hdvar("x")
  val xbod = C0Vvar(x)
  val k = C0NT(x,  c(xbod))
  val k = 
  list_vt2t(list_make_sing<c0nt>(k))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  in
  C0Eprimop(C0Ppptr(l, i), e, k) 
  end)
//
| L0Etuple(i, j, es) =>
  xcps(es, lam(es) =<cloref1> 
  let
  val x = fresh_hdvar("x")
  val xbod = C0Vvar(x)
  val k = C0NT(x,  c(xbod))
  val k = 
  list_vt2t(list_make_sing<c0nt>(k))
  in
  C0Eprimop(C0Ptuple(i, j), es, k) 
  end)
//
| L0Eassgn(e1, e2) =>
  xcps(e1, lam(e1) =<cloref1> 
  xcps(e2, lam(e2) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = C0Vvar(x)
  val k = C0NT(x,  c(xbod))
  val k = 
  list_vt2t(list_make_sing<c0nt>(k))
  //
  val es = 
  list_cons(e1, list_cons(e2, list_nil()))
  in
  C0Eprimop(C0Passgn(), es, k)
  end))
//
| L0Eif0(e1, e2, e3) =>
  xcps(e1, lam(e1) =<cloref1>
  let
  val e1 = 
  list_vt2t(list_make_sing<c0val>(e1))
  //
  val x = fresh_hdvar("x")
  val y = fresh_hdvar("y")
  val xbod = xcps(e2, lam(e2) =<cloref1> c(e2))
  val ybod =
  case e3 of
  | Some(e3) => xcps(e3, lam(e3) =<cloref1> c(e3))
  | None() => c(C0Vnone0())
  //
  val k2 = C0NT(x, xbod)
  val k3 = C0NT(y, ybod)
  //
  val ks = 
  list_cons(k2, list_cons(k3, list_nil()))
  in
  C0Eprimop(C0Pif0(), e1, ks)
  end)
//
| L0Ecase(i, e, cs) =>
  xcps(e, lam(e) =<cloref1>
  let 
  val e =
  list_vt2t(list_make_sing<c0val>(e))
  //
  val x = fresh_hdvar("x")
  val y = fresh_hdvar("y")
  val xbod = 
  list_vt2t(list_make_sing<c0val>(C0Vvar(x)))
  val ybod = C0Vvar(y)
  //
  val cs = xcps(cs, lam(f) =<cloref1> 
  C0Edapp(f, xbod, C0NT(y, c(ybod))))
  val ks = 
  list_vt2t(list_map<c0exp><c0nt>(cs))
  where
  {
  implement
  list_map$fopr<c0exp><c0nt>(bod) =
  C0NT(x, bod)
  }
  in
  C0Eprimop(C0Pcase(i), e, ks)
  end)
//
| L0Efix(fdcl) =>
  xcps(fdcl, lam(fdcl) =<cloref1>
  c(C0Vfix(fdcl)))
| L0Elam_hfarg(a, e) =>
  let
  val k = fresh_kvar("k")
  val bod =
  xcps(e, lam(e) =<cloref1> C0Eret(C0VAR(k), e))
  in
  c(C0Vlam_hfarg(a, k, bod))
  end
| L0Elam_hdcst(a, e) =>
  let
  val k = fresh_kvar("k")
  val bod =
  xcps(e, lam(e) =<cloref1> C0Eret(C0VAR(k), e))
  in
  c(C0Vlam_hdcst(a, k, bod))
  end
| L0Elam_hdvar(a, e) =>
  let
  val k = fresh_kvar("k")
  val bod =
  xcps(e, lam(e) =<cloref1> C0Eret(C0VAR(k), e))
  in
  c(C0Vlam_hdvar(a, k, bod))
  end
//
| L0Etry0(t, e, cs) =>
  xcps(e, lam(e) =<cloref1>
  let
  val e =
  list_vt2t(list_make_sing<c0val>(e))
  //
  val x = fresh_hdvar("x")
  val y = fresh_hdvar("y")
  val xbod = 
  list_vt2t(list_make_sing<c0val>(C0Vvar(x)))
  val ybod = C0Vvar(y)
  //
  val cs = xcps(cs, lam(f) =<cloref1> 
  C0Edapp(f, xbod, C0NT(y, c(ybod))))
  val k = C0NT(y, c(ybod))
  val ks = 
  list_vt2t(list_map<c0exp><c0nt>(cs))
  where
  {
  implement
  list_map$fopr<c0exp><c0nt>(bod) =
  C0NT(x, bod)
  }
  in
  C0Eprimop(C0Ptry0(t), e, list_cons(k, ks))
  end)
//
| L0Eaddr(e) =>
  xcps(e, lam(e) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = C0Vvar(x)
  val k = C0NT(x,  c(xbod))
  val k = 
  list_vt2t(list_make_sing<c0nt>(k))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  in
  C0Eprimop(C0Paddr(), e, k)
  end)
| L0Eflat(e) =>
  xcps(e, lam(e) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = C0Vvar(x)
  val k = C0NT(x,  c(xbod))
  val k = 
  list_vt2t(list_make_sing<c0nt>(k))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  in
  C0Eprimop(C0Pflat(), e, k)
  end)
| L0Etalf(e) =>
  xcps(e, lam(e) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = C0Vvar(x)
  val k = C0NT(x,  c(xbod))
  val k = 
  list_vt2t(list_make_sing<c0nt>(k))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  in
  C0Eprimop(C0Ptalf(), e, k)
  end)
//
| L0Efold(e) =>
  xcps(e, lam(e) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = C0Vvar(x)
  val k = C0NT(x,  c(xbod))
  val k = 
  list_vt2t(list_make_sing<c0nt>(k))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  in
  C0Eprimop(C0Pfold(), e, k)
  end)
//
| L0Eeval(i, e) =>
  xcps(e, lam(e) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = C0Vvar(x)
  val k = C0NT(x,  c(xbod))
  val k = 
  list_vt2t(list_make_sing<c0nt>(k))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  in
  C0Eprimop(C0Peval(i), e, k)
  end)
//
| L0Efree(i, e) =>
  xcps(e, lam(e) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = C0Vvar(x)
  val k = C0NT(x,  c(xbod))
  val k = 
  list_vt2t(list_make_sing<c0nt>(k))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  in
  C0Eprimop(C0Pfree(i), e, k)
  end)
//
| L0Eraise(e) =>
  xcps(e, lam(e) =<cloref1>
  let 
  val x = fresh_hdvar("x")
  val xbod = C0Vvar(x)
  val k = C0NT(x, c(xbod))
  val k =
  list_vt2t(list_make_sing<c0nt>(k))
  //
  val e =
  list_vt2t(list_make_sing<c0val>(e))
  in
  C0Eprimop(C0Praise(), e, k)
  end)
//
| L0Elazy(e) =>
  xcps(e, lam(e) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = C0Vvar(x)
  val k = C0NT(x,  c(xbod))
  val k = 
  list_vt2t(list_make_sing<c0nt>(k))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  in
  C0Eprimop(C0Plazy(), e, k)
  end)
| L0Ellazy(e, es) =>
  xcps_l0exp(e, lam(e) =<cloref1>
  xcps_l0explst(es, lam(es) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = C0Vvar(x)
  val k = C0NT(x,  c(xbod))
  val k = 
  list_vt2t(list_make_sing<c0nt>(k))
  //
  val es = list_cons(e, es)
  in
  C0Eprimop(C0Pllazy(), es, k)
  end))
//
| L0Enone0() => c(C0Vnone0())
| L0Enone1(p) => c(C0Vnone1(p))

(* ****** ****** *)

implement
xcps_lfundecl(fdcl, c) =
let
val k = fresh_kvar("k")
//
val LFUNDECL(fdcl) = fdcl
val nam = fdcl.nam
val hag = fdcl.hag
val e = 
case fdcl.def of
| Some(e) =>
  Some(xcps(e, lam(e) =<cloref1> 
  C0Eret(C0VAR(k), e)))
| None() => None()
//
in
c(CFUNDECL(@{
  nam= nam  
, hag= hag
, knt= k
, def= e
}))
end

(* ****** ****** *)
  
implement
xcps_lfundeclst(fdclst, c) =
case fdclst of
| list_cons(fdcl, fdclst) =>
  xcps_lfundecl(fdcl, lam(fdcl) =<cloref1>
  xcps_lfundeclst(fdclst, lam(fdclst) =<cloref1>
  c(list_cons(fdcl, fdclst))))
| list_nil() =>
  c(list_nil())

(* ****** ****** *)

implement
xcps_l0clau(clau, c) =
case clau of
| L0CLAUpat(gpat) =>
  let
  val k = fresh_kvar("k")
  val bod = C0Eret(C0VAR(k), C0Vnone0())
  in
  xcps(gpat, lam(gpat) =<cloref1>
  c(C0Vlam_c0gpat(gpat, k, bod)))
  end
| L0CLAUexp(gpat, bod) =>
  let
  val k = fresh_kvar("k")
  val bod = 
  xcps(bod, lam(e) =<cloref1>
  C0Eret(C0VAR(k), e))
  in
  xcps(gpat, lam(gpat) =<cloref1>
  c(C0Vlam_c0gpat(gpat, k, bod)))
  end

(* ****** ****** *)

implement
xcps_l0claulst(clau, c) =
list_vt2t(list_map<l0clau><c0exp>(clau))
where
{
implement  
list_map$fopr<l0clau><c0exp>(clau) =
xcps(clau, c)
}

(* ****** ****** *)

implement
xcps_l0gpat(gpat, c) =
case gpat of
| L0GPATpat(pat) => c(C0GPATpat(pat))
| L0GPATgua(pat, gua) =>
  xcps(gua, lam(gua) =<cloref1> 
  c(C0GPATgua(pat, gua)))

(* ****** ****** *)

implement
xcps_l0gua(gua, c) =
case gua of
| L0GUAexp(e) =>
  xcps(e, lam(e) =<cloref1> 
  c(C0GUAexp(e)))
| L0GUAmat(e, pat) =>
  xcps(e, lam(e) =<cloref1>
  c(C0GUAmat(e, pat)))

(* ****** ****** *)

implement
xcps_l0gualst(gualst, c) =
case gualst of
| list_cons(gua, gualst) =>
  xcps_l0gua(gua, lam(gua) => 
  xcps_l0gualst(gualst, lam(gualst) => 
  c(list_cons(gua, gualst))))
| list_nil() =>
  c(list_nil())
