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
#staload $LAB
#staload $INTREP0
//
#staload "./../SATS/xlambda.sats"
#staload "./../SATS/xcps.sats"
//
(* ****** ****** *)

implement
fprint_val<hfarg> = fprint_hfarg

implement
fprint_val<hdcst> = fprint_hdcst

implement
fprint_val<hdvar> = fprint_hdvar

(* ****** ****** *)

implement
fprint_val<kvar> = fprint_kvar

implement
fprint_val<c0val> = fprint_c0val

implement
fprint_val<c0exp> = fprint_c0exp

implement
fprint_val<c0nt> = fprint_c0nt

implement
fprint_val<cfundecl> = fprint_cfundecl

implement
fprint_val<c0primop> = fprint_c0primop

implement
fprint_val<c0gpat> = fprint_c0gpat

implement
fprint_val<c0gua> = fprint_c0gua

(* ****** ****** *)
  
implement 
print_kvar(v) =
fprint!(stdout_ref, v)

implement
prerr_kvar(v) =
fprint!(stderr_ref, v)

implement
fprint_kvar(fp, v) =
let
val s = tostring_kvar(v)
val n = tostamp_kvar(v)
in
fprint!(fp, s, "(", n, ")")
end

(* ****** ****** *)

implement
print_c0val(v) =
fprint!(stdout_ref, v)

implement
prerr_c0val(v) =
fprint!(stderr_ref, v)

implement
fprint_c0val(fp, v) =
case v of
| C0Vi00(x) =>
  fprint!(fp, "C0Vi00(", x, ")")
| C0Vb00(x) =>
  fprint!(fp, "C0Vb00(", x, ")")
| C0Vs00(x) =>
  fprint!(fp, "C0Vs00(", x, ")")
//
| C0Vint(t) =>
  fprint!(fp, "C0Vint(", t, ")")
| C0Vbtf(t) =>
  fprint!(fp, "C0Vbtf(", t, ")")
| C0Vchr(t) =>
  fprint!(fp, "C0Vchr(", t, ")")
| C0Vflt(t) =>
  fprint!(fp, "C0Vflt(", t, ")")
| C0Vstr(t) =>
  fprint!(fp, "C0Vstr(", t, ")")
//
| C0Vtop(t) =>
  fprint!(fp, "C0Vtop(", t, ")")
//
| C0Vvar(v) =>
  fprint!(fp, "C0Vvar(", v, ")")
| C0Vvknd(i, v) =>
  fprint!(fp, "C0Vvnd(", i, ", ", v, ")")
//
| C0Vfcon(c) =>
  fprint!(fp, "C0Vfcon(", c, ")")
| C0Vtcon(c, ht) =>
  fprint!(fp, "C0Vtcon(", c, ", ", ht, ")")
//
| C0Vfcst(cst) =>
  fprint!(fp, "C0Vfcst(", cst, ")")
| C0Vtcst(cst, ht) =>
  fprint!(fp, "C0Vtcst(", cst, ", ", ht, ")")
//
| C0Vfix(fdcl) =>
  fprint!(fp, "C0Vfix(", fdcl, ")")
| C0Vlam_hfarg(hag, k, e) =>
  fprint!(fp, "C0Vlam_hfarg(", hag, ", ", k, ", ", e, ")")
| C0Vlam_hdcst(hdc, k, e) =>
  fprint!(fp, "C0Vlam_hdcst(", hdc, ", ", k, ", ", e, ")")
| C0Vlam_hdvar(hdv, k, e) =>
  fprint!(fp, "C0Vlam_hdvar(", hdv, ", ", k, ", ", e, ")")
| C0Vlam_c0gpat(gpat, k, e) =>
  fprint!(fp, "C0Vlam_c0gpat(", gpat, ", ", k, ", ", e, ")")
//
| C0Vnone0() =>
  fprint!(fp, "C0Vnone0()")
| C0Vnone1(p) =>
  fprint!(fp, "C0Vnone1(", p, ")")

(* ****** ****** *)
  
implement
print_c0exp(e) =
fprint!(stdout_ref, e)

implement
prerr_c0exp(e) =
fprint!(stderr_ref, e)

implement
fprint_c0exp(fp, e) =
case e of
| C0Eret(c, v) =>
  fprint!(fp, "C0Eret(", c, ", ", v, ")")
| C0Edapp(f, arg, k) =>
  fprint!(fp, "C0Edapp(", f, ", [", arg, "], ", k, ")")
| C0Eprimop(prim, arg, k) =>
  fprint!(fp, "C0Eprimop(", prim, ", [", arg, "], ", k, ")")
| C0Ehalt() =>
  fprint!(fp, "C0Ehalt()")

(* ****** ****** *)

implement
print_c0nt(k) =
fprint!(stdout_ref, k)

implement
prerr_c0nt(k) =
fprint!(stderr_ref, k)

implement
fprint_c0nt(fp, k) =
case k of
| C0VAR(k) =>
  fprint!(fp, "C0VAR(", k, ")")
| C0NT(arg, e) =>
  fprint!(fp, "C0NT(", arg, ", ", e, ")")

(* ****** ****** *)

implement
print_cfundecl(fdcl) =
fprint!(stdout_ref, fdcl)

implement
prerr_cfundecl(fdcl) =
fprint!(stderr_ref, fdcl)

implement
fprint_cfundecl(fp, fdcl) =
let
val CFUNDECL(fdcl) = fdcl
in
case fdcl.hag of
| Some(fdcl_hag) =>
  fprint!
  ( fp
  , "CFUNDECL@{" 
  , "nam=", fdcl.nam, ", "
  , "hag=", fdcl_hag, ", "
  , "knt=", fdcl.knt, ", "
  , "def=", fdcl.def, "}")
| None() =>
  fprint!
  ( fp
  , "CFUNDECL@{" 
  , "nam=", fdcl.nam, ", "
  , "knt=", fdcl.knt, ", "
  , "def=", fdcl.def, "}")
end

(* ****** ****** *)

implement
print_c0primop(prim) =
fprint!(stdout_ref, prim)

implement
prerr_c0primop(prim) =
fprint!(stderr_ref, prim)

implement
fprint_c0primop(fp, prim) =
case prim of
| C0Ppcon(l) =>
  fprint!(fp, "C0Ppcon(", l, ")")
| C0Ppbox(l, i) =>
  fprint!(fp, "C0Ppbox(", l, ", ", i, ")")
//
| C0Pproj(l, i) =>
  fprint!(fp, "C0Pproj(", l, ", ", i, ")")
| C0Pplft(l, i) =>
  fprint!(fp, "C0Pplft(", l, ", ", i, ")")
| C0Ppptr(l, i) =>
  fprint!(fp, "C0Ppptr(", l, ", ", i, ")")
//
| C0Ptuple(i, j) =>
  fprint!(fp, "C0Ptuple(", i, ", ", j, ")")
//
| C0Passgn() =>
  fprint!(fp, "C0Passgn()")
//
| C0Pif0() =>
  fprint!(fp, "C0Pif0()")
//
| C0Pcase(i) =>
  fprint!(fp, "C0Pcase(", i, ")")
//
| C0Paddr() =>
  fprint!(fp, "C0Paddr()")
| C0Pflat() =>
  fprint!(fp, "C0Pflat()")
| C0Ptalf() =>
  fprint!(fp, "C0Ptalf()")
//
| C0Pfold() =>
  fprint!(fp, "C0Pfold()")
//
| C0Peval(i) =>
  fprint!(fp, "C0Peval(", i,")")
//
| C0Pfree(i) =>
  fprint!(fp, "C0Pfree(", i,")")
//
| C0Plazy() =>
  fprint!(fp, "C0Plazy()")
| C0Pllazy() =>
  fprint!(fp, "C0Pllazy()")

(* ****** ****** *)

implement
print_c0gpat(pat) =
fprint!(stdout_ref, pat)

implement
prerr_c0gpat(pat) =
fprint!(stderr_ref, pat)

implement
fprint_c0gpat(fp, pat) =
case pat of
| C0GPATpat(pat) =>
  fprint!(fp, "C0GPATpat(", pat, ")")
| C0GPATgua(pat, gua) =>
  fprint!(fp, "C0GPATgua(", pat, ", ", gua, ")")

(* ****** ****** *)

implement
print_c0gua(gua) =
fprint!(stdout_ref, gua)

implement
prerr_c0gua(gua) =
fprint!(stderr_ref, gua)

implement
fprint_c0gua(fp, gua) =
case gua of
| C0GUAexp(e) =>
  fprint!(fp, "C0GUAexp(", e, ")")
| C0GUAmat(e, pat) =>
  fprint!(fp, "C0GUAmat(", e, ", ", pat, ")")
