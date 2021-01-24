//
#include
"share/atspre_staload.hats"
#staload
UN="prelude/SATS/unsafe.sats"
//
#define
XATSOPT_targetloc
"./../../xatsopt/srcgen/xats"
//
#include
"./../../HATS/libxcmltrk.hats"
#staload $XATSOPT(*open-it*)
//
#staload $LOC
#staload $LEX
#staload $LAB
#staload $INTREP0
//
#staload "./../../SATS/xlambda.sats"
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
fprint_val<lfundecl> = fprint_lfundecl

implement
fprint_val<lvaldecl> = fprint_lvaldecl

implement
fprint_val<lvardecl> = fprint_lvardecl

implement
fprint_val<l0clau> = fprint_l0clau

implement
fprint_val<l0gpat>(fp, gpat) =
fprint!(fp, gpat)

implement
fprint_val<l0gua>(fp, gua) =
fprint!(fp, gua)

(* ****** ****** *)

implement
print_l0exp(e) =
fprint!(stdout_ref, e)

implement
prerr_l0exp(e) =
fprint!(stderr_ref, e)

implement
fprint_val<l0exp>(fp, e) =
fprint!(fp, e)

implement
fprint_l0exp(fp, e) =
case e of
//
| L0Ei00(x) =>
  fprint!(fp, "L0Ei00(", x, ")")
| L0Eb00(x) =>
  fprint!(fp, "L0Eb00(", x, ")")
| L0Es00(x) =>
  fprint!(fp, "L0Es00(", x, ")")
//
| L0Eint(t) =>
  fprint!(fp, "L0Eint(", t, ")")
| L0Ebtf(t) =>
  fprint!(fp, "L0Ebtf(", t, ")")
| L0Echr(t) =>
  fprint!(fp, "L0Echr(", t, ")")
| L0Eflt(t) =>
  fprint!(fp, "L0Eflt(", t, ")")
| L0Estr(t) =>
  fprint!(fp, "L0Estr(", t, ")")
//
| L0Etop(t) =>
  fprint!(fp, "L0Etop(", t, ")")
//
| L0Evar(v) =>
  fprint!(fp, "L0Evar(", v, ")")
| L0Evknd(i, v) =>
  fprint!(fp, "L0Evknd(", i, ", ", v, ")")
//
| L0Efcon(con) =>
  fprint!(fp, "L0Efcon(", con, ")")
| L0Etcon(con, ti) =>
  fprint!(fp, "L0Etcon(", con, ", ", ti, ")")
//
| L0Efcst(cst) =>
  fprint!(fp, "L0Efcst(", cst, ")")
| L0Etcst(cst, ti) =>
  fprint!(fp, "L0Efcst(", cst, ", ", ti, ")")
//
| L0Edapp(e1, es) =>
  fprint!(fp, "L0Edapp(", e1, ", ", es, ")")
//
| L0Epcon(e, l) =>
  fprint!(fp, "L0Epcon(", e, ", ", l, ")")
| L0Epbox(e, l, i) =>
  fprint!(fp, "L0Epbox(", e, ", ", l, ", ", i, ")")
//
| L0Eproj(e, l, i) =>
  fprint!(fp, "L0Eproj(", e, ", ", l, ", ", i, ")")
| L0Eplft(e, l, i) =>
  fprint!(fp, "L0Eplft(", e, ", ", l, ", ", i, ")")
| L0Epptr(e, l, i) =>
  fprint!(fp, "L0Epptr(", e, ", ", l, ", ", i, ")")
//
| L0Etuple(knd, npf, es) =>
  fprint!(fp, "L0Etype(", knd, ", ", npf, ", ", es, ")")
//
| L0Eassgn(e1, e2) =>
  fprint!(fp, "L0Eassgn(", e1, ", ", e2, ")")
//
| L0Eif0(e1, e2, e3) =>
  fprint!(fp, "L0Eif0(", e1, ", ", e2, ", ", e3, ")")
//
| L0Ecase(i, e, clau) =>
  fprint!(fp, "L0Ecase(", i, ", ", e, ", ", clau, ")")
//
| L0Eseqn(es, e) =>
  fprint!(fp, "L0Eseqn([", es, "], ", e, ")")
//
| L0Elam(hag, e) =>
  fprint!(fp, "L0Elam(", hag, ", ", e, ")")
| L0Efix(v, hag, e) =>
  fprint!(fp, "L0Efix(", v, ", [", hag, "], ", e, ")")
| L0Efun(fdcl, e) =>
  fprint!(fp, "L0Efun([", fdcl, "], ", e, ")")
| L0Eimp_fun(hdc, hag, bod, e) =>
  fprint!(fp, "L0Eimp_fun(", hdc, ", [", hag, "], ", bod, ", ", e, ")")
| L0Eimp_val(hdc, bod, e) =>
  fprint!(fp, "L0Eimp_val(", hdc, ", ", bod, ", ", e, ")")
| L0Elet_val(ldcl, e) =>
  fprint!(fp, "L0Elet_val([", ldcl, "], ", e, ")")
| L0Elet_var(ldcl, e) =>
  fprint!(fp, "L0Elet_var([", ldcl, "], ", e, ")")
//
| L0Etry0(t, e, clau) =>
  fprint!(fp, "L0Etry0(", t, ", ", e, ", ", clau, ")")
//
| L0Eaddr(e) =>
  fprint!(fp, "L0Eaddr(", e, ")")
| L0Eflat(e) =>
  fprint!(fp, "L0Eflat(", e, ")")
| L0Etalf(e) =>
  fprint!(fp, "L0Etalf(", e, ")")
//
| L0Efold(e) =>
  fprint!(fp, "L0Efold(", e, ")")
//
| L0Eeval(i, e) =>
  fprint!(fp, "L0Eeval(", i, ", ", e, ")")
//
| L0Efree(i, e) =>
  fprint!(fp, "L0Efree(", i, ", ", e, ")")
//
| L0Eraise(e) =>
  fprint!(fp, "L0Eraise(", e, ")")
//
| L0Elazy(e) =>
  fprint!(fp, "L0Elazy(", e, ")")
| L0Ellazy(e, es) =>
  fprint!(fp, "L0Ellazy(", e, ", ", es, ")")
//
| L0Enone0() =>
  fprint!(fp, "L0Enone0()")
| L0Enone1(p) =>
  fprint!(fp, "L0Enone1(", p, ")")

(* ****** ****** *)

implement
print_lfundecl(fdcl) =
fprint!(stdout_ref, fdcl)

implement
prerr_lfundecl(fdcl) =
fprint!(stderr_ref, fdcl)

implement
fprint_lfundecl(fp, fdcl) =
let
val LFUNDECL(fdcl) = fdcl
in
case fdcl.hag of
| Some(fdcl_hag) =>
  fprint!
  ( fp
  , "LFUNDECL@{"
  , "nam=", fdcl.nam, ", "
  , "hag=", fdcl_hag, ", "
  , "def=", fdcl.def, "}")
| None() =>
  fprint!
  ( fp
  , "LFUNDECL@{"
  , "nam=", fdcl.nam, ", "
  , "def=", fdcl.def, "}")
end

(* ****** ****** *)

implement
print_lvaldecl(ldcl) =
fprint!(stdout_ref, ldcl)

implement
prerr_lvaldecl(ldcl) =
fprint!(stderr_ref, ldcl)

implement
fprint_lvaldecl(fp, ldcl) =
let
val LVALDECL(ldcl) = ldcl
in
fprint!
( fp
, "LVALDECL@{"
, "pat=", ldcl.pat, ", "
, "def=", ldcl.def, "}")
end

(* ****** ****** *)

implement
print_lvardecl(ldcl) =
fprint!(stdout_ref, ldcl)

implement
prerr_lvardecl(ldcl) =
fprint!(stderr_ref, ldcl)

implement
fprint_lvardecl(fp, ldcl) =
let
val LVARDECL(ldcl) = ldcl
in
fprint!
( fp
, "LVARDECL@{"
, "hdv=", ldcl.hdv, ", "
, "wth=", ldcl.wth, ", "
, "ini=", ldcl.ini, "}")
end

(* ****** ****** *)

implement
print_l0clau(clau) =
fprint!(stdout_ref, clau)

implement
prerr_l0clau(clau) =
fprint!(stderr_ref, clau)

implement
fprint_l0clau(fp, clau) =
case clau of
| L0CLAUpat(gpat) =>
  fprint!(fp, "L0CLAUpat(", gpat, ")")
| L0CLAUexp(gpat, e) =>
  fprint!(fp, "L0CLAUexp(", gpat, ", ", e, ")")

(* ****** ****** *)

implement
print_l0gpat(gpat) =
fprint!(stdout_ref, gpat)

implement
prerr_l0gpat(gpat) =
fprint!(stderr_ref, gpat)

implement
fprint_l0gpat(fp, gpat) =
case gpat of
| L0GPATpat(pat) =>
  fprint!(fp, "L0GPATpat(", pat, ")")
| L0GPATgua(pat, gua) =>
  fprint!(fp, "L0GPATgua(", pat, ", ", gua, ")")

(* ****** ****** *)

implement
print_l0gua(gua) =
fprint!(stdout_ref, gua)

implement
prerr_l0gua(gua) =
fprint!(stderr_ref, gua)

implement
fprint_l0gua(fp, gua) =
case gua of
| L0GUAexp(e) =>
  fprint!(fp, "L0GUAexp(", e, ")")
| L0GUAmat(e, pat) =>
  fprint!(fp, "L0GUAmat(", e, ", ", pat, ")")
