#define
XATSOPT_targetloc
"./../../xatsopt/srcgen/xats"
//
#staload "{$XATSOPT}/SATS/lexing0.sats"
#staload "{$XATSOPT}/SATS/xstamp0.sats"
#staload "{$XATSOPT}/SATS/statyp2.sats"
//
#staload "{$XATSOPT}/SATS/intrep0.sats"

(* ****** ****** *)

datatype 
l0exp =
| L0Ei00 of (int)
| L0Eb00 of (bool)
| L0Es00 of string
//
| L0Eint of (token)
| L0Ebtf of (token)
| L0Echr of (token)
| L0Eflt of (token)
| L0Estr of (token)
//
| L0Etop of (token)
//
| L0Evar of (hdvar)
| L0Evknd of (int, hdvar)
//
| L0Efcon of (hdcon)
| L0Etcon of (hdcon, htiarg)
//
| L0Efcst of (hdcst)
| L0Etcst of (hdcst, htiarg)
//
(* NOTE: convert L0Etimp *)
//
| L0Edapp of (l0exp, l0explst)
//
| L0Epcon of (l0exp, label) // NOTE: primop
| L0Epbox of (l0exp, label, int) // NOTE: primop
//
| L0Eproj of (l0exp, label, int) // NOTE: primop
| L0Eplft of (l0exp, label, int) // NOTE: primop
| L0Epptr of (l0exp, label, int) // NOTE: primop
//
(* NOTE: convert H0Elet *)
//
(* NOTE: convert H0Eseqn *)
//
| L0Etuple of (int, int, l0explst) // NOTE: primop
//
| L0Eassgn of (l0exp, l0exp) // NOTE: primop
//
| L0Eif0 of (l0exp, l0exp, l0expopt) // NOTE: primop
//
| L0Ecase of (int, l0exp, l0claulst) // NOTE: primop
//
| L0Eseqn of (l0explst, l0exp)
//
| L0Elam of (hfarglst, l0exp)
| L0Efix of (hdvar, hfarglst, l0exp)
| L0Efun of (lfundeclst, l0exp)
| L0Eimp of (hdcst, l0exp, l0exp)
| L0Eval of (lvaldeclst, l0exp)
| L0Evar of (lvardeclst, l0exp) 
//
| L0Etry0 of (token, l0exp, l0claulst) // NOTE: primop
//
| L0Eaddr of (l0exp) // NOTE: primop
| L0Eflat of (l0exp) // NOTE: primop
| L0Etalf of (l0exp) // NOTE: primop
//
| L0Efold of (l0exp) // NOTE: primop
//
| L0Eeval of (int, l0exp) // NOTE: primop
//
| L0Efree of (int, l0exp) // NOTE: primop
//
| L0Eraise of (l0exp) // NOTE: primop
//
| L0Elazy of (l0exp) // NOTE: primop
| L0Ellazy of (l0exp, l0explst) // NOTE: primop
//
| L0Enone0 of ()
| L0Enone1 of (dataptr)

and lfundecl = 
LFUNDECL of @{ 
  nam= hdvar
, hdc= hdcst
, hag= hfarglstopt
, def= l0expopt 
}

and lvaldecl =
LVALDECL of @{
  pat= h0pat  
, def= l0expopt
}

and lvardecl =
LVARDECL of @{
, hdv= hdvar  
, wth= hdvaropt
, ini= l0expopt
}

and l0clau =
| L0CLAUpat of (l0gpat)
| L0CLAUexp of (l0gpat, l0exp)

and l0gpat =
| L0GPATpat of (h0pat)
| L0GPATgua of (h0pat, l0gualst)

and l0gua =
| L0GUAexp of (l0exp)
| L0GUAmat of (l0exp, h0pat)

where l0explst = List0(l0exp)
and l0expopt = Option(l0exp)
and lfundeclst = List0(lfundecl)
and lvaldeclst = List0(lvaldecl)
and lvardeclst = List0(lvardecl)
and l0claulst = List0(l0clau)
and l0gualst = List0(l0gua)

(* ****** ****** *)

fun print_l0exp(l0exp): void
fun prerr_l0exp(l0exp): void
fun fprint_l0exp(FILEref, l0exp): void

overload print with print_l0exp
overload prerr with prerr_l0exp
overload fprint with fprint_l0exp
 
(* ****** ****** *)

fun print_lfundecl(lfundecl): void
fun prerr_lfundecl(lfundecl): void
fun fprint_lfundecl(FILEref, lfundecl): void

overload print with print_lfundecl
overload prerr with prerr_lfundecl
overload fprint with fprint_lfundecl

(* ****** ****** *)

fun print_lvaldecl(lvaldecl): void
fun prerr_lvaldecl(lvaldecl): void
fun fprint_lvaldecl(FILEref, lvaldecl): void

overload print with print_lvaldecl
overload prerr with prerr_lvaldecl
overload fprint with fprint_lvaldecl

(* ****** ****** *)

fun print_lvardecl(lvardecl): void
fun prerr_lvardecl(lvardecl): void
fun fprint_lvardecl(FILEref, lvardecl): void

overload print with print_lvardecl
overload prerr with prerr_lvardecl
overload fprint with fprint_lvardecl

(* ****** ****** *)

fun print_l0clau(l0clau): void
fun prerr_l0clau(l0clau): void
fun fprint_l0clau(FILEref, l0clau): void

overload print with print_l0clau
overload prerr with prerr_l0clau
overload fprint with fprint_l0clau

(* ****** ****** *)
 
fun print_l0gpat(l0gpat): void
fun prerr_l0gpat(l0gpat): void
fun fprint_l0gpat(FILEref, l0gpat): void

overload print with print_l0gpat
overload prerr with prerr_l0gpat
overload fprint with fprint_l0gpat

(* ****** ****** *)

fun print_l0gua(l0gua): void
fun prerr_l0gua(l0gua): void
fun fprint_l0gua(FILEref, l0gua): void

overload print with print_l0gua
overload prerr with prerr_l0gua
overload fprint with fprint_l0gua

(* ****** ****** *)

fun 
xlambda_h0exp(h0exp): l0exp
fun 
xlambda_h0explst(h0explst): l0explst
fun 
xlambda_h0expopt(h0expopt): l0expopt

overload xlambda with xlambda_h0exp
overload xlambda with xlambda_h0explst
overload xlambda with xlambda_h0expopt

(* ****** ****** *)

fun
xlambda_h0dcl(h0dcl, l0exp): l0exp
fun
xlambda_h0dclist(h0dclist, l0exp): l0exp

overload xlambda with xlambda_h0dcl
overload xlambda with xlambda_h0dclist

(* ****** ****** *)

fun
xlambda_h0clau(h0clau): l0clau
fun
xlambda_h0claulst(h0claulst): l0claulst

overload xlambda with xlambda_h0clau 
overload xlambda with xlambda_h0claulst

(* ****** ****** *)

fun
xlambda_h0gpat(h0gpat): l0gpat

overload xlambda with xlambda_h0gpat

(* ****** ****** *)

fun
xlambda_h0gua(h0gua): l0gua
fun
xlambda_h0gualst(h0gualst): l0gualst

overload xlambda with xlambda_h0gua
overload xlambda with xlambda_h0gualst

(* ****** ****** *)

fun
to_xlambda(h0dclist): l0exp
