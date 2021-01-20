#define
XATSOPT_targetloc
"./../../xatsopt/srcgen/xats"
//
#staload "{$XATSOPT}/SATS/lexing0.sats"
#staload "{$XATSOPT}/SATS/xstamp0.sats"
#staload "{$XATSOPT}/SATS/statyp2.sats"
//
#staload "{$XATSOPT}/SATS/intrep0.sats"
//
#staload "./xlambda.sats"

(* ****** ****** *)

typedef hdcstopt = Option(hdcst)
typedef hdcstlst = List0(hdcst)

(* ****** ****** *)
 
abstype kvar = ptr

(* ****** ****** *)

fun fresh_hdvar(string): hdvar

(* ****** ****** *)

fun fresh_kvar(string): kvar
fun eq_kvar(kvar, kvar): bool

fun tostring_kvar(kvar): string
fun tostamp_kvar(kvar): int

overload = with eq_kvar

(* ****** ****** *)
  
datatype c0val =
| C0Vi00 of (int)
| C0Vb00 of (bool)
| C0Vs00 of string
//
| C0Vint of (token)
| C0Vbtf of (token)
| C0Vchr of (token)
| C0Vflt of (token)
| C0Vstr of (token)
//
| C0Vtop of (token)
//
| C0Vvar of (hdvar) // user variable
| C0Vvknd of (int, hdvar) // user variable
//
| C0Vfcon of (hdcon)
| C0Vtcon of (hdcon, htiarg)
//
| C0Vfcst of (hdcst)
| C0Vtcst of (hdcst, htiarg)
//
(* TODO: C0Etimp *)
//
| C0Vfix of // recursive lambda
( cfundeclst) 
//
| C0Vlam_hfarg of // pattern lambda
( hfarglst(*arg*)
, kvar(*cont*)
, c0exp(*body*))
//
| C0Vlam_hdcst of // hfundecl lambda
( hdcstlst(*arg*)
, kvar(*cont*)
, c0exp(*body*))
//
| C0Vlam_hdvar of // hvardecl lambda
( hdvarlst(*arg*)
, kvar(*cont*)
, c0exp(*body*))
//
| C0Vlam_c0gpat of // c0clau lambda
( c0gpat(*arg*)
, kvar(*cont*)
, c0exp(*body*))
//
| C0Vnone0 of ()
| C0Vnone1 of (dataptr)

and c0exp =
| C0Eret of // continuation application
( c0nt(*cont*)
, c0val(*arg*) )
//
| C0Edapp of // function application
( c0val(*fun*)
, c0valst(*arg*)
, c0nt(*cont*))
//
| C0Eprimop of // primitive operation
( c0primop(*primop*)
, c0valst(*arg*)
, c0ntlst(*cont*))
//
| C0Ehalt of ()

and c0nt =
| C0VAR of (kvar)
//
| C0NT of // continuation
( hdvar(*arg*)
, c0exp(*body*))
 
and cfundecl = 
CFUNDECL of  
@{ nam= hdvar
 , hag= hfarglstopt  
 , knt= kvar
 , def= c0expopt
}
  
and c0primop =
| C0Ppcon of (label) // unary
| C0Ppbox of (label, int) // unary
//
| C0Pproj of (label, int) // unary
| C0Pplft of (label, int) // unary
| C0Ppptr of (label, int) // unary
//
| C0Ptuple of (int, int) // Nnary
//
| C0Passgn of () // binary
//
| C0Pif0 of () // trinary
//
| C0Pcase of (int) // N+1nary
//
| C0Paddr of () // unary
| C0Pflat of () // unary
| C0Ptalf of () // unary
//
| C0Pfold of () // unary
//
| C0Peval of (int) // unary
//
| C0Pfree of (int) // unary
//
| C0Plazy of () // unary
| C0Pllazy of () // N+1nary
//
(* TODO: C0Etry0 *)
//
(* TODO: C0Eraise *)
//

and c0gpat =
| C0GPATpat of (h0pat)
| C0GPATgua of (h0pat, c0gualst)

and c0gua =
| C0GUAexp of (c0val)
| C0GUAmat of (c0val, h0pat)

where cfundeclst = List0(cfundecl)
and c0explst = List0(c0exp)
and c0valst = List0(c0val)
and c0ntlst = List0(c0nt)
and c0gpatlst = List0(c0gpat)
and c0gualst = List0(c0gua)

and c0expopt = Option(c0exp)

(* ****** ****** *)

fun print_kvar(kvar): void
fun prerr_kvar(kvar): void
fun fprint_kvar(FILEref, kvar): void

overload print with print_kvar
overload prerr with prerr_kvar
overload fprint with fprint_kvar

(* ****** ****** *)

fun print_c0val(c0val): void
fun prerr_c0val(c0val): void
fun fprint_c0val(FILEref, c0val): void

overload print with print_c0val
overload prerr with prerr_c0val
overload fprint with fprint_c0val

(* ****** ****** *)

fun print_c0exp(c0exp): void
fun prerr_c0exp(c0exp): void
fun fprint_c0exp(FILEref, c0exp): void

overload print with print_c0exp
overload prerr with prerr_c0exp
overload fprint with fprint_c0exp

(* ****** ****** *)

fun print_c0nt(c0nt): void
fun prerr_c0nt(c0nt): void
fun fprint_c0nt(FILEref, c0nt): void

overload print with print_c0nt
overload prerr with prerr_c0nt
overload fprint with fprint_c0nt

(* ****** ****** *)

fun print_cfundecl(cfundecl): void
fun prerr_cfundecl(cfundecl): void
fun fprint_cfundecl(FILEref, cfundecl): void

overload print with print_cfundecl
overload prerr with prerr_cfundecl
overload fprint with fprint_cfundecl

(* ****** ****** *)

fun print_c0primop(c0primop): void
fun prerr_c0primop(c0primop): void
fun fprint_c0primop(FILEref, c0primop): void

overload print with print_c0primop
overload prerr with prerr_c0primop
overload fprint with fprint_c0primop

(* ****** ****** *)

fun print_c0gpat(c0gpat): void
fun prerr_c0gpat(c0gpat): void
fun fprint_c0gpat(FILEref, c0gpat): void

overload print with print_c0gpat
overload prerr with prerr_c0gpat
overload fprint with fprint_c0gpat

(* ****** ****** *)

fun print_c0gua(c0gua): void
fun prerr_c0gua(c0gua): void
fun fprint_c0gua(FILEref, c0gua): void

overload print with print_c0gua
overload prerr with prerr_c0gua
overload fprint with fprint_c0gua

(* ****** ****** *)

typedef c0exp_cont = c0val -<cloref1> c0exp
typedef c0explst_cont = c0valst -<cloref1> c0exp

typedef cfundecl_cont = cfundecl -<cloref1> c0exp
typedef cfundeclst_cont = cfundeclst -<cloref1> c0exp

typedef c0gpat_cont = c0gpat -<cloref1> c0exp

typedef c0gua_cont = c0gua -<cloref1> c0exp
typedef c0gualst_cont = c0gualst -<cloref1> c0exp

(* ****** ****** *)

fun xcps_l0exp(l0exp, c0exp_cont): c0exp
fun xcps_l0explst(l0explst, c0explst_cont): c0exp

overload xcps with xcps_l0exp
overload xcps with xcps_l0explst

(* ****** ****** *)

fun xcps_lfundecl(lfundecl, cfundecl_cont): c0exp
fun xcps_lfundeclst(lfundeclst, cfundeclst_cont): c0exp

overload xcps with xcps_lfundecl
overload xcps with xcps_lfundeclst

(* ****** ****** *)

fun xcps_l0clau(l0clau, c0exp_cont): c0exp
fun xcps_l0claulst(l0claulst, c0exp_cont): c0explst

overload xcps with xcps_l0clau
overload xcps with xcps_l0claulst

(* ****** ****** *)

fun xcps_l0gpat(l0gpat, c0gpat_cont): c0exp
overload xcps with xcps_l0gpat

(* ****** ****** *)
 
fun xcps_l0gua(l0gua, c0gua_cont): c0exp
fun xcps_l0gualst(l0gualst, c0gualst_cont): c0exp

overload xcps with xcps_l0gua
overload xcps with xcps_l0gualst

(* ****** ****** *)


