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

fun fresh_hdvar(string): hdvar

(* ****** ****** *)

abstbox kdvar_tbox = ptr
typedef kdvar = kdvar_tbox

fun fresh_kdvar(string): kdvar
fun tostring_kdvar(kdvar): string
fun tostamp_kdvar(kdvar): int

fun eq_kdvar(kdvar, kdvar): bool
overload = with eq_kdvar

fun print_kdvar(kdvar): void
fun prerr_kdvar(kdvar): void
fun fprint_kdvar(FILEref, kdvar): void

overload print with print_kdvar
overload prerr with prerr_kdvar
overload fprint with fprint_kdvar

(* ****** ****** *)

abstbox c0val_tbox = ptr
typedef c0val = c0val_tbox
typedef c0valst = List0(c0val)
typedef c0valopt = Option(c0val)

fun print_c0val(c0val): void
fun prerr_c0val(c0val): void
fun fprint_c0val(FILEref, c0val): void

overload print with print_c0val
overload prerr with prerr_c0val
overload fprint with fprint_c0val

(* ****** ****** *)

abstbox c0exp_tbox = ptr
typedef c0exp = c0exp_tbox
typedef c0explst = List0(c0exp)
typedef c0expopt = Option(c0exp)

fun print_c0exp(c0exp): void
fun prerr_c0exp(c0exp): void
fun fprint_c0exp(FILEref, c0exp): void

overload print with print_c0exp
overload prerr with prerr_c0exp
overload fprint with fprint_c0exp

(* ****** ****** *)

abstbox c0nt_tbox = ptr
typedef c0nt = c0nt_tbox
typedef c0ntlst = List0(c0nt)

fun print_c0nt(c0nt): void
fun prerr_c0nt(c0nt): void
fun fprint_c0nt(FILEref, c0nt): void

overload print with print_c0nt
overload prerr with prerr_c0nt
overload fprint with fprint_c0nt
  
(* ****** ****** *)
  
abstbox cfundecl_tbox = ptr
typedef cfundecl = cfundecl_tbox
typedef cfundeclst = List0(cfundecl)

fun print_cfundecl(cfundecl): void
fun prerr_cfundecl(cfundecl): void
fun fprint_cfundecl(FILEref, cfundecl): void

overload print with print_cfundecl
overload prerr with prerr_cfundecl
overload fprint with fprint_cfundecl

(* ****** ****** *)

abstbox cvaldecl_tbox = ptr
typedef cvaldecl = cvaldecl_tbox
typedef cvaldeclst = List0(cvaldecl)

fun print_cvaldecl(cvaldecl): void
fun prerr_cvaldecl(cvaldecl): void
fun fprint_cvaldecl(FILEref, cvaldecl): void

overload print with print_cvaldecl
overload prerr with prerr_cvaldecl
overload fprint with fprint_cvaldecl

(* ****** ****** *)

abstbox cvardecl_tbox = ptr
typedef cvardecl = cvardecl_tbox
typedef cvardeclst = List0(cvardecl)

fun print_cvardecl(cvardecl): void
fun prerr_cvardecl(cvardecl): void
fun fprint_cvardecl(FILEref, cvardecl): void

overload print with print_cvardecl
overload prerr with prerr_cvardecl
overload fprint with fprint_cvardecl

(* ****** ****** *)

abstbox c0primop_tbox = ptr
typedef c0primop = c0primop_tbox

fun print_c0primop(c0primop): void
fun prerr_c0primop(c0primop): void
fun fprint_c0primop(FILEref, c0primop): void

overload print with print_c0primop
overload prerr with prerr_c0primop
overload fprint with fprint_c0primop
  
(* ****** ****** *)

abstbox c0clau_tbox = ptr
typedef c0clau = c0clau_tbox
typedef c0claulst = List0(c0clau)

fun print_c0clau(c0clau): void
fun prerr_c0clau(c0clau): void
fun fprint_c0clau(FILEref, c0clau): void

overload print with print_c0clau
overload prerr with prerr_c0clau
overload fprint with fprint_c0clau

(* ****** ****** *)
  
abstbox c0gpat_tbox = ptr
typedef c0gpat = c0gpat_tbox
typedef c0gpatlst = List0(c0gpat)

fun print_c0gpat(c0gpat): void
fun prerr_c0gpat(c0gpat): void
fun fprint_c0gpat(FILEref, c0gpat): void

overload print with print_c0gpat
overload prerr with prerr_c0gpat
overload fprint with fprint_c0gpat

(* ****** ****** *)

abstbox c0gua_tbox = ptr
typedef c0gua = c0gua_tbox
typedef c0gualst = List0(c0gua)

fun print_c0gua(c0gua): void
fun prerr_c0gua(c0gua): void
fun fprint_c0gua(FILEref, c0gua): void

overload print with print_c0gua
overload prerr with prerr_c0gua
overload fprint with fprint_c0gua

(* ****** ****** *)
  
datatype c0val_node =
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
| C0Vlam of // lambda
( hfarglst(*arg*)
, kdvar(*cont*)
, c0exp(*body*))
//
| C0Vfix of // recursive lambda
( hdvar(*fid*)
, hfarglst(*arg*)
, kdvar(*cont*)
, c0exp(*body*))
//
| C0Vnone0 of ()
| C0Vnone1 of (dataptr)

fun c0val_make_node(c0val_node): c0val
fun c0val_get_node(c0val): c0val_node
fun c0val_get_label(c0val): int
 
overload .node with c0val_get_node 
overload .label with c0val_get_label

(* ****** ****** *)

datatype c0exp_node =
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
, c0nt(*cont*))
//
| C0Efun of
( cfundeclst
, c0exp )
//
| C0Eimp_fun of
( hdcst(*nam*)
, hfarglst(*arg*)
, kdvar(*cont*)
, c0exp(*bod*)
, c0exp(*scope*) )
//
| C0Eimp_val of
( hdcst(*nam*)
, c0val(*bod*)
, c0exp(*scope*) )
//
| C0Elet_val of
( cvaldeclst
, c0exp )
//
| C0Elet_var of
( cvardeclst
, c0exp )
//
| C0Eif0 of 
( c0val(*subject*)
, c0nt(*cont*)
, c0nt(*cont*))
//
| C0Ecase of // pattern match
( int
, c0val(*subject*)
, c0claulst(*clause*)
, c0nt(*cont*)) // NOTE: raise exception if unmatched
//
| C0Etry0 of // exception handle
( token
, c0val(*subject*)
, c0claulst(*clause*)
, c0nt(*cont*)) // NOTE: propagate exception if unmatched

fun c0exp_make_node(c0exp_node): c0exp
fun c0exp_get_node(c0exp): c0exp_node
fun c0exp_get_label(c0exp): int

overload .node with c0exp_get_node
overload .label with c0exp_get_label

(* ****** ****** *)

datatype c0nt_node =
| C0HALT of ()
| C0VAR of (kdvar)
//
| C0NT of // continuation
( hdvar(*arg*)
, c0exp(*body*))

fun c0nt_make_node(c0nt_node): c0nt
fun c0nt_get_node(c0nt): c0nt_node
fun c0nt_get_label(c0nt): int

overload .node with c0nt_get_node
overload .label with c0nt_get_label

(* ****** ****** *)
 
datatype cfundecl_node = 
CFUNDECL of @{ 
  nam= hdvar
, hdc= hdcst 
, hag= hfarglstopt  
, knt= kdvar
, def= c0expopt
}

fun cfundecl_make_node(cfundecl_node): cfundecl 
fun cfundecl_get_node(cfundecl): cfundecl_node
fun cfundecl_get_label(cfundecl): int

overload .node with cfundecl_get_node
overload .label with cfundecl_get_label

(* ****** ****** *)

datatype cvaldecl_node = 
CVALDECL of @{ 
  pat= h0pat  
, def= c0valopt
}

fun cvaldecl_make_node(cvaldecl_node): cvaldecl 
fun cvaldecl_get_node(cvaldecl): cvaldecl_node
fun cvaldecl_get_label(cvaldecl): int

overload .node with cvaldecl_get_node
overload .label with cvaldecl_get_label

(* ****** ****** *)

datatype cvardecl_node = 
CVARDECL of @{ 
, hdv= hdvar  
, wth= hdvaropt
, ini= c0valopt
}

fun cvardecl_make_node(cvardecl_node): cvardecl 
fun cvardecl_get_node(cvardecl): cvardecl_node
fun cvardecl_get_label(cvardecl): int

overload .node with cvardecl_get_node
overload .label with cvardecl_get_label

(* ****** ****** *)
  
datatype c0primop_node =
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
| C0Praise of () // unary

fun c0primop_make_node(c0primop_node): c0primop
fun c0primop_get_node(c0primop): c0primop_node
fun c0primop_get_label(c0primop): int

overload .node with c0primop_get_node
overload .label with c0primop_get_label

(* ****** ****** *)
 
datatype c0clau_node = 
| C0CLAU of // c0clau lambda
( c0gpat(*arg*)
, kdvar(*cont*)
, c0exp(*body*))

fun c0clau_make_node(c0clau_node): c0clau
fun c0clau_get_node(c0clau): c0clau_node
fun c0clau_get_label(c0clau): int

overload .node with c0clau_get_node
overload .label with c0clau_get_label

(* ****** ****** *)

datatype c0gpat_node =
| C0GPATpat of (h0pat)
| C0GPATgua of (h0pat, c0gualst)

fun c0gpat_make_node(c0gpat_node): c0gpat
fun c0gpat_get_node(c0gpat): c0gpat_node
fun c0gpat_get_label(c0gpat): int

overload .node with c0gpat_get_node
overload .label with c0gpat_get_label

(* ****** ****** *)

datatype c0gua_node =
| C0GUAexp of (c0val)
| C0GUAmat of (c0val, h0pat)

fun c0gua_make_node(c0gua_node): c0gua
fun c0gua_get_node(c0gua): c0gua_node
fun c0gua_get_label(c0gua): int

overload .node with c0gua_get_node
overload .label with c0gua_get_label

(* ****** ****** *)

typedef c0val_cont = c0val -<cloref1> c0exp
typedef c0valst_cont = c0valst -<cloref1> c0exp
typedef c0valopt_cont = c0valopt -<cloref1> c0exp

typedef cfundecl_cont = cfundecl -<cloref1> c0exp
typedef cfundeclst_cont = cfundeclst -<cloref1> c0exp

typedef cvaldecl_cont = cvaldecl -<cloref1> c0exp
typedef cvaldeclst_cont = cvaldeclst -<cloref1> c0exp

typedef cvardecl_cont = cvardecl -<cloref1> c0exp
typedef cvardeclst_cont = cvardeclst -<cloref1> c0exp

typedef c0clau_cont = c0clau-<cloref1> c0exp
typedef c0claulst_cont = c0claulst -<cloref1> c0exp

typedef c0gpat_cont = c0gpat -<cloref1> c0exp

typedef c0gua_cont = c0gua -<cloref1> c0exp
typedef c0gualst_cont = c0gualst -<cloref1> c0exp

(* ****** ****** *)

fun xcps_l0exp(l0exp, c0val_cont): c0exp
fun xcps_l0explst(l0explst, c0valst_cont): c0exp
fun xcps_l0expopt(l0expopt, c0valopt_cont): c0exp

overload xcps with xcps_l0exp
overload xcps with xcps_l0explst
overload xcps with xcps_l0expopt

(* ****** ****** *)

fun xcps_lfundecl(lfundecl, cfundecl_cont): c0exp
fun xcps_lfundeclst(lfundeclst, cfundeclst_cont): c0exp

overload xcps with xcps_lfundecl
overload xcps with xcps_lfundeclst

(* ****** ****** *)

fun xcps_lvaldecl(lvaldecl, cvaldecl_cont): c0exp
fun xcps_lvaldeclst(lvaldeclst, cvaldeclst_cont): c0exp

overload xcps with xcps_lvaldecl
overload xcps with xcps_lvaldeclst

(* ****** ****** *)

fun xcps_lvardecl(lvardecl, cvardecl_cont): c0exp
fun xcps_lvardeclst(lvardeclst, cvardeclst_cont): c0exp

overload xcps with xcps_lvardecl
overload xcps with xcps_lvardeclst

(* ****** ****** *)

fun xcps_l0clau(l0clau, c0clau_cont): c0exp
fun xcps_l0claulst(l0claulst, c0claulst_cont): c0exp

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
 
fun fresh_c0val(c0val): c0val
fun fresh_c0valst(c0valst): c0valst
 
overload fresh with fresh_c0val
overload fresh with fresh_c0valst
 
(* ****** ****** *)
 
fun fresh_c0exp(c0exp): c0exp
fun fresh_c0explst(c0explst): c0explst

overload fresh with fresh_c0exp
overload fresh with fresh_c0explst

(* ****** ****** *)

fun fresh_c0nt(c0nt): c0nt
fun fresh_c0ntlst(c0ntlst): c0ntlst

overload fresh with fresh_c0nt
overload fresh with fresh_c0ntlst
 
(* ****** ****** *)

fun fresh_cfundecl(cfundecl): cfundecl
fun fresh_cfundeclst(cfundeclst): cfundeclst

overload fresh with fresh_cfundecl
overload fresh with fresh_cfundeclst
 
(* ****** ****** *)

fun fresh_cvaldecl(cvaldecl): cvaldecl
fun fresh_cvaldeclst(cvaldeclst): cvaldeclst

overload fresh with fresh_cvaldecl
overload fresh with fresh_cvaldeclst
 
(* ****** ****** *)

fun fresh_cvardecl(cvardecl): cvardecl
fun fresh_cvardeclst(cvardeclst): cvardeclst

overload fresh with fresh_cvardecl
overload fresh with fresh_cvardeclst
 
(* ****** ****** *)

fun fresh_c0primop(c0primop): c0primop
overload fresh with fresh_c0primop

(* ****** ****** *)

fun fresh_c0clau(c0clau): c0clau
fun fresh_c0claulst(c0claulst): c0claulst

overload fresh with fresh_c0clau
overload fresh with fresh_c0claulst

(* ****** ****** *)

fun fresh_c0gpat(c0gpat): c0gpat
fun fresh_c0gpatlst(c0gpatlst): c0gpatlst

overload fresh with fresh_c0gpat
overload fresh with fresh_c0gpatlst

(* ****** ****** *)

fun fresh_c0gua(c0gua): c0gua
fun fresh_c0gualst(c0gualst): c0gualst

overload fresh with fresh_c0gua
overload fresh with fresh_c0gualst

(* ****** ****** *)

fun to_xcps(l0exp): c0exp
