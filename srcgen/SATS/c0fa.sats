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
#staload "./xcps.sats"

(* ****** ****** *)

abstbox kset_tbox = ptr
typedef kset = kset_tbox

abstbox vset_tbox = ptr
typedef vset = vset_tbox

datatype k_elem =
| K0NT of (hdvar, c0exp)

datatype v_elem = 
| V0lam_harg

(* ****** ****** *)

abstbox c0cache_tbox = ptr
typedef c0cache = c0cache_tbox

(* ****** ****** *)

abstbox c0set_tbox = ptr
typedef c0set = c0set_tbox
typedef c0setlst = List0(c0set)

(* ****** ****** *)
 
abstbox c0venv_tbox
typedef c0venv = c0venv_tbox

(* ****** ****** *)

fun c0cache_empty(): c0cache
fun c0cache_extend(c0cache, int(*label*), c0set): c0cache
fun c0cache_lookup(c0cache, int): c0set
fun c0cache_union(c0cache, c0cache): c0cache

overload .extend with c0cache_extend
overload .lookup with c0cache_lookup
overload union with c0cache_union

(* ****** ****** *)

fun c0set_empty(): c0set
fun c0set_sing(c0triv): c0set
fun c0set_extend(c0set, c0triv): c0set
fun c0set_union(c0set, c0set): c0set

overload .extend with c0set_extend
overload union with c0set_union

(* ****** ****** *)
 
fun c0venv_empty(): c0venv
fun c0venv_extend_hdvar(c0venv, hdvar, c0triv): c0venv
fun c0venv_extend_hdcst(c0venv, hdcst, c0triv): c0venv
fun c0venv_lookup_hdvar(c0venv, hdvar): c0set
fun c0venv_lookup_hdcst(c0venv, hdcst): c0set
fun c0venv_lookup_kvar(c0venv, kvar): c0set

overload .extend with c0venv_extend_hdvar
overload .extend with c0venv_extend_hdcst
overload .lookup with c0venv_lookup_hdvar
overload .lookup with c0venv_lookup_hdcst
overload .lookup with c0venv_lookup_kvar

(* ****** ****** *)

fun c0fa_c0val_1(c0val, c0venv): c0set // function position
fun c0fa_c0val_n(c0val, c0venv): c0setlst // argument position
fun c0fa_c0valst(c0valst, c0venv): c0setlst // argument position
fun c0fa_c0nt(c0nt, c0venv): c0set
fun c0fa_c0clau(c0clau, c0venv): c0set
fun c0fa_c0claulst(c0claulst, c0venv): c0setlst
fun c0fa_c0exp(c0exp, c0venv): c0cache 

fun c0fa_C0Eret(c0set, c0set, c0venv): c0cache
fun c0fa_C0Edapp(c0set, c0setlst, c0set, c0venv): c0cache
fun c0fa_C0Eprimop(c0primop, c0setlst, c0set, c0venv): c0cache
fun c0fa_C0Ecase(c0set, c0setlst, c0set, c0venv): c0cache
fun c0fa_C0Etry0(c0set, c0setlst, c0set, c0venv): c0cache

// NOTE: call cache does not need to record continuation
// TODO: stratified sets for continuations and functions
// TODO: stratified venv for continuations and functions
