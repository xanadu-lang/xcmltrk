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

abstbox K_tbox = ptr
typedef K = K_tbox

abstbox V_tbox = ptr
typedef V = V_tbox

(* ****** ****** *)

abstbox set_tbox(a:t0p) = ptr
typedef set(a:t0p) = set_tbox(a:t0p)
typedef setlst(a:t0p) = List0(set(a))

fun {a:t0p}set_empty(): set(a)
fun {a:t0p}set_sing(a): set(a)
fun {a:t0p}set_extend(set(a), a): set(a)
fun {a:t0p}set_union(set(a), set(a)): set(a)
fun {a:t0p}setlst_union(setlst(a)): set(a)

overload .extend with set_extend
overload union with set_union
overload union with setlst_union

fun {a:t0p}{b:t0p}set_fold(set(a), b): b
fun {a:t0p}{b:t0p}set_fold$fopr(a, b): b

(* ****** ****** *)

abstbox cache_tbox(a:t0p) = ptr
typedef cache(a:t0p) = cache_tbox(a:t0p)

fun {a:t0p}cache_empty(): cache(a)
fun {a:t0p}cache_sing(int, set(a)): cache(a)
fun {a:t0p}cache_extend(cache(a), int, set(a)): cache(a)
fun {a:t0p}cache_union(cache(a), cache(a)): cache(a)

overload .extend with cache_extend
overload union with cache_union

(* ****** ****** *)

abstbox env_tbox(a:t0p,b:t0p) = ptr
typedef env(a:t0p,b:t0p) = env_tbox(a,b)

fun {a:t0p}{b:t0p}env_empty(): env(a,b)
fun {a:t0p}{b:t0p}env_extend(env(a,b), a, set(b)): env(a,b)
fun {a:t0p}{b:t0p}env_union(env(a,b), env(a,b)): env(a,b)

overload .extend with env_extend
overload union with env_union

fun {a:t0p}{b:t0p}env_lookup(env(a,b), a): set(b)
overload .lookup with env_lookup

(* ****** ****** *)

datatype K_node = // continuation nodes
| EC0NT of (hdvar, c0exp)

datatype V_node = // user nodes
| E0con of (hdcon, setlst(V))
| E0lam of (hfarglst, kdvar, c0exp)
| E0fix of (hdvar, hfarglst, kdvar, c0exp)
| E0imp of (hdcst, hfarglst, kdvar, c0exp)

fun K_make_node(K_node, int(*label*)): K
fun V_make_node(V_node, int(*label*)): V

fun K_get_node(K): K_node
fun V_get_node(V): V_node

overload .node with K_get_node
overload .node with V_get_node

fun K_get_label(K): int
fun V_get_label(V): int

overload .label with K_get_label
overload .label with V_get_label

(* ****** ****** *)

typedef c0env = @{
  hdv= env(hdvar, V)
, hdc= env(hdcst, V)
, kvr= env(kdvar, K)
}

(* ****** ****** *)

fun c0env_hfarg(hfarg, set(V), c0env): c0env
fun c0env_hfarglst(hfarglst, setlst(V), c0env): c0env

fun c0env_h0pat(h0pat, set(V), c0env): c0env
fun c0env_h0patlst(h0patlst, setlst(V), c0env): c0env

(* ****** ****** *)

fun c0fa_c0val(c0val, c0env): set(V)
fun c0fa_c0nt(c0nt, c0env): set(K)

(* ****** ****** *)

fun c0fa(c0exp, c0env): cache(V)

fun 
c0fa_ret // continuation application
( set(K) // continuation set
, set(V) // argument set
, c0env // environment
): cache(V)
 
fun 
c0fa_dapp
( set(V) // function set
, setlst(V) // argument set
, set(K) // continuation set
, c0env // environment set
): cache(V)

fun
c0fa_clau
( set(V)
, c0clau 
, set(K)
): cache(V)
 
(* fun c0fa_C0Eprimop(c0primop, c0setlst, c0set, c0venv): c0cache *)
(* fun c0fa_C0Ecase(c0set, c0setlst, c0set, c0venv): c0cache *)
(* fun c0fa_C0Etry0(c0set, c0setlst, c0set, c0venv): c0cache *)

// NOTE: call cache does not need to record continuation
// TODO: stratified sets for continuations and functions
// TODO: stratified venv for continuations and functions
