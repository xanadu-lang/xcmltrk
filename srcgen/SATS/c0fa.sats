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

abstbox c0cache_tbox
typedef c0cache = c0cache_tbox

(* ****** ****** *) 

fun
c0fa(c0exp): c0cache
