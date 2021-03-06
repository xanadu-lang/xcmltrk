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
#staload "./../../SATS/xlambda.sats"
#staload "./../../SATS/xcps.sats"
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

typedef 
kdvar_struct = @{
  nam= string  
, stamp= int
}

absimpl 
kdvar_tbox = ref(kdvar_struct)

local

val kdvar_stamp = ref<int>(0)

in

implement
fresh_kdvar(s) =
let
val n = kdvar_stamp[]
val () = kdvar_stamp[] := n + 1
in
ref<kdvar_struct>
@{
  nam= s  
, stamp= n
}
end

implement
tostring_kdvar(k) =
k->nam

implement
tostamp_kdvar(k) =
k->stamp

implement 
eq_kdvar(k1, k2) =
k1->stamp = k2->stamp
 
end

(* ****** ****** *)

absimpl 
c0val_tbox = '{
  node= c0val_node  
, label= int
}

implement
c0val_make_node(node) = '{
  node= node
, label= ~1
}

implement
c0val_get_node(x) =
x.node

implement
c0val_get_label(x) =
x.label

(* ****** ****** *)

absimpl
c0exp_tbox = '{
  node= c0exp_node  
, label= int
}

implement
c0exp_make_node(node) = '{
  node= node
, label= ~1
}

implement
c0exp_get_node(x) =
x.node

implement
c0exp_get_label(x) =
x.label

(* ****** ****** *)

absimpl 
c0nt_tbox = '{
  node= c0nt_node  
, label= int
}

implement
c0nt_make_node(node) = '{
  node= node
, label= ~1
}

implement
c0nt_get_node(x) =
x.node

implement
c0nt_get_label(x) =
x.label
  
(* ****** ****** *)

absimpl 
cfundecl_tbox = '{
  node= cfundecl_node  
, label= int
}

implement
cfundecl_make_node(node) = '{
  node= node
, label= ~1
}

implement
cfundecl_get_node(x) =
x.node

implement
cfundecl_get_label(x) =
x.label
  
(* ****** ****** *)

absimpl 
cvaldecl_tbox = '{
  node= cvaldecl_node  
, label= int
}

implement
cvaldecl_make_node(node) = '{
  node= node
, label= ~1
}

implement
cvaldecl_get_node(x) =
x.node

implement
cvaldecl_get_label(x) =
x.label
  
(* ****** ****** *)

absimpl 
cvardecl_tbox = '{
  node= cvardecl_node  
, label= int
}

implement
cvardecl_make_node(node) = '{
  node= node
, label= ~1
}

implement
cvardecl_get_node(x) =
x.node

implement
cvardecl_get_label(x) =
x.label
  
(* ****** ****** *)

absimpl 
c0primop_tbox =  '{
  node= c0primop_node  
, label= int
}

implement
c0primop_make_node(node) = '{
  node= node
, label= ~1
}

implement
c0primop_get_node(x) =
x.node

implement
c0primop_get_label(x) =
x.label

(* ****** ****** *)

absimpl 
c0clau_tbox = '{
  node= c0clau_node  
, label= int
}

implement
c0clau_make_node(node) = '{
  node= node
, label= ~1
}

implement
c0clau_get_node(x) =
x.node

implement
c0clau_get_label(x) =
x.label

(* ****** ****** *)

absimpl 
c0gpat_tbox = '{
  node= c0gpat_node  
, label= int
}

implement
c0gpat_make_node(node) = '{
  node= node
, label= ~1
}

implement
c0gpat_get_node(x) =
x.node

implement
c0gpat_get_label(x) =
x.label

(* ****** ****** *)

absimpl 
c0gua_tbox = '{
  node= c0gua_node  
, label= int
}

implement
c0gua_make_node(node) = '{
  node= node
, label= ~1
}

implement
c0gua_get_node(x) =
x.node

implement
c0gua_get_label(x) =
x.label

(* ****** ****** *)
  
implement
xcps_l0exp(e, c) =
case e of
| L0Ei00(x) => c(c0val_make_node(C0Vi00(x)))
| L0Eb00(x) => c(c0val_make_node(C0Vb00(x)))
| L0Es00(x) => c(c0val_make_node(C0Vs00(x)))
//
| L0Eint(t) => c(c0val_make_node(C0Vint(t)))
| L0Ebtf(t) => c(c0val_make_node(C0Vbtf(t)))
| L0Echr(t) => c(c0val_make_node(C0Vchr(t)))
| L0Eflt(t) => c(c0val_make_node(C0Vflt(t)))
| L0Estr(t) => c(c0val_make_node(C0Vstr(t)))
//
| L0Etop(t) => c(c0val_make_node(C0Vtop(t)))
//
| L0Evar(v) => c(c0val_make_node(C0Vvar(v)))
| L0Evknd(i, v) => c(c0val_make_node(C0Vvknd(i, v)))
//
| L0Efcon(hc) => c(c0val_make_node(C0Vfcon(hc)))
| L0Etcon(hc, ht) => c(c0val_make_node(C0Vtcon(hc, ht)))
//
| L0Efcst(hc) => c(c0val_make_node(C0Vfcst(hc)))
| L0Etcst(hc, ht) => c(c0val_make_node(C0Vtcst(hc, ht)))
//
| L0Edapp(e, es) =>
  xcps(e, lam(e) =<cloref1> 
  xcps(es, lam(es) =<cloref1> 
  let
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  in
  c0exp_make_node(C0Edapp(e, es, k))
  end))
//
| L0Epcon(e, l) =>
  xcps(e, lam(e) =<cloref1> 
  let
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  //
  val es = 
  list_vt2t(list_make_sing<c0val>(e))
  //
  val p = c0primop_make_node(C0Ppcon(l))
  in
  c0exp_make_node(C0Eprimop(p, es, k))
  end)
| L0Epbox(e, l, i) =>
  xcps(e, lam(e) =<cloref1> 
  let
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  //
  val es = 
  list_vt2t(list_make_sing<c0val>(e))
  //
  val p = c0primop_make_node(C0Ppbox(l, i))
  in
  c0exp_make_node(C0Eprimop(p, es, k))
  end)
//
| L0Eproj(e, l, i) =>
  xcps(e, lam(e) =<cloref1> 
  let
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  //
  val es = 
  list_vt2t(list_make_sing<c0val>(e))
  //
  val p = c0primop_make_node(C0Pproj(l, i))
  in
  c0exp_make_node(C0Eprimop(p, es, k))
  end)
| L0Eplft(e, l, i) =>
  xcps(e, lam(e) =<cloref1> 
  let
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  //
  val es = 
  list_vt2t(list_make_sing<c0val>(e))
  //
  val p = c0primop_make_node(C0Pplft(l, i))
  in
  c0exp_make_node(C0Eprimop(p, es, k))
  end)
| L0Epptr(e, l, i) =>
  xcps(e, lam(e) =<cloref1> 
  let
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  //
  val es = 
  list_vt2t(list_make_sing<c0val>(e))
  //
  val p = c0primop_make_node(C0Ppptr(l, i))
  in
  c0exp_make_node(C0Eprimop(p, es, k))
  end)
//
| L0Etuple(i, j, es) =>
  xcps(es, lam(es) =<cloref1> 
  let
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  //
  val p = c0primop_make_node(C0Ptuple(i, j))
  in
  c0exp_make_node(C0Eprimop(p, es, k))
  end)
//
| L0Eassgn(e1, e2) =>
  xcps(e1, lam(e1) =<cloref1> 
  xcps(e2, lam(e2) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  //
  val es = 
  list_cons(e1, list_cons(e2, list_nil()))
  //
  val p = c0primop_make_node(C0Passgn())
  in
  c0exp_make_node(C0Eprimop(p, es, k))
  end))
//
| L0Eif0(e1, e2, e3) =>
  xcps_l0exp(e1, lam(e1) =<cloref1>
  let
  val t = fresh_hdvar("t")
  val tbod = xcps(e2, lam(e2) =<cloref1> c(e2))
  val kt = c0nt_make_node(C0NT(t,  tbod))
  //
  val f = fresh_hdvar("f")
  val fbod = xcps_l0expopt(e3, lam(e3) =<cloref1> 
  case e3 of
  | Some(e3) => c(e3)
  | None() => c(c0val_make_node(C0Vnone0())))
  val kf = c0nt_make_node(C0NT(f,  fbod))
  in
  c0exp_make_node(C0Eif0(e1, kt, kf)) 
  end)
//
| L0Ecase(i, e, cs) =>
  xcps(e, lam(e) =<cloref1>
  xcps(cs, lam(cs) =<cloref1>
  let 
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  in
  c0exp_make_node(C0Ecase(i, e, cs, k))
  end))
//
| L0Eseqn(es, e) =>
  xcps(es, lam(es) =<cloref1> 
  xcps(e, lam(e) =<cloref1> c(e)))
//
| L0Elam(hag, e) =>
  let
  val k = fresh_kdvar("k")
  val bod =
  xcps(e, lam(e) =<cloref1> 
  c0exp_make_node(C0Eret(c0nt_make_node(C0VAR(k)), e)))
  in
  c(c0val_make_node(C0Vlam(hag, k, bod)))
  end
| L0Efix(fid, hag, e) =>
  let
  val k = fresh_kdvar("k")
  val bod =
  xcps(e, lam(e) =<cloref1> 
  c0exp_make_node(C0Eret(c0nt_make_node(C0VAR(k)), e)))
  in
  c(c0val_make_node(C0Vfix(fid, hag, k, bod)))
  end
| L0Efun(fdcl, e) =>
  xcps(fdcl, lam(fdcl) =<cloref1> 
  c0exp_make_node(C0Efun(fdcl, xcps(e, c))))
| L0Eimp(hdc, bod, e) =>
  xcps(bod, lam(bod) =<cloref1>
  c0exp_make_node(C0Eimp(hdc, bod, xcps(e, c))))
| L0Eval(ldcl, e) =>
  xcps(ldcl, lam(ldcl) =<cloref1> 
  c0exp_make_node(C0Eval(ldcl, xcps(e, c))))
| L0Evar(ldcl, e) =>
  xcps(ldcl, lam(ldcl) =<cloref1> 
  c0exp_make_node(C0Evar(ldcl, xcps(e, c))))
//
| L0Etry0(t, e, cs) =>
  xcps(e, lam(e) =<cloref1>
  xcps(cs, lam(cs) =<cloref1>
  let 
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  in
  c0exp_make_node(C0Etry0(t, e, cs, k))
  end))
//
| L0Eaddr(e) =>
  xcps(e, lam(e) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  //
  val p = c0primop_make_node(C0Paddr())
  in
  c0exp_make_node(C0Eprimop(p, e, k))
  end)
| L0Eflat(e) =>
  xcps(e, lam(e) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  //
  val p = c0primop_make_node(C0Pflat())
  in
  c0exp_make_node(C0Eprimop(p, e, k))
  end)
| L0Etalf(e) =>
  xcps(e, lam(e) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  //
  val p = c0primop_make_node(C0Ptalf())
  in
  c0exp_make_node(C0Eprimop(p, e, k))
  end)
//
| L0Efold(e) =>
  xcps(e, lam(e) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  //
  val p = c0primop_make_node(C0Pfold())
  in
  c0exp_make_node(C0Eprimop(p, e, k))
  end)
//
| L0Eeval(i, e) =>
  xcps(e, lam(e) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  //
  val p = c0primop_make_node(C0Peval(i))
  in
  c0exp_make_node(C0Eprimop(p, e, k))
  end)
//
| L0Efree(i, e) =>
  xcps(e, lam(e) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  //
  val p = c0primop_make_node(C0Pfree(i))
  in
  c0exp_make_node(C0Eprimop(p, e, k))
  end)
//
| L0Eraise(e) =>
  xcps(e, lam(e) =<cloref1>
  let 
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x, c(xbod)))
  //
  val e =
  list_vt2t(list_make_sing<c0val>(e))
  //
  val p = c0primop_make_node(C0Praise())
  in
  c0exp_make_node(C0Eprimop(p, e, k))
  end)
//
| L0Elazy(e) =>
  xcps(e, lam(e) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  //
  val e = 
  list_vt2t(list_make_sing<c0val>(e))
  //
  val p = c0primop_make_node(C0Plazy())
  in
  c0exp_make_node(C0Eprimop(p, e, k))
  end)
| L0Ellazy(e, es) =>
  xcps_l0exp(e, lam(e) =<cloref1>
  xcps_l0explst(es, lam(es) =<cloref1>
  let
  val x = fresh_hdvar("x")
  val xbod = c0val_make_node(C0Vvar(x))
  val k = c0nt_make_node(C0NT(x,  c(xbod)))
  //
  val es = list_cons(e, es)
  //
  val p = c0primop_make_node(C0Pllazy())
  in
  c0exp_make_node(C0Eprimop(p, es, k))
  end))
//
| L0Enone0() => c(c0val_make_node(C0Vnone0()))
| L0Enone1(p) => c(c0val_make_node(C0Vnone1(p)))

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
xcps_l0expopt(e, c) =
case e of
| Some(e) =>
  xcps(e, lam(e) =<cloref1> c(Some(e)))
| None() => c(None())

(* ****** ****** *)

implement
xcps_lfundeclst(fdclst, c) =
case fdclst of
| list_cons(fdcl, fdclst) =>
  let 
  val LFUNDECL(fdcl) = fdcl
  val k = fresh_kdvar("k")
  in
  case (fdcl.hag, fdcl.def) of
  | (Some(list_nil()), Some(def)) =>
    xcps(def, lam(def) =<cloref1> 
    let 
    val fdcl =
    cfundecl_make_node(CFUNDECL@{
      hdc= fdcl.hdc
    , def= def
    })
    in
    xcps_lfundeclst(fdclst, lam(fdclst) =<cloref1>
    c(list_cons(fdcl, fdclst)))
    end)
  //
  | (Some(hag), Some(def)) =>
    xcps(L0Efix(fdcl.nam, hag, def), lam(def) =<cloref1> 
    let
    val fdcl = 
    cfundecl_make_node(CFUNDECL@{
      hdc= fdcl.hdc
    , def= def
    })
    in
    xcps_lfundeclst(fdclst, lam(fdclst) =<cloref1>
    c(list_cons(fdcl, fdclst)))
    end)
  //
  | (Some(hag), None()) =>
    xcps_lfundeclst(fdclst, c)
  //
  | (None(), Some(def)) =>
    xcps(def, lam(def) =<cloref1> 
    let 
    val fdcl =
    cfundecl_make_node(CFUNDECL@{
      hdc= fdcl.hdc
    , def= def
    })
    in
    xcps_lfundeclst(fdclst, lam(fdclst) =<cloref1>
    c(list_cons(fdcl, fdclst)))
    end)
  //
  | (None(), None()) =>
    xcps_lfundeclst(fdclst, c)
  end
| list_nil() => c(list_nil())

(* ****** ****** *)

implement
xcps_lvaldecl(ldcl, c) =
let
val LVALDECL(ldcl) = ldcl
in
xcps(ldcl.def, lam(def) =<cloref1> 
c(cvaldecl_make_node(CVALDECL@{
  pat= ldcl.pat
, def= def
})))
end

(* ****** ****** *)

implement
xcps_lvaldeclst(ldclst, c) =
case ldclst of
| list_cons(ldcl, ldclst) =>
  xcps_lvaldecl(ldcl, lam(ldcl) =<cloref1>
  xcps_lvaldeclst(ldclst, lam(ldclst) =<cloref1>
  c(list_cons(ldcl, ldclst))))
| list_nil() => c(list_nil())

(* ****** ****** *)

implement
xcps_lvardecl(ldcl, c) =
let
val LVARDECL(ldcl) = ldcl
in
xcps(ldcl.ini, lam(ini) =<cloref1> 
c(cvardecl_make_node(CVARDECL@{
  hdv= ldcl.hdv
, wth= ldcl.wth
, ini= ini
})))
end

(* ****** ****** *)

implement
xcps_lvardeclst(ldclst, c) =
case ldclst of
| list_cons(ldcl, ldclst) =>
  xcps_lvardecl(ldcl, lam(ldcl) =<cloref1>
  xcps_lvardeclst(ldclst, lam(ldclst) =<cloref1>
  c(list_cons(ldcl, ldclst))))
| list_nil() => c(list_nil())

(* ****** ****** *)

implement
xcps_l0clau(clau, c) =
case clau of
| L0CLAUpat(gpat) =>
  let
  val k = fresh_kdvar("k")
  val bod = 
  c0exp_make_node(C0Eret
  (c0nt_make_node(C0VAR(k)), c0val_make_node(C0Vnone0())))
  in
  xcps(gpat, lam(gpat) =<cloref1>
  c(c0clau_make_node(C0CLAU(gpat, k, bod))))
  end
| L0CLAUexp(gpat, bod) =>
  let
  val k = fresh_kdvar("k")
  val bod = 
  xcps(bod, lam(e) =<cloref1>
  c0exp_make_node(C0Eret(c0nt_make_node(C0VAR(k)), e)))
  in
  xcps(gpat, lam(gpat) =<cloref1>
  c(c0clau_make_node(C0CLAU(gpat, k, bod))))
  end

(* ****** ****** *)

implement
xcps_l0claulst(claulst, c) =
case claulst of
| list_cons(clau, claulst) =>
  xcps_l0clau(clau, lam(clau) => 
  xcps_l0claulst(claulst, lam(claulst) =>
  c(list_cons(clau, claulst))))
| list_nil() =>
  c(list_nil())

(* ****** ****** *)

implement
xcps_l0gpat(gpat, c) =
case gpat of
| L0GPATpat(pat) => c(c0gpat_make_node(C0GPATpat(pat)))
| L0GPATgua(pat, gua) =>
  xcps(gua, lam(gua) =<cloref1> 
  c(c0gpat_make_node(C0GPATgua(pat, gua))))

(* ****** ****** *)

implement
xcps_l0gua(gua, c) =
case gua of
| L0GUAexp(e) =>
  xcps(e, lam(e) =<cloref1> 
  c(c0gua_make_node(C0GUAexp(e))))
| L0GUAmat(e, pat) =>
  xcps(e, lam(e) =<cloref1>
  c(c0gua_make_node(C0GUAmat(e, pat))))

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

(* ****** ****** *)

local

val label_stamp = ref<int>(0)

in

fun
fresh_label(): int =
let
val n = label_stamp[]
val () = label_stamp[] := n + 1
in
n
end

end

(* ****** ****** *)
  
implement
fresh_c0val(v) =
let  
val node =
case v.node() of
| C0Vi00(_) => v.node()
| C0Vb00(_) => v.node()
| C0Vs00(_) => v.node()
//
| C0Vint(_) => v.node()
| C0Vbtf(_) => v.node()
| C0Vchr(_) => v.node()
| C0Vflt(_) => v.node()
| C0Vstr(_) => v.node()
//
| C0Vtop(_) => v.node()
//
| C0Vvar(_) => v.node()
| C0Vvknd(_,_) => v.node()
//
| C0Vfcon(_) => v.node()
| C0Vtcon(_,_) => v.node()
//
| C0Vfcst(_) => v.node()
| C0Vtcst(_,_) => v.node()
//
| C0Vlam(hag, k, e) =>
  C0Vlam(hag, k, fresh(e))
//
| C0Vfix(fid, hag, k, e) =>
  C0Vfix(fid, hag, k, fresh(e))
//
| C0Vnone0() => v.node()
| C0Vnone1(_) => v.node()
in
'{ node= node, label= fresh_label() }
end

implement
fresh_c0valst(vs) =
list_vt2t(list_map<c0val><c0val>(vs))
where
{
implement
list_map$fopr<c0val><c0val>(v) =
fresh(v)
}

(* ****** ****** *)

implement
fresh_c0exp(e) =
let
val node =
case e.node() of
| C0Eret(k, v) =>
  C0Eret(fresh(k), fresh(v))
| C0Edapp(v, vs, k) =>
  C0Edapp(fresh(v), fresh(vs), fresh(k))
| C0Eprimop(p, vs, ks) =>
  C0Eprimop(fresh(p), fresh(vs), fresh(ks))
| C0Efun(fdcl, e) =>
  C0Efun(fresh(fdcl), fresh(e))
| C0Eimp(nam, bod, e) =>
  C0Eimp(nam, fresh(bod), fresh(e))
| C0Eval(ldcl, e) =>
  C0Eval(fresh(ldcl), fresh(e))
| C0Evar(ldcl, e) =>
  C0Evar(fresh(ldcl), fresh(e))
| C0Eif0(v, k1, k2) =>
  C0Eif0(fresh(v), fresh(k1), fresh(k2))
| C0Ecase(i, v, cs, k) =>
  C0Ecase(i, fresh(v), fresh(cs), fresh(k))
| C0Etry0(t, v, cs, k) =>
  C0Etry0(t, fresh(v), fresh(cs), fresh(k))
in
'{ node= node, label= fresh_label() }
end

implement
fresh_c0explst(es) =
list_vt2t(list_map<c0exp><c0exp>(es))
where
{
implement
list_map$fopr<c0exp><c0exp>(e) =
fresh(e)
}

(* ****** ****** *)
implement
fresh_c0nt(k) =
let
val node =
case k.node() of
| C0HALT() => k.node()
| C0VAR(_) => k.node()
| C0NT(v, e) =>
  C0NT(v, fresh(e))
in
'{ node= node, label= fresh_label() }
end

implement
fresh_c0ntlst(ks) =
list_vt2t(list_map<c0nt><c0nt>(ks))
where
{
implement
list_map$fopr<c0nt><c0nt>(k) =
fresh(k)
}

(* ****** ****** *)

implement
fresh_cfundecl(fdcl) =
let
val node =
case fdcl.node() of
| CFUNDECL(fdcl) =>
  CFUNDECL(@{
    hdc= fdcl.hdc
  , def= fresh(fdcl.def)
  })
in
'{ node= node, label= fresh_label() }
end

implement
fresh_cfundeclst(fdcl) =
list_vt2t(list_map<cfundecl><cfundecl>(fdcl))
where
{
implement
list_map$fopr<cfundecl><cfundecl>(fdcl) =
fresh(fdcl)
}

(* ****** ****** *)

implement
fresh_cvaldecl(ldcl) =
let
val node =
case ldcl.node() of
| CVALDECL(ldcl) =>
  CVALDECL(@{
    pat= ldcl.pat
  , def= 
    case ldcl.def of
    | Some(def) => Some(fresh(def))
    | None() => None()
  })
in
'{ node= node, label= fresh_label() }
end

implement
fresh_cvaldeclst(ldcl) =
list_vt2t(list_map<cvaldecl><cvaldecl>(ldcl))
where
{
implement
list_map$fopr<cvaldecl><cvaldecl>(ldcl) =
fresh(ldcl)
}

(* ****** ****** *)

implement
fresh_cvardecl(ldcl) =
let
val node =
case ldcl.node() of
| CVARDECL(ldcl) =>
  CVARDECL(@{
    hdv= ldcl.hdv
  , wth= ldcl.wth
  , ini= 
    case ldcl.ini of
    | Some(ini) => Some(fresh(ini))
    | None() => None()
  })
in
'{ node= node, label= fresh_label() }
end

implement
fresh_cvardeclst(ldcl) =
list_vt2t(list_map<cvardecl><cvardecl>(ldcl))
where
{
implement
list_map$fopr<cvardecl><cvardecl>(ldcl) =
fresh(ldcl)
}


(* ****** ****** *)

implement
fresh_c0primop(p) =
let
val node = p.node()
in
'{ node= node, label= fresh_label() }
end

(* ****** ****** *)

implement
fresh_c0clau(clau) =
let
val node = 
case clau.node() of
| C0CLAU(gpat, k, e) =>
  C0CLAU(fresh(gpat), k, fresh(e))
in
'{ node= node, label= fresh_label() }
end

implement
fresh_c0claulst(claulst) =
list_vt2t(list_map<c0clau><c0clau>(claulst))
where
{
implement
list_map$fopr<c0clau><c0clau>(clau) =
fresh(clau)
}

(* ****** ****** *)

implement
fresh_c0gpat(gpat) =
let
val node =
case gpat.node() of
| C0GPATpat(_) => gpat.node()
| C0GPATgua(pat, gua) =>
  C0GPATgua(pat, fresh(gua))
in
'{ node= node, label= fresh_label() }
end

implement
fresh_c0gpatlst(gpat) =
list_vt2t(list_map<c0gpat><c0gpat>(gpat))
where
{
implement
list_map$fopr<c0gpat><c0gpat>(gpat) =
fresh(gpat)
}

(* ****** ****** *)

implement
fresh_c0gua(gua) =
let
val node =
case gua.node() of
| C0GUAexp(v) => 
  C0GUAexp(fresh(v))
| C0GUAmat(v, pat) =>
  C0GUAmat(fresh(v), pat)
in
'{ node= node, label= fresh_label() }
end

implement
fresh_c0gualst(gua) =
list_vt2t(list_map<c0gua><c0gua>(gua))
where
{
implement
list_map$fopr<c0gua><c0gua>(gua) =
fresh(gua)
}
  
(* ****** ****** *)

implement
to_xcps(e) =
let
val e =
xcps(e, lam(x) =<cloref1>
c0exp_make_node(C0Eret(c0nt_make_node(C0HALT()), x)))
in
fresh(e)
end
