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
#staload $INTREP0
//
#staload "./../SATS/xlambda.sats"
//
(* ****** ****** *)

implement
xlambda_h0exp(e) =
let
val loc = e.loc()
val node = e.node()
in
case node of
//
| H0Ei00(x) => L0Ei00(x)
| H0Eb00(x) => L0Eb00(x)
| H0Es00(x) => L0Es00(x)
//
| H0Eint(t) => L0Eint(t)
| H0Ebtf(t) => L0Ebtf(t)
| H0Echr(t) => L0Echr(t)
| H0Eflt(t) => L0Eflt(t)
| H0Estr(t) => L0Estr(t)
//
| H0Etop(t) => L0Etop(t)
//
| H0Evar(v) => L0Evar(v)
| H0Evknd(x, v) => L0Evknd(x, v)
//
| H0Efcon(c) => L0Efcon(c)
| H0Etcon(c, a) => L0Etcon(c, a)
//
| H0Efcst(c) => L0Efcst(c)
| H0Etcst(c, a) => L0Etcst(c, a)
//
| H0Etimp(_, e, _, dcl, _) =>
  let
  val e = xlambda(e)
  in
  xlambda(dcl, e)
  end
//
| H0Edapp(e1, _, e2) =>
  let
  val e1 = xlambda(e1)
  val e2 = xlambda(e2)
  in
  L0Edapp(e1, e2)
  end
//
| H0Epcon(e, l) =>
  let
  val e = xlambda(e)
  in
  L0Epcon(e, l)
  end
| H0Epbox(e, l, i) =>
  let
  val e = xlambda(e)
  in
  L0Epbox(e, l, i)
  end
//
| H0Eproj(e, l, i) =>
  let
  val e = xlambda(e)
  in
  L0Eproj(e, l, i)
  end
| H0Eplft(e, l, i) =>
  let
  val e = xlambda(e)
  in
  L0Eplft(e, l, i)
  end
| H0Epptr(e, l, i) =>
  let
  val e = xlambda(e)
  in
  L0Epptr(e, l, i)
  end
//
| H0Etuple(knd, npf, es) =>
  let
  val es = xlambda(es)
  in
  L0Etuple(knd, npf, es)
  end
//
| H0Eassgn(e1, e2) =>
  let
  val e1 = xlambda(e1)
  val e2 = xlambda(e2)
  in
  L0Eassgn(e1, e2)
  end
//
| H0Eif0(e1, e2, e3) =>
  let
  val e1 = xlambda(e1)
  val e2 = xlambda(e2)
  in
  case e3 of
  | Some(e3) =>
    let 
    val e3 = xlambda(e3)
    in
    L0Eif0(e1, e2, Some(e3))
    end
  | None() => L0Eif0(e1, e2, None())
  end
//
| H0Ecase(knd, e, clau) =>
  let
  val e = xlambda(e)
  val clau = xlambda(clau)
  in
  L0Ecase(knd, e, clau)
  end
//
| H0Elet(ds, e) =>
  let
  val e = xlambda(e)
  in
  xlambda(ds, e)
  end
//
| H0Eseqn(es, e) =>
  let
  val pats = 
  list_vt2t(list_map<h0exp><h0pat>(es))
  val node = HFARGnpats(~1, pats)
  val hfarg = hfarg_make_node(loc, node)
  val hfargs = 
  list_vt2t(list_make_sing<hfarg>(hfarg))
  //
  val es = xlambda(es)
  val e = xlambda(e)
  val f = L0Elam_hfarg(hfargs, e)
  in
  L0Edapp(f, es)
  end
  where
  {
  implement
  list_map$fopr<h0exp><h0pat>(e) =
  let
  val loc = e.loc()
  val typ = e.type()
  val node = H0Pany()
  in
  h0pat_make_node(loc, typ, node) 
  end
  }
| H0Elam(knd, arg, body) =>
  let
  val body = xlambda(body)
  in
  L0Elam_hfarg(arg, body)
  end
| H0Efix(t, fid, arg, body) =>
  let
  val dcl = LFUNDECL(@{
    nam= fid
  , hag= Some(arg)
  , def= Some(xlambda(body))
  })
  val dcls = 
  list_vt2t(list_make_sing<lfundecl>(dcl))
  in
  L0Efix(dcls)
  end
//
| H0Etry0(t, e, clau) => 
  let
  val e = xlambda(e)
  val clau = xlambda(clau)
  in
  L0Etry0(t, e, clau)
  end
//
| H0Eaddr(e) =>
  let
  val e = xlambda(e)
  in
  L0Eaddr(e)
  end
| H0Eflat(e) =>
  let
  val e = xlambda(e)
  in
  L0Eflat(e)
  end
| H0Etalf(e) =>
  let
  val e = xlambda(e)
  in
  L0Etalf(e)
  end
//
| H0Efold(e) =>
  let
  val e = xlambda(e)
  in
  L0Efold(e)
  end
//
| H0Eeval(knd, e) =>
  let
  val e = xlambda(e)
  in
  L0Eeval(knd, e)
  end
//
| H0Efree(knd, e) =>
  let
  val e = xlambda(e)
  in
  L0Efree(knd, e)
  end
//
| H0Eraise(e) =>
  let
  val e = xlambda(e)
  in
  L0Eraise(e)
  end
//
| H0Elazy(e) =>
  let
  val e = xlambda(e)
  in
  L0Elazy(e)
  end
| H0Ellazy(e, es) =>
  let
  val e = xlambda(e)
  val es = xlambda(es)
  in
  L0Ellazy(e, es)
  end
//
| H0Enone0() => L0Enone0()
| H0Enone1(x) => L0Enone1(x)
end

(* ****** ****** *)

implement
xlambda_h0explst(es) =
list_vt2t(list_map<h0exp><l0exp>(es))
where
{
implement  
list_map$fopr<h0exp><l0exp>(e) =
xlambda(e)
}

(* ****** ****** *)

implement
xlambda_h0dclist(dcls, e) =
list_foldright<h0dcl><l0exp>(dcls, e)
where
{
implement
list_foldright$fopr<h0dcl><l0exp>(dcl, e) =
xlambda(dcl, e)
}

(* ****** ****** *)

implement
xlambda_h0dcl(dcl, e) =
let
val loc = dcl.loc()
val node = dcl.node()
in
case node of
| H0Cstatic(t, dcl) => xlambda(dcl, e)
| H0Cextern(t, dcl) => xlambda(dcl, e)
| H0Clocal(head, body) =>
  let
  val dcls = list_append<h0dcl>(head, body)
  in
  xlambda(dcls, e)
  end
| H0Cinclude(t, g, knd, fp, dclsopt) =>
  (
  case dclsopt of
  | Some(dcls) => xlambda(dcls, e)
  | None() => e
  )
| H0Cfundecl(knd, dec, ht, fdcls) =>
  let
  val hdc =
  list_vt2t(list_map<hfundecl><hdcst>(fdcls))
  val fdcls =
  list_vt2t(list_map<hfundecl><lfundecl>(fdcls))
  val exp = L0Efix(fdcls)
  val exps =
  list_vt2t(list_make_sing<l0exp>(exp))
  in
  L0Edapp(L0Elam_hdcst(hdc, e), exps)
  end
  where
  {
  implement
  list_map$fopr<hfundecl><hdcst>(fdcl) =
  let
  val HFUNDECL(hf) = fdcl
  in
  hf.hdc
  end
  //
  implement
  list_map$fopr<hfundecl><lfundecl>(fdcl) =
  let
  val HFUNDECL(hf) = fdcl
  val def =
  case hf.def of
  | Some(def) => Some(xlambda(def))
  | None() => None()
  in
  LFUNDECL(@{
    nam= hf.nam
  , hag= hf.hag 
  , def= def
  })
  end
  }
| H0Cvaldecl(t, d, vd) =>
  let
  val pats = 
  list_vt2t(list_map<hvaldecl><h0pat>(vd))
  val defs = 
  list_vt2t(list_map<hvaldecl><l0exp>(vd))
  val node = HFARGnpats(~1, pats)
  val hag = hfarg_make_node(loc, node)
  val args = 
  list_vt2t(list_make_sing<hfarg>(hag))
  in
  L0Edapp(L0Elam_hfarg(args, e), defs)
  end
  where
  {
  implement
  list_map$fopr<hvaldecl><h0pat>(v) =
  let
  val HVALDECL(v) = v
  in
  v.pat
  end
  //
  implement
  list_map$fopr<hvaldecl><l0exp>(v) =
  let
  val HVALDECL(v) = v
  val def = v.def
  in
  case def of
  | Some(e) => xlambda(e)
  | None() => L0Enone0()
  end
  }
| H0Cvardecl(t, d, hv) =>
  let
  val vars =
  list_vt2t(list_map<hvardecl><hdvar>(hv))
  val exps =
  list_vt2t(list_map<hvardecl><l0exp>(hv))
  in
  L0Edapp(L0Elam_hdvar(vars, e), exps)
  end
  where
  {
  implement
  list_map$fopr<hvardecl><hdvar>(v) =
  let
  val HVARDECL(v) = v
  in
  v.hdv
  end
  //
  implement
  list_map$fopr<hvardecl><l0exp>(v) =
  let
  val HVARDECL(v) = v
  val ini = v.ini
  in
  case ini of
  | Some(e) => xlambda(e)
  | None() => L0Enone0()
  end
  }
//
| H0Cexcptcon(_) => e
//
| H0Cimpdecl3(t, s, d, q, hdc, ta, arg, exp) =>
  let
  val exp = xlambda(exp)
  val fexp = 
  case arg of 
  | list_cons(_,_) => L0Elam_hfarg(arg, exp)
  | list_nil() => exp
  val fexps =
  list_vt2t(list_make_sing<l0exp>(fexp))
  //
  val hdcs = 
  list_vt2t(list_make_sing<hdcst>(hdc))
  in
  L0Edapp(L0Elam_hdcst(hdcs, e), fexps)
  end
//
| H0Cnone1(_) => e
end

(* ****** ****** *)
  
implement
xlambda_h0clau(clau) =
let
val node = clau.node()
in
case node of
| H0CLAUpat(pat) => 
  let
  val pat = xlambda(pat)
  in
  L0CLAUpat(pat)
  end
| H0CLAUexp(pat, e) =>
  let
  val pat = xlambda(pat)
  val e = xlambda(e)
  in
  L0CLAUexp(pat, e)
  end
end

implement
xlambda_h0claulst(clau) =
list_vt2t(list_map<h0clau><l0clau>(clau))
where
{
implement
list_map$fopr<h0clau><l0clau>(clau) =
xlambda(clau)
}

(* ****** ****** *)

implement
xlambda_h0gpat(gpat) =
let
val node = gpat.node()
in
case node of
| H0GPATpat(pat) => L0GPATpat(pat)
| H0GPATgua(pat, gua) =>
  let
  val gua = xlambda(gua)
  in
  L0GPATgua(pat, gua)
  end
end

(* ****** ****** *)

implement
xlambda_h0gua(gua) =
let
val node = gua.node()
in
case node of
| H0GUAexp(e) =>
  let
  val e = xlambda(e)
  in
  L0GUAexp(e)
  end
| H0GUAmat(e, pat) =>
  let
  val e = xlambda(e)
  in
  L0GUAmat(e, pat)
  end
end

(* ****** ****** *)

implement
xlambda_h0gualst(gua) =
list_vt2t(list_map<h0gua><l0gua>(gua))
where
{
implement
list_map$fopr<h0gua><l0gua>(gua) =
xlambda(gua)
}
