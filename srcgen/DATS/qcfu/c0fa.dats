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
#staload "./../../SATS/xcps.sats"
#staload "./../../SATS/c0fa.sats"
//
(* ****** ****** *)
 
implement  
c0fa(e, env) =
let
val node = e.node()
val label = e.label()
in
case node of
| C0Eret(k, v) => // continuation call
  let
  val K = c0fa_c0nt(k, env)
  val V = c0fa_c0val(v, env)
  in
  c0fa_ret(K, V, env)
  end
//
| C0Edapp(f, vs, k) => // function call
  let
  val F = c0fa_c0val(f, env)
  val VS = 
  list_vt2t(list_map<c0val><set(V)>(vs))
  where
  {
  implement
  list_map$fopr<c0val><set(V)>(v) =
  c0fa_c0val(v, env)
  }
  val K = c0fa_c0nt(k, env)
  val cache = c0fa_dapp(F, VS, K, env)
  in
  cache.extend(label, F)
  end
//
| C0Efun(fdcl, e) => // no call
  let
  val env = list_foldleft<c0env><cfundecl>(fdcl, env)
  in
  c0fa(e, env)
  end
  where
  {
  implement
  list_foldleft$fopr<c0env><cfundecl>(env, fdcl) =
  let
  val label = fdcl.label()
  val CFUNDECL(fdcl) = fdcl.node()
  val V = c0fa_c0val(fdcl.def, env)
  val env = @{
    hdv= env.hdv
  , hdc= env.hdc.extend(fdcl.hdc, V)
  , kvr= env.kvr
  }
  in
  case fdcl.def.node() of
  | C0Vfix(fid, hag, k, e) => @{
      hdv= env.hdv.extend(fid, V) 
    , hdc= env.hdc
    , kvr= env.kvr
    }
  | _ => env
  end
  }
//
| C0Eimp_fun(hdc, hag, k, bod, e) =>
  let
  val F = 
  set_sing<V>(V_make_node(E0lam(hag, k, bod), label))
  val env = @{
    hdv= env.hdv
  , hdc= env.hdc.extend(hdc, F)
  , kvr= env.kvr
  }
  in
  c0fa(e, env)
  end
| C0Eimp_val(hdc, v, e) => _
end

(* ****** ****** *)

implement
c0fa_c0val(v, env) =
let
val node = v.node()
val label = v.label()
in
case node of
| C0Vvar(x) => env.hdv.lookup(x)
| C0Vvknd(_,x) => env.hdv.lookup(x)
//
| C0Vfcon(x) =>
  set_sing<V>(V_make_node(E0con(x, list_nil()), label))
| C0Vtcon(x, ht) =>
  set_sing<V>(V_make_node(E0con(x, list_nil()), label))
//
| C0Vfcst(x) => env.hdc.lookup(x)
| C0Vtcst(x, ht) => env.hdc.lookup(x)
//
| C0Vlam(hag, k, e) =>
  set_sing<V>(V_make_node(E0lam(hag, k, e), label))
| C0Vfix(hdv, hag, k, e) =>
  set_sing<V>(V_make_node(E0fix(hdv, hag, k, e), label))
//
| _ => set_empty<V>() // uninteresting
end

(* ****** ****** *)

implement
c0fa_c0nt(k, env) =
let
val node = k.node()
val label = k.label()
in
case node of
| C0HALT() => set_empty<K>()
| C0VAR(k) => env.kvr.lookup(k)
| C0NT(hdv, e) =>
  set_sing<K>(K_make_node(EC0NT(hdv, e), label))
end

(* ****** ****** *)

implement
c0fa_ret(K, V, env) =
set_fold<K><cache(V)>(K, cache_empty<V>())
where
{
implement
set_fold$fopr<K><cache(V)>(k, cache) =
let
val node = k.node()
in
case node of
| EC0NT(hdv, e) =>
  let
  val env = @{
    hdv= env.hdv.extend(hdv, V)
  , hdc= env.hdc
  , kvr= env.kvr
  }
  in
  union(cache, c0fa(e, env))
  end
end
}

(* ****** ****** *)

implement
c0fa_dapp(F, VS, K, env) =
set_fold<V><cache(V)>(F, cache_empty<V>())
where
{
implement
set_fold$fopr<V><cache(V)>(f, cache) =
let
val node = f.node()
val label = f.label()
in
case node of
| E0con(hd, vs) => 
  let
  val VS = list_append<set(V)>(vs, VS)
  val V = 
  set_sing<V>(V_make_node(E0con(hd, vs), label))
  in
  c0fa_ret(K, V, env)
  end
| E0lam(hag, k, e) =>
  let
  val env = c0env_hfarglst(env, hag, VS)
  val env = @{
    hdv= env.hdv
  , hdc= env.hdc
  , kvr= env.kvr.extend(k, K)
  }
  in
  c0fa(e, env)
  end
| E0fix(fid, hag,  k, e) =>
  let
  val env = c0env_hfarglst(env, hag, VS)
  val env = @{
    hdv= env.hdv.extend(fid, set_sing<V>(f))
  , hdc= env.hdc
  , kvr= env.kvr.extend(k, K)
  }
  in
  c0fa(e, env)
  end
| E0imp(hdc, hag, k, e) =>
  let
  val env = c0env_hfarglst(env, hag, VS)
  val env = @{
    hdv= env.hdv
  , hdc= env.hdc
  , kvr= env.kvr.extend(k, K)
  }
  in
  c0fa(e, env)
  end
end
}
