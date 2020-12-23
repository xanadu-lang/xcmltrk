(* ****** ****** *)
//
%{^
//
extern
void
libxatsopt_dynloadall();
//
%} (* %{^ *)
val () =
$extfcall
(void, "libxatsopt_dynloadall")
//
(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
#staload
UN="prelude/SATS/unsafe.sats"
//
(* ****** ****** *)
//
#include
"./../HATS/libxcmltrk.hats"
#staload $XATSOPT(*open-it*)
//
(* ****** ****** *)
//
#staload
STDIO =
"libats/libc/SATS/stdio.sats"
//
(* ****** ****** *)
//
#staload "./../SATS/xcmltrk.sats"
//
(* ****** ****** *)

local

in(*in-of-local*)

(* ****** ****** *)

implement
xcmltrk_main0
  (argc, argv) = let
//
val
XATSENV =
$GLO.the_XATSHOME_get((*void*))
//
val () = 
$FP0.the_includes_push(XATSENV)
//
in
println!
("xcmltrk_main0: XATSENV = ", XATSENV)
end // end of [xcmltrk_main0]

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

(* end of [xats_xcmltrk_main0.dats] *)
