(* ****** ****** *)
//
#include
"./../HATS/libxcmltrk.hats"
//
(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
#staload
UN = "prelude/SATS/unsafe.sats"
//
(* ****** ****** *)
#staload
"./../SATS/xcmltrk.sats"
(* ****** ****** *)
#dynload
"./../DATS/xcmltrk_main0.dats"
(* ****** ****** *)
//
implement
main0(argc, argv) =
(
//
if
(argc >= 2)
then
(
  xcmltrk_main0(argc, argv)
)
else
{
val () =
prerrln!
("Hello from ATS3(xcmltrk)!")
//
val
XATSHOME = the_XATSHOME_get()
val
((*void*)) =
prerrln!
("xcmltrk: XATSHOME=",XATSHOME)
//
} (* else *) // end of [if]
) where
{
// (*
val out = stderr_ref
val ( ) =
$XATSOPT.echo_argc_argv(out, argc, argv)
// *)
} (* end of [main] *)
//
(* ****** ****** *)

(* end of [xats_xcmltrk.dats] *)
