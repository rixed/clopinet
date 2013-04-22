(* Low level functions for IP Location *)
open Batteries

external ll_init : string -> unit = "geoip_init"
external location : Unix.inet_addr -> string * float * float = "geoip_location"

let init ?(file) () =
    match file with
    | None   -> Prefs.get_string "CPN_GEOIP_DATABASE" "" |> ll_init
    | Some f -> ll_init f
