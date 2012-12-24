open Batteries

module Config =
struct
    type t = unit
    let out = IO.stderr
    let prefix = ""

    let last = ref (Unix.time ())
    let my_prefix () =
        let now = Unix.time () in
        let dt = now -. !last in
        last := now ;
        Printf.sprintf "[%+ 8.3f]" dt
    
    let flags = [ `Date; `Time; `Custom my_prefix ]
end

include BatLog.Make (Config)

let debug fmt = Printf.ifprintf Config.out fmt (* is optimized out *)
let info = logf
let warn = logf
let err = logf

