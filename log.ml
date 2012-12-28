open Batteries

module Config =
struct
    type t = unit
    let out = IO.stderr
    let prefix = ""

    let get_w () = Gc.counters () |> BatTuple.Tuple3.first
    let last_t = ref (Unix.gettimeofday ())
    let last_w = ref (get_w ())
    let bench () =
        let now = Unix.gettimeofday () in
        let dt = now -. !last_t in
        let w = get_w () in
        let dw = w -. !last_w in
        last_t := now ;
        last_w := w ;
        Printf.sprintf "[%+ 8.3fs,%+ 12.0fw]" dt dw

    let flags = [ `Date; `Time; `Custom bench ]
end

include BatLog.Make (Config)

let nolog fmt = Printf.ifprintf Config.out fmt (* is optimized out *)
let debug = nolog
let info = logf
let warn = logf
let err = logf
let error = err
let warning = warn
