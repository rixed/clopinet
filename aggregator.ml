open Batteries

(*
   Aggregation functions takes a value and an optional previous result
   and produce a new result.
 *)

type ('a, 'b) t = 'a -> 'b option -> 'b

let min ?(cmp=Pervasives.compare) x = function
    | None -> x
    | Some y -> if cmp x y < 0 then x else y

let inv f = fun a b -> f b a

let max ?(gt=(>)) x = min (inv gt) x

let count _x = function
    | None -> 1
    | Some c -> c+1

let avg (++) x = function
    | None -> 1, x
    | Some (n, y) -> n+1, x++y

let bounds ?(cmp=Pervasives.compare) x = function
    | None -> x, x
    | Some (mi, ma) ->
        (if cmp x mi < 0 then x else mi),
        (if cmp ma x < 0 then x else ma)

(* [accum flush aggr ks] returns a function taking entries (as [k, v]) and grouping
   them (by [k], aggregating [v]s with aggr), flushing into the given [ks] whenever
   [flush k v] is true, and another function closing the accumulator *)
let accum flush aggr ks =
    let h = Hashtbl.create 997 in
    let first = ref true in
    let do_flush () =
        List.iter (fun k -> Hashtbl.iter k h) ks ;
        Hashtbl.clear h ;
        first := true in
    (fun k v ->
        if not !first && flush k v then (
            do_flush ()
        ) else (
            (match Hashtbl.find_option h k with
                | None   -> Hashtbl.add h k v
                | Some o -> Hashtbl.replace h k (aggr v o)) ;
            first := false
        )),
    do_flush

(* Returns true once every n seconds. *)
let now_and_then d =
    let start = ref (Unix.time ()) in
    fun _k _v ->
        let now = Unix.time () in
        if now -. !start >= d then (
            start := now ;
            true
        ) else false

(* Returns true once in n calls *)
let once_every n =
    let count = ref 0 in
    fun _k _v ->
        incr count ;
        if !count >= n then (
            count := 0 ;
            true
        ) else false

