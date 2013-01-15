open Batteries
open LStream

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

(* We often want the first or last N entries of a sorted stream.
 * Insteqd of sorting the whole stream just to discard most of the
 * result, we here only build the set of the N entries (unsorted!). *)

let mins n (<<<) s =
    let m = Array.create n None and worst = ref 0 in
    let reset_worst () =
        worst := 0 ;
        for i = 1 to n-1 do
            if Option.get m.(!worst) <<< Option.get m.(i) then worst := i
        done in
    iteri s (fun i x ->
        if i < n then m.(i) <- Some x else (
            if i = n then reset_worst () ;
            if x <<< Option.get m.(!worst) then (
                m.(!worst) <- Some x ;
                reset_worst ()
            )
        )) ;
    of_array m // (fun x -> x <> None)

let maxs n (>>) s = mins n (inv (>>)) s

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

