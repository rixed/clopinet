open Bricabrac
open LStream

(*
   Aggregation functions takes a value and an optional previous result
   and produce a new result.
 *)

type ('a, 'b) t = 'a -> 'b option -> 'b

let min ?(lt=(<)) x = function
    | None -> x
    | Some y -> if lt x y then x else y
    
let inv f = fun a b -> f b a

let max ?(gt=(>)) x = min (inv gt) x

let count _x = function
    | None -> 1
    | Some c -> c+1

let avg (++) x = function
    | None -> 1, x
    | Some (n, y) -> n+1, x++y

let bounds ?(lt=(<)) x = function
    | None -> x, x
    | Some (mi, ma) ->
        (if lt x mi then x else mi),
        (if lt ma x then x else ma)

(* [accum flush aggr] returns a function taking entries (as [k, v]) and grouping
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
            let v' = aggr v (try Some (Hashtbl.find h k) with Not_found -> None) in
            Hashtbl.replace h k v' ;
            first := false
        )),
    do_flush

(* We often want the first or last N entries of a sorted stream.
 * Insteqd of sorting the whole stream just to discard most of the
 * result, we here only build the set of the N entries (unsorted!). *)

let mins n (<<) s =
    let m = Array.create n None and worst = ref 0 in
    let reset_worst () =
        worst := 0 ;
        for i = 1 to n-1 do
            if unopt m.(!worst) << unopt m.(i) then worst := i
        done in
    iteri s (fun i x ->
        if i < n then m.(i) <- Some x else (
            if i = n then reset_worst () ;
            if x << unopt m.(!worst) then (
                m.(!worst) <- Some x ;
                reset_worst ()
            )
        )) ;
    of_array m // (fun x -> x <> None)

let maxs n (>>) s = mins n (inv (>>)) s

