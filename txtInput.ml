(* Custom IO with a peek facility *)

let debug = false

type t = {
    ic : in_channel ;
    mutable peeked : char option }

let from ic = { ic ; peeked = None }

let read t = match t.peeked with
    | None   -> input_char t.ic
    | Some b -> t.peeked <- None ; b

let read t =
    if debug then (
        let c = read t in
        Printf.printf "read '%c'\n%!" c ;
        c
    ) else read t

let nread t n =
    let s = String.create n in
    s.[0] <- read t ;
    really_input t.ic s 1 (n-1) ;
    s

let nread t n =
    if debug then (
        let s = nread t n in
        Printf.printf "nread '%s'\n%!" s ;
        s
    ) else nread t n

let peek t = match t.peeked with
    | Some b -> b
    | None   ->
        let b = input_char t.ic in
        t.peeked <- Some b ;
        b

let peek t =
    if debug then (
        let c = peek t in
        Printf.printf "peek '%c'\n%!" c ;
        c
    ) else peek t

let swallow t =
    if t.peeked <> None then t.peeked <- None

