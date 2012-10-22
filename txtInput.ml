(* Custom IO with a peek facility *)

let debug = false

type strofs = { s : string ; mutable o : int }
type char_source = File of in_channel | String of strofs

type t = {
    src : char_source ;
    mutable peeked : char option }

let from_file ic = { src = File ic ; peeked = None }

let of_string str = { src = String { s = str ; o = 0 } ; peeked = None }

let read t = match t.peeked with
    | None ->
        (match t.src with
        | File ic -> input_char ic
        | String str ->
            if str.o >= String.length str.s then raise End_of_file ;
            str.o <- succ str.o ;
            str.s.[str.o - 1])
    | Some b -> t.peeked <- None ; b

let read t =
    if debug then (
        let c = read t in
        Printf.printf "read '%c'\n%!" c ;
        c
    ) else read t

let nread t n =
    assert (n >= 0) ;
    if n = 0 then "" else
    let s = String.create n in
    (match t.src with
    | File ic -> really_input ic s 0 n
    | String str ->
        if n > String.length str.s - str.o then raise End_of_file ;
        String.blit str.s str.o s 0 n ;
        str.o <- str.o + n) ;
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
        let b = read t in
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

let hexdigit t =
    let c = read t in
    if c >= '0' && c <= '9' then (
        Char.code c - Char.code '0'
    ) else if c >= 'a' && c <= 'f' then (
        10 + Char.code c - Char.code 'a'
    ) else (
        assert (c >= 'A' && c <= 'F') ;
        10 + Char.code c - Char.code 'A'
    )

