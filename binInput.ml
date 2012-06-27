(* Binary input from ordinary file or zip file *)

open Bricabrac

let debug = false

type strofs = { s : string ; mutable o : int }

type t = File of in_channel
       | String of strofs
       | Zip of Gzip.in_channel

let of_channel ic = File ic
let of_string str = String { s = str ; o = 0 }
let open_in ?(try_gz=true) fname =
    (* We must check first for fname then for fname.gz.
     * Otherwise, we face this race condition:
     * - look for fname.gz, not found
     * - gzip fname into fname.gz
     * - look for fname, not found! *)
    try File (open_in fname)
    with ((Sys_error _) as e) ->
        if try_gz then (
            let fname = fname ^ ".gz" in
            Zip (Gzip.open_in fname)
        ) else raise e

let read = function
    | File ic -> input_byte ic
    | Zip ic -> Gzip.input_byte ic
    | String str ->
        if str.o >= String.length str.s then raise End_of_file ;
        str.o <- str.o + 1 ;
        Char.code (str.s.[str.o - 1])

let nread t n =
    assert (n >= 0) ;
    if n = 0 then "" else
    let s = String.create n in
    (match t with
    | File ic -> really_input ic s 0 n
    | Zip ic -> Gzip.really_input ic s 0 n
    | String str ->
        if n > String.length str.s - str.o then raise End_of_file ;
        String.blit str.s str.o s 1 n) ;
    s

let close = function
    | File ic -> close_in ic
    | Zip ic -> Gzip.close_in ic
    | String _ -> ()

let with_file ?try_gz fname f =
    let t = open_in ?try_gz fname in
    try_finalize f t close t

