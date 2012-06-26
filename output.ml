(* We want to be able to output in a regular channel as well as in a buffer,
   thus this module. *)
open Bricabrac

type t = File of out_channel
       | Buf of Buffer.t

let of_channel oc = File oc

let of_buffer buf = Buf buf

let stdout = of_channel stdout
let stderr = of_channel stderr

let char t c = match t with
    | File oc -> output_char oc c
    | Buf buf -> Buffer.add_char buf c

let byte t i = match t with
    | File oc -> output_byte oc i
    | Buf buf -> Buffer.add_char buf (Char.chr (i land 255))

let string t str = match t with
    | File oc -> output_string oc str
    | Buf buf -> Buffer.add_string buf str

let close = function
    | File oc -> close_out oc
    | Buf _buf -> ()

