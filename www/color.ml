(* Simple color scales *)
open Batteries

type t = float array
type scale = (float * t) list (* ordered list of value + color *)

let white = [| 1.;1.;1. |]

let to_html c =
    let os = IO.output_string () in
    Array.print ~first:"#" ~last:"" ~sep:""
        (fun oc f ->
            Printf.fprintf oc "%02x" (int_of_float (255.*.f)))
        os c ;
    IO.close_out os

let get s v =
    let rec aux (start, start_col) = function
        | [] -> white
        | (stop, stop_col)::s' ->
            if v = stop then stop_col else
            if v > stop then aux (stop, stop_col) s' else
            let r = (v -. start) /. (stop -. start) in
            Array.mapi (fun i stop_c ->
                let start_c = if Array.length start_col <= i then 0. else start_col.(i) in
                start_c +. r *. (stop_c -. start_c))
                stop_col in
    aux (0., [||]) s |> to_html
    
