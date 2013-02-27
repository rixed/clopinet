open Batteries

let overwrite_function = ref (fun _s -> None)
let set_overwrite_function f = overwrite_function := f

let pair_from_line line =
    let open String in
    if length line > 0 && line.[0] <> '#' then
        try let pname, pvalue = split line "=" in
            Some (trim pname, trim pvalue)
        with Not_found -> None
    else None

let insert_into h line =
    pair_from_line line |>
    BatOption.may (fun (k, v) -> Hashtbl.add h k v)

let overwrite_h = Hashtbl.create 31
let overwrite_many h =
    Hashtbl.iter (fun n v -> Hashtbl.replace overwrite_h n v) h
let overwrite_single s =
    insert_into overwrite_h s

let base = ref "./conf"
let last_read = ref 0.
let cache = Hashtbl.create 11

let set_base d =
    base := d ;
    last_read := 0.

let get_from_cached_file pname =
    let mdate = Unix.((stat !base).st_mtime) in
    if !last_read < mdate then (
        (* renew cache *)
        Hashtbl.clear cache ;
        File.lines_of !base |>
        Enum.iter (insert_into cache) ;
        last_read := mdate
    ) ;
    (* then read from the cache *)
    Hashtbl.find_option cache pname

let get_option name =
    match !overwrite_function name with
    | None ->
        (match Hashtbl.find_option overwrite_h name with
        | None -> get_from_cached_file name
        | x -> x)
    | x -> x

let get_string name default =
    get_option name |>
    Option.default default

let my_float_of_string s =
    Scanf.sscanf s "%f%s" (fun f rest ->
        f *. match String.trim rest with
            | "" -> 1.
            | "k" -> 1_000.
            | "K" -> 1_024.
            | "M" -> 1_000_000.
            | "G" -> 1_000_000_000.
            | "P" -> 1_000_000_000_000.
            | _ -> invalid_arg rest)

let get_float_option name =
    get_option name |>
    Option.map my_float_of_string

let get_float name default =
    get_float_option name |>
    Option.default default

let my_int_of_string s =
    int_of_float (my_float_of_string s)

let get_int_option name =
    get_option name |>
    Option.map my_int_of_string

let get_int name default =
    get_int_option name |>
    Option.default default

let get_bool_option name =
    get_option name |>
    Option.map (String.lowercase %> bool_of_string)

let get_bool name default =
    get_bool_option name |>
    Option.default default

(* Conf file shoudl probably stay read only
let filter_file fname f =
    let tmpfile = fname ^ (Random.int 99999 |> string_of_int) ^ ".tmp" in
    ignore_exceptions Unix.unlink tmpfile ;
    File.lines_of fname /@ f |> File.write_lines tmpfile ;
    let backup = fname ^".old" in
    ignore_exceptions Unix.unlink backup ;
    Unix.rename fname backup ;
    Unix.rename tmpfile fname
*)
