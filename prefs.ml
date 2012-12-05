open Batteries

let overwrite_function = ref (fun _s -> None)
let set_overwrite_function f = overwrite_function := f

let insert_into h line =
    let open String in
    if length line > 0 && line.[0] <> '#' then
        try let pname, pvalue = split line "=" in
            Hashtbl.add h (trim pname) (trim pvalue)
        with Not_found -> ()
    else ()

let overwrite_h = Hashtbl.create 31
let overwrite_many h =
    Hashtbl.iter (fun n v -> Hashtbl.replace overwrite_h n v) h
let overwrite_single s =
    insert_into overwrite_h s

let base = ref "./conf"
let set_base d = base := d

let cache_timeout = 60. (* re-read param files every minutes *)
type cache_file = float * (string, string) Hashtbl.t
let file_cache = Hashtbl.create 11 (* from filename to cache_file *)

(** Read a whole file into a hash *)
let load_file fname =
    try let h = Hashtbl.create 11 in
        File.lines_of fname |>
        Enum.iter (insert_into h) ;
        Some h
    with Sys_error _ ->
        (* No such file *)
        None

let get_from_cached_file fname pname =
    let renew_cache () =
        load_file fname |>
        Option.map (fun cache ->
            Hashtbl.replace file_cache fname (Unix.time(), cache) ;
            cache) in
    (* get the file cache *)
    let cache = match Hashtbl.find_option file_cache fname with
    | None ->
        renew_cache ()
    | Some (cache_date, cache) ->
        if cache_date < Unix.time () -. cache_timeout then
            renew_cache ()
        else Some cache in
    (* use it to get the parameter from *)
    Option.bind cache (fun cache ->
        Hashtbl.find_option cache pname)

let rec get_from_dir fname pname =
    match get_from_cached_file fname pname with
    | None ->
        (try let basename, paramname = String.split pname "/" in
            get_from_dir (fname ^"/"^ basename) paramname
        with Not_found -> None)
    | x -> x

let get_option name =
    match !overwrite_function name with
    | None ->
        (match Hashtbl.find_option overwrite_h name with
        | None -> get_from_dir !base name
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
    Option.map (String.lowercase |- bool_of_string)

let get_bool name default =
    get_bool_option name |>
    Option.default default

