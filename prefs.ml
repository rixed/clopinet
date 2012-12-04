open Batteries

let overwrite_h = ref None
let overwrite h = overwrite_h := Some h

let dir = ref "."
let set_dir d = dir := d

let cache_timeout = 60. (* re-read param files every minutes *)
type cache_file = float * (string, string) Hashtbl.t
let file_cache = Hashtbl.create 11 (* from filename to cache_file *)

(** Read a whole file into a hash *)
let load_file fname =
    try let h = Hashtbl.create 11 in
        File.lines_of fname |>
        Enum.iter (fun line ->
            let open String in
            if length line > 0 && line.[0] <> '#' then
                try let pname, pvalue = split line "=" in
                    Hashtbl.add h (trim pname) (trim pvalue)
                with Not_found -> ()
            else ()) ;
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
    (match Hashtbl.find_option file_cache fname with
    | None ->
        renew_cache ()
    | Some (cache_date, cache) ->
        if cache_date < Unix.time () -. cache_timeout then
            renew_cache ()
        else Some cache) |>
    (* use it to get the parameter from *)
    Option.bind (fun cache ->
        Hashtbl.find_option cache pname)

let rec get_from_dir dir pname =
    let basename, paramname = String.split pname "/" in
    let fname = dir ^"/"^ basename in
    match get_from_cached_file fname paramname with
    | None -> get_from_dir fname paramname
    | x -> x

let get_option name =
    match Option.bind (fun h ->
        Hashtbl.find_option h name)
        !overwrite_h with
    | None -> get_from_dir !dir name
    | x -> x

let get_string name default =
    get_option name |>
    Option.default default

let get_int name default =
    get_option name |>
    Option.map int_of_string |>
    Option.default default

