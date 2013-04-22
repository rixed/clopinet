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

let get_from_env name =
    (* remove all underscores, then replace '/' by '_', then put uppercase *)
    try Some (Sys.getenv name)
    with Not_found -> None

let get_option name =
    match !overwrite_function name with
    | None ->
        (match Hashtbl.find_option overwrite_h name with
        | None -> get_from_env name
        | x -> x)
    | x -> x

let get_string name default =
    get_option name |>
    Option.default default

let enum () =
    let he = Hashtbl.enum overwrite_h
    and fe = Enum.unfold 0 (fun n -> Option.map (fun e -> e, succ n) (Environ.get n)) in
    Enum.append he fe
