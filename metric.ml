(* Functions commonly used in metrics *)
open Datatype

(* Functions related to META files that are formed by start and stop timestamps *)
module BoundsTS = Tuple2.Make (Timestamp) (Timestamp)

open Batteries

(* Tools *)

let list_merge_lim lim l1 l2 =
    (* as lists are short, not tail rec *)
    let rec aux lim l1 l2 =
        if lim <= 0 then [] else
        match l1,l2 with
            | a::a', b::b' -> if a < b then a :: aux (lim-1) a' l2 else
                              if a > b then b :: aux (lim-1) l1 b' else
                                            a :: aux (lim-1) a' b'
            | a::a', [] -> a :: aux (lim-1) a' l2
            | [], b::b' -> b :: aux (lim-1) l1 b'
            | [], [] -> []
    in
    aux lim l1 l2

(*$= list_merge_lim as lml & ~printer:(IO.to_string (List.print Int.print))
  (lml 5 [1;3] [2;4]) [1;2;3;4]
  (lml 3 [1;3] [2;4]) [1;2;3]
  (* check fusion of same elements *)\
  (lml 5 [1;2;3] [2;3;4;5]) [1;2;3;4;5]
 *)

let check vopt f = match vopt with
    | None -> true
    | Some v -> f v

let is_within bounds start stop = match bounds with
    | None -> true
    | Some (ts1, ts2) ->
        let cmp = Timestamp.compare in
        check start (fun start -> not (cmp ts2 start < 0)) &&
        check stop  (fun stop  -> not (cmp stop ts1 < 0))

let optmin a b = match a, b with
    | Some a, Some b -> Some (min a b)
    | _ -> a
let optmax a b = match a, b with
    | Some a, Some b -> Some (max a b)
    | _ -> b

(* used by some filters *)
let string_ends_with e s =
    let eo = String.length e - 1 and so = String.length s - 1 in
    if eo > so then false else
    let rec aux eo so =
        if eo < 0 then true else e.[eo] = s.[so] && aux (eo-1) (so-1) in
    aux eo so

let string_starts_with e s =
    if String.length e > String.length s then false else
    let rec aux eo so =
        if eo >= String.length e then true else e.[eo] = s.[so] && aux (eo+1) (so+1) in
    aux 0 0

(* functions related to the index *)

let fold_using_indexed ip tdir fold_hnum make_fst merge = match ip with
    | Some cidr when subnet_size cidr < Table.max_hash_size ->
        Log.debug "Using index" ;
        (* We have an index for this! Build the list of hnums *)
        let visited = Hashtbl.create Table.max_hash_size in
        let hnums = fold_ips cidr (fun ip p ->
            let hnum = InetAddr.hash ip mod Table.max_hash_size in
            if Hashtbl.mem visited hnum then (
                p
            ) else (
                Hashtbl.add visited hnum true ;
                hnum :: p
            )) [] in
        Table.fold_some_hnums hnums fold_hnum make_fst merge
    | _ ->
        Table.fold_hnums tdir fold_hnum make_fst merge

(* function related to loading datas *)

let load fname parzer append flush =
    Sys.(List.iter
        (fun s -> set_signal s (Signal_handle (fun _ -> Log.info "Flushing..." ; flush ())))
        [ sigabrt; sigfpe; sigill; sigint;
          sigpipe; sigquit; sigsegv; sigterm ]) ;

    let lineno = ref 0 in
    let last_line = ref "" in
    let save_line l = last_line := l in
    (try File.lines_of fname /@
        tap save_line /@
        String.to_list /@
        Peg.(parzer ++ eof) |>
        Enum.iter (function
            | Peg.Res ((v,_), rest) ->
                assert (rest = []) ;
                append v ;
                incr lineno
            | Peg.Fail -> Peg.parse_error ())
    with Peg.Parse_error err ->
        Log.error "Error at line %d: %s" !lineno (Peg.string_of_error ~input:!last_line err)
    |  e ->
        Log.error "Error at line %d: %s" !lineno (Printexc.to_string e) ;
        Log.error "%s" (Printexc.get_backtrace ())) ;
    Log.info "Inserted %d lines" !lineno ;
    Log.info "Flushing..." ;
    flush ()

(* misc *)

let table_name dbname lodname =
    let dbdir = Prefs.get_string "CPN_DB_BASE_DIR" "./" in
    dbdir ^"/"^ dbname ^"/"^ lodname

let rti_of_lod lod metric =
    let pname = "CPN_DB_"^ metric ^"_"^ lod ^"_ROUND" |> String.uppercase in
    match Interval.of_pref_option pname with
    | Some rti -> Some (Interval.to_ms rti)
    | None -> Log.warning "No round time interval found for %s/%s, will skip this LoD" metric lod ;
              None

let buffer_duration_of_lod lod metric =
    let msecs = rti_of_lod lod metric in
    BatOption.map (fun ms -> Int64.to_float ms *. 0.001 *. 2.) msecs |>
    BatOption.default 1. (* won't be used if this lod is not defined *)


(* functions related to repairing db files *)

exception File_truncated
let save_backup fname =
    Unix.system ("cp "^fname^" "^fname^".bak") |>
    ignore

let del_file fname =
    save_backup fname ;
    Unix.unlink fname

let fix_data e fname pos =
    Log.notice "Data file '%s' unreadable after offset %Ld:\n%s %s%!"
        fname pos (Printexc.to_string e) (Printexc.get_backtrace ()) ;
    save_backup fname ;
    Unix.LargeFile.truncate fname pos ;
    (* We keep the meta file as is, with the assumption that it contains only
     * range of present values - ie still valid, is less acute *)
    raise File_truncated

let fix_meta e fname =
    Log.notice "Meta file '%s' unreadable:\n%s %s%!"
        fname (Printexc.to_string e) (Printexc.get_backtrace ()) ;
    del_file fname

let dbck dbname lods read meta_read =
    let ck_read tdir hnum snum last_read ic =
        try ignore (read ic) ;
            last_read := Serial.position ic
        with e when e <> End_of_file ->
            fix_data e (Dbfile.path tdir hnum snum) !last_read in
    let ck_file tdir hnum snum _meta =
        (* check meta file *)
        let fname = Dbfile.path tdir hnum snum ^ ".meta" in
        (try Serial.with_file_in fname meta_read |>
             ignore
        with Sys_error _ -> ()
           | e -> fix_meta e fname) ;
        (* check data file *)
        try
            Table.iter_file tdir hnum snum (ck_read tdir hnum snum (ref 0L)) ignore
        with File_truncated -> () in
    let ck_hnum tdir hnum =
        Table.iter_snums tdir hnum (fun _ -> None) (ck_file tdir hnum) in
    let ck_lod lod =
        let tdir = table_name dbname lod in
        Table.iter_hnums tdir (ck_hnum tdir) in
    Array.iter ck_lod lods

(* Functions related to purge old dbfiles *)

let rec purge dbname lods =
    let purge_lod lod =
        let tdir = table_name dbname lod in
        (* Get a list of all deletable files (as filename, age) *)
        let files =
            Table.fold_hnums tdir (fun hnum first ->
                Table.fold_snums tdir hnum ignore (fun snum _ lst ->
                    (* Add the *previous* snum to our list (thus avoiding deletion of
                     * the last snum which is still written to) *)
                    if snum > 0 then (
                        let snum = pred snum in
                        let fname = Dbfile.path tdir hnum snum in
                        let open Unix in
                        try (
                            let age = time () -. (stat fname).st_mtime in
                            (fname, age) :: lst
                        ) with Unix_error (ENOENT, "stat", _) -> lst
                    ) else lst
                ) first List.rev_append
            ) (fun () -> []) List.rev_append in
        (* order files according to age (descending) so that we have older files first *)
        let files =
            List.sort (fun (_, a1) (_, a2) -> ~- (compare a1 a2)) files in
        (* now delete files until we met date/space constraints *)
        let max_age_pname  = "CPN_DB_"^dbname^"_"^lod^"_MAX_AGE"  |> String.uppercase
        and max_size_pname = "CPN_DB_"^dbname^"_"^lod^"_MAX_SIZE" |> String.uppercase in
        let max_age =
            Interval.of_pref_option max_age_pname |>
            Option.map Interval.to_secs
        and max_size =
            Integer.of_pref_option max_size_pname in
        if max_age  = None then Log.notice "%s unset, skipping purge by age"  max_age_pname ;
        if max_size = None then Log.notice "%s unset, skipping purge by size" max_size_pname ;
        let tot_size = ref (Table.max_file_size * List.length files) in
        List.iter (fun (fname, age) ->
            if (match max_age with Some max -> age > max | _ -> false) ||
               (match max_size with Some max -> !tot_size > max | _ -> false)
            then (
                Log.info "Deleting %s" fname ;
                tot_size := !tot_size - Table.max_file_size ;
                Unix.unlink fname ;
                ignore_exceptions Unix.unlink (fname ^".meta")
            )
        ) files in
    let period =
        Interval.of_pref_option "CPN_DB_PURGE_PERIOD" |>
        Option.map Interval.to_secs |>
        Option.map Int.of_float in
    Array.iter purge_lod lods ;
    (* loop *)
    match period with
    | Some p ->
        Unix.sleep p ;
        purge dbname lods
    | None -> ()

(* Functions related to enriching labels.
 * Many functions receive string labels but may want to know what the string refers to
 * to be able to offer links or more. So we keep native repr as long as possible. *)

type label = Mac of (VLan.t * EthAddr.t) | Ip of InetAddr.t | Other of string

let string_of_label = function
    | Mac (Some vl, mac) ->
        "vlan:"^ string_of_int vl ^","^ EthAddr.to_string mac
    | Mac (None, mac) -> EthAddr.to_string mac
    | Ip t -> InetAddr.to_string t
    | Other t -> t

let label_compare l1 l2 =
    String.compare (string_of_label l1) (string_of_label l2)

