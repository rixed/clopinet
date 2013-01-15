(* Functions commonly used in metrics *)
open Datatype

let verbose = ref false

(* Functions related to META files that are formed by start and stop timestamps *)
module BoundsTS = Tuple2.Make (Timestamp) (Timestamp)

open Batteries

let check vopt f = match vopt with
    | None -> true
    | Some v -> f v

let is_within bounds start stop = match bounds with
    | None -> true
    | Some (ts1, ts2) ->
        let cmp = Timestamp.compare in
        check start (fun start -> not (cmp ts2 start < 0)) &&
        check stop  (fun stop  -> not (cmp stop ts1 < 0))

(* functions related to the index *)

let fold_using_indexed ip tdir fold_hnum make_fst merge = match ip with
    | Some cidr when subnet_size cidr < Table.max_hash_size ->
        if !verbose then Printf.fprintf stderr "Using index\n" ;
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
        (fun s -> set_signal s (Signal_handle (fun _ -> flush ())))
        [ sigabrt; sigfpe; sigill; sigint;
          sigpipe; sigquit; sigsegv; sigterm ]) ;

    let lineno = ref 0 in
    (try File.lines_of fname /@
        String.to_list /@
        Peg.(parzer ++ eof) |>
        Enum.iter (function
            | Peg.Res ((v,_), []) ->
                append v ;
                incr lineno
            | _ -> raise Peg.Parse_error)
    with e ->
        Printf.fprintf stderr "Error at line %d\n" !lineno ;
        flush () ;
        raise e) ;
    if !verbose then Printf.fprintf stderr "Inserted %d lines\n" !lineno ;
    flush ()

(* misc *)

let table_name dbdir name = dbdir ^ "/" ^ name

(* functions related to repairing db files *)

exception File_truncated
let save_backup fname =
    Unix.system ("cp "^fname^" "^fname^".bak") |>
    ignore

let del_file fname =
    save_backup fname ;
    Unix.unlink fname

let fix_data e fname pos =
    Printf.printf "Data file '%s' unreadable after offset %Ld:\n%s %s\n%!"
        fname pos (Printexc.to_string e) (Printexc.get_backtrace ()) ;
    save_backup fname ;
    Unix.LargeFile.truncate fname pos ;
    (* We keep the meta file as is, with the assumption that it contains only
     * range of present values - ie still valid, is less acute *)
    raise File_truncated

let fix_meta e fname =
    Printf.printf "Meta file '%s' unreadable:\n%s %s%!"
        fname (Printexc.to_string e) (Printexc.get_backtrace ()) ;
    del_file fname

let dbck dbdir lods read meta_read =
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
        let tdir = table_name dbdir lod in
        Table.iter_hnums tdir (ck_hnum tdir) in
    Array.iter ck_lod lods

(* Fields models for templates *)

type aggr_function = { zero : string ; func : string ; fin : string }

type selectable_field = {
    aggrs : (string * aggr_function) list ;
    sortable : string ; (* name of to_int function, or "" *)
    keyable : bool ;
    datatype : string }

