(* Functions commonly used in metrics *)
open Datatype

let verbose = ref false

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
            | Peg.Res ((v,_), rest) ->
                assert (rest = []) ;
                append v ;
                incr lineno
            | Peg.Fail -> Peg.parse_error ())
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

(* Functions related to purge old dbfiles *)

let purge dbdir lods =
    let name = Filename.basename dbdir in
    let purge_file max_age tdir hnum snum _meta =
        (* Never delete the last snum which is still written to *)
        if snum > 0 then (
            let open Unix in
            let fname = Dbfile.path tdir hnum (pred snum) in
            try (
                let last_write_age = time () -. (stat fname).st_mtime |> int_of_float in
                let last_write_age = last_write_age / 86400 in (* in days *)
                if last_write_age > max_age then (
                    Printf.printf "Deleting %s\n" fname ;
                    unlink fname ;
                    ignore_exceptions unlink (fname ^".meta")
                )
            ) with Unix_error (ENOENT, "stat", _) -> ()
        ) in
    let purge_hnum max_age tdir hnum =
        Table.iter_snums tdir hnum (fun _ -> None) (purge_file max_age tdir hnum) in
    let purge_lod lod =
        let pname = "db/max_days/"^name^"/"^lod in
        match Prefs.get_int_option pname with
        | None ->
            Printf.printf "%s unset, skipping\n" pname
        | Some max_age ->
            let tdir = table_name dbdir lod in
            Table.iter_hnums tdir (purge_hnum max_age tdir) in
    Array.iter purge_lod lods

