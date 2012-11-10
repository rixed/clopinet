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

let load fname read append flush =
    Sys.(List.iter
        (fun s -> set_signal s (Signal_handle (fun _ -> flush ())))
        [ sigabrt; sigfpe; sigill; sigint;
          sigpipe; sigquit; sigsegv; sigterm ]) ;

    let lineno = ref 0 in
    Bricabrac.try_finalize (fun () ->
        Bricabrac.with_file_in fname (fun ic ->
            Printf.fprintf stderr "loading from %s\n%!" fname ;
            let ic = TxtInput.from_file ic in
            try forever (fun () ->
                read ic |> append ;
                let eol = TxtInput.read ic in
                assert (eol = '\n' || eol = '\r') ;
                incr lineno) ()
            with End_of_file ->
                if !verbose then Printf.fprintf stderr "Inserted %d lines\n" !lineno
               | e ->
                Printf.fprintf stderr "Error at line %d\n" !lineno ;
                raise e)) ()
        flush ()


(* misc *)

let table_name dbdir name = dbdir ^ "/" ^ name

