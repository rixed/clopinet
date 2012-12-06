(*
   We are going to need a lot of file descriptors for writing (both
   because we hash each table and because we use an arbitraty number
   of tables.
   So we'd rather share a large (but limited) set of them.
 *)

let (|>) = Batteries.(|>)

let fds = Array.make (Prefs.get_int "db/max_opened_filedescr" 1000) None
let free_fds = ref (LStream.range 0 (Array.length fds - 1) |> LStream.to_list)

let dir tdir hnum =
    Printf.sprintf "%s/%d" tdir hnum

let path tdir hnum snum =
    Printf.sprintf "%s/%d" (dir tdir hnum) snum

let get ?prev prealloc tdir hnum snum =
    let fopen i =
        let oc = Serial.make_obuf (path tdir hnum snum) false prealloc in
        (* FIXME: in ll_serial, lock opened obufs!
            (* Lock from the current pos up to the end of file.
             * specs says: "lock all bytes starting at the location specified by l_whence
             * and l_start through to the end of file, no matter how large the file grows". *)
            Unix.(lockf (descr_of_out_channel oc) F_LOCK 0) ;
            (* This lock will be dismissed whenever we close the file *)
            Output.of_channel oc in *)
        fds.(i) <- Some (tdir, hnum, snum, oc) ;
        i, oc in
    let get_new () = match !free_fds with
        | i::i' ->
            let res = fopen i in
            free_fds := i' ;
            res
        | [] ->
            let i = Random.int (Array.length fds) in
            (match fds.(i) with
            | Some (_, _, _, oc) ->
                Serial.close_obuf oc ;
                fds.(i) <- None ;
                fopen i
            | None -> assert false) in
    match prev with
    | Some i ->
        (match fds.(i) with
        | Some (dir, sernum, seqnum, oc) ->
            if dir == tdir && hnum = sernum && seqnum = snum then (
                i, oc
            ) else (
                Serial.close_obuf oc ;
                fds.(i) <- None ;
                fopen i
            )
        | None -> get_new ())
    | None -> get_new ()

let close ?prev tdir hnum snum =
    match prev with
    | None -> ()
    | Some i ->
        (match fds.(i) with
        | Some (dir, sernum, seqnum, oc) ->
            if dir == tdir && sernum = hnum && seqnum = snum then (
                Serial.close_obuf oc ;
                fds.(i) <- None ;
                free_fds := i :: !free_fds
            )
        | None -> ())

