open Bricabrac
(*
   We are going to need a lot of file descriptors for writing.
   So we'd rather share a large (but limited) set of them.
 *)

let fds = Array.make 1000 None
let free_fds = ref (LStream.range 0 (Array.length fds - 1) |> LStream.to_list)
let perm = 0o644

let dir tdir hnum =
    Printf.sprintf "%s/%d" tdir hnum

let path tdir hnum snum =
    Printf.sprintf "%s/%d" (dir tdir hnum) snum

let get ?prev tdir hnum snum =
    let fopen i =
        let oc =
            let mode = [ Open_append ; Open_creat ; Open_binary ] in
            let oc = open_out_gen mode perm (path tdir hnum snum) in
            (* Lock from the current pos up to the end of file.
             * Verified experimentaly to work on files opened in append mode,
             * even if they are empty. *)
            Unix.(lockf (descr_of_out_channel oc) F_LOCK 0) ;
            (* This lock will be dismissed whenever we close the file *)
            Output.of_channel oc in
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
                Output.close oc ;
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
                Output.close oc ;
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
                Output.close oc ;
                fds.(i) <- None ;
                free_fds := i :: !free_fds
            )
        | None -> ())

