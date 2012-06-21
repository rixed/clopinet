open Bricabrac
(*
   We are going to need a lot of file descriptors.
   So we'd rather share a large (but limited) set of them.
 *)

let free_fd = "", 0, 0, stdout
let fds = Array.make 1000 free_fd
let free_fds = ref (LStream.range 0 (Array.length fds - 1) |> LStream.to_list)
let perm = 0o644

let dir tdir hnum =
    Printf.sprintf "%s/%d" tdir hnum

let path tdir hnum snum =
    Printf.sprintf "%s/%d" (dir tdir hnum) snum

let get ?prev tdir hnum snum ensure_new =
    let fopen i =
        let oc =
            let mode = [ Open_append ; Open_creat ] in
            let mode = if ensure_new then Open_excl::mode else mode in
            open_out_gen mode perm (path tdir hnum snum) in
        fds.(i) <- tdir, hnum, snum, oc ;
        i, oc in
    match prev with
    | Some i ->
        let dir, sernum, seqnum, oc = fds.(i) in
        if dir == tdir && hnum = sernum && seqnum = snum then (
            let _, _, _, oc = fds.(i) in
            i, oc
        ) else (
            close_out oc ;
            fds.(i) <- free_fd ;
            fopen i
        )
    | None ->
        (match !free_fds with
        | i::i' ->
            let res = fopen i in
            free_fds := i' ;
            res
        | [] ->
            let i = Random.int (Array.length fds) in
            let _, _, _, oc = fds.(i) in
            close_out oc ;
            fds.(i) <- free_fd ;
            fopen i)

let close ?prev tdir hnum snum =
    match prev with
    | None -> ()
    | Some i ->
        let dir, sernum, seqnum, oc = fds.(i) in
        if dir == tdir && sernum = hnum && seqnum = snum then (
            close_out oc ;
            fds.(i) <- free_fd ;
            free_fds := i :: !free_fds
        )

