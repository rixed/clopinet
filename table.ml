open Bricabrac
open LargeFile

let max_file_size = 10_000_000
let max_hash_size = 1009 (* which is prime *)
let ncores = ref 1

(* READING
   Can be done by multiple programs simultaneously,
   no persistent data involved.
*)

(* The meta file, which accompany every dbfile, stores the aggregation result *)
let read_meta_fname fname aggr_reader =
    try Some (Serial.with_file_in fname aggr_reader)
    with Sys_error _ -> None

let read_meta tdir hnum snum aggr_reader =
    let fname = Dbfile.path tdir hnum snum ^ ".meta" in
    read_meta_fname fname aggr_reader

let iter_fname fname reader f =
    Serial.with_file_in fname (fun ic ->
        (* TODO: Use posix_fadvise(fd, 0, 0, POSIX_FADV_SEQUENTIAL) *)
        try forever (fun () ->
            (* TODO: from time to time, call posix_fadvise(fd, 0, current_offset, POSIX_FADV_DONTNEED) *)
            f (reader ic)) ()
        with End_of_file -> ())

let iter_file tdir hnum snum reader f =
    let hnum = hnum mod max_hash_size in
    let fname = Dbfile.path tdir hnum snum in
    iter_fname fname reader f

let iter_snums tdir hnum aggr_reader f =
    let hnum = hnum mod max_hash_size in
    let iterfile name =
        let name =
            let len = String.length name in
            if len > 3 && name.[len-3] = '.' && name.[len-2] = 'g' && name.[len-1] = 'z'
            then String.sub name 0 (len-3)
            else name in
        try let snum = int_of_string name in
            f snum (read_meta tdir hnum snum aggr_reader)
        with Failure _ -> ()
    and files = Sys.readdir (Dbfile.dir tdir hnum) in
    try (
        if !ncores > 1 then
            Parmap.pariter ~ncores:!ncores iterfile (Parmap.A files)
        else
            Array.iter iterfile files
    ) with Sys_error _ -> () (* no such directory, may happen in we haven't written anything yet *)

let iter_hnums tdir f =
    try (
        Sys.readdir tdir |>
        Array.iter (fun name ->
            try f (int_of_string name)
            with Failure _ -> ())
    ) with Sys_error _ -> ()

let iter tdir reader f =
    iter_hnums tdir (fun hnum ->
        iter_snums tdir hnum ignore (fun snum _meta ->
            iter_file tdir hnum snum reader f))

(* Folding. As expected, but we have a merge function to operate on partial results. *)

let fold_file tdir hnum snum reader f start =
    let hnum = hnum mod max_hash_size in
    let fname = Dbfile.path tdir hnum snum in
    Serial.with_file_in fname (fun ic ->
        let rec aux res =
            match (try Some (reader ic) with End_of_file -> None) with
            | Some x -> aux (f x res)
            | None   -> res in
        aux start)

let fold_snums tdir hnum aggr_reader f start merge =
    let hnum = hnum mod max_hash_size in
    let foldfile name res =
        let name =
            let len = String.length name in
            if len > 3 && name.[len-3] = '.' && name.[len-2] = 'g' && name.[len-1] = 'z'
            then String.sub name 0 (len-3)
            else name in
        try let snum = int_of_string name in
            f snum (read_meta tdir hnum snum aggr_reader) res
        with Failure _ -> res
    and files = Sys.readdir (Dbfile.dir tdir hnum) in
    try (
        if !ncores > 1 then
            Parmap.parfold ~ncores:!ncores foldfile (Parmap.A files) start merge
        else
            Array.fold_right foldfile files start
    ) with Sys_error _ -> start (* no such directory, may happen in we haven't written anything yet *)

let fold_hnums tdir f start copy merge =
    let res =
        try (
            Sys.readdir tdir |>
            Array.fold_left (fun prev name ->
                try (f (int_of_string name) (copy start)) :: prev
                with Failure _ -> prev)
                []
        ) with Sys_error _ -> [] in
    List.fold_left merge start res

let fold tdir reader f start copy merge =
    fold_hnums tdir (fun hnum res ->
        fold_snums tdir hnum ignore (fun snum _meta res' ->
            fold_file tdir hnum snum reader f res')
            res merge)
        start
        copy
        merge

(* WRITING
   We cache index function for the table, current sequence number and
   aggregator for each hnum.
   Because of this cache only one process can write.
   TODO: lock a file to check this *)

(* Due to how we open and write in many files simultaneously, the
   resulting file set will be fragmented up to a point reading will
   be slow. Compression/copy solves this problem. *)

type 'b h_cache =
          { mutable max_snum : int ;
                mutable file : int option ;
                mutable aggr : 'b option ;
     mutable last_checkpoint : float }

type ('a, 'b) t =
                       { dir : string ;
                        hash : 'a -> int ;
                    h_caches : 'b h_cache option array ;
                  val_writer : Serial.obuf -> 'a -> unit ;
                  aggregator : ('a, 'b) Aggregator.t ;
                 aggr_reader : Serial.ibuf -> 'b ;
                 aggr_writer : Serial.obuf -> 'b -> unit }

let save_meta t hnum h_cache =
    match h_cache.aggr with
    | Some aggr ->
        let fname = Dbfile.path t.dir hnum h_cache.max_snum ^ ".meta" in
        Serial.with_file_out fname ~trunc:true (fun oc ->
            t.aggr_writer oc aggr)
    | None -> ()

let get_max_snum dir =
    try (
        Sys.readdir dir |>
        Array.fold_left (fun prev name ->
            try max prev (int_of_string name)
            with Failure _ -> prev) 0
    ) with Sys_error _ -> 0

let fsize tdir hnum snum =
    let fname = Dbfile.path tdir hnum snum in
    Unix.(
        try (stat fname).st_size
        with Unix_error (ENOENT, _, _) -> 0
    )

let rotate t hnum h_cache =
    Dbfile.close ?prev:h_cache.file t.dir hnum h_cache.max_snum ;
    h_cache.max_snum <- h_cache.max_snum + 1 ;
    h_cache.aggr <- None

let create_h_cache t hnum =
    let dir = Dbfile.dir t.dir hnum in
    mkdir_all dir ;
    let max_snum = get_max_snum dir in
    { max_snum ;
      file = None ;
      aggr = read_meta t.dir hnum max_snum t.aggr_reader ;
      last_checkpoint = Unix.time () }

let create dir hash val_writer aggregator aggr_reader aggr_writer =
    { dir ; hash ;
      h_caches = Array.create max_hash_size None ;
      val_writer ;
      aggregator ; aggr_reader ; aggr_writer }

let fsize_cap = ref 0   (* to cut down number of fsize call *)

let append t x =
    let hnum = t.hash x mod max_hash_size in
    let h_cache = match t.h_caches.(hnum) with
        | Some s -> s
        | None ->
            let s = create_h_cache t hnum in
            t.h_caches.(hnum) <- Some s ;
            s in
    (* rotate this dbfile? *)
    incr fsize_cap ;
    if !fsize_cap > 10 then (
        fsize_cap := 0 ;
        if fsize t.dir hnum h_cache.max_snum >= max_file_size then (
            save_meta t hnum h_cache ;
            rotate t hnum h_cache
        )
    ) ;
    let file, oc = Dbfile.get ?prev:h_cache.file t.dir hnum h_cache.max_snum in
    h_cache.file <- Some file ;
    t.val_writer oc x ;
    h_cache.aggr <- Some (t.aggregator x h_cache.aggr) ;
    (* Save the meta file once in a while so that readers can have
     * up to date description of last snum *)
    let now = Unix.time () in
    if now > h_cache.last_checkpoint +. 10. then (
        h_cache.last_checkpoint <- now ;
        save_meta t hnum h_cache
    )


let close t =
    Array.iteri (fun hnum h_opt -> match h_opt with
        | None -> ()
        | Some h_cache ->
            save_meta t hnum h_cache ;
            Dbfile.close ?prev:h_cache.file t.dir hnum h_cache.max_snum ;
        ) t.h_caches ;
    Array.fill t.h_caches 0 (Array.length t.h_caches) None

