open Bricabrac
open LargeFile

let max_file_size = 100_000
let max_hash_size = 2048

(* READING
   Can be done by multiple programs simultaneously,
   no persistent data involved.
*)

(* The meta file, which accompany every dbfile, stores the aggregation result *)
let read_meta tdir hnum snum aggr_reader =
    let fname = Dbfile.path tdir hnum snum ^ ".meta" in
    try Some (BinInput.with_file_in ~try_gz:false fname aggr_reader)
    with Sys_error _ -> None

let iter_file tdir hnum snum reader f =
    let hnum = hnum mod max_hash_size in
    let fname = Dbfile.path tdir hnum snum in
    BinInput.with_file_in fname (fun ic ->
        try forever (fun () ->
            f (reader ic)) ()
        with End_of_file -> ())

let iter_snums tdir hnum aggr_reader f =
    let hnum = hnum mod max_hash_size in
    try (
        Sys.readdir (Dbfile.dir tdir hnum) |>
        Array.iter (fun name ->
            let name =
                let len = String.length name in
                if len > 3 && name.[len-3] = '.' && name.[len-2] = 'g' && name.[len-1] = 'z'
                then String.sub name 0 (len-3)
                else name in
            try let snum = int_of_string name in
                f snum (read_meta tdir hnum snum aggr_reader)
            with Failure _ -> ())
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

(* WRITING
   We cache index function for the table, current sequence number and
   aggregator for each hnum.
   Because of this cache only one process can write.
   TODO: lock a file to check this *)

type 'b h_cache =
          { mutable max_snum : int ;
                mutable file : int option ;
                mutable aggr : 'b option }

type ('a, 'b) t =
                       { dir : string ;
                      create : bool ;
                        hash : 'a -> int ;
                    h_caches : 'b h_cache option array ;
                  val_writer : out_channel -> 'a -> unit ;
                  aggregator : ('a, 'b) Aggregator.t ;
                 aggr_reader : BinInput.t -> 'b ;
                 aggr_writer : out_channel -> 'b -> unit }

let save_meta t hnum h_cache =
    match h_cache.aggr with
    | Some aggr ->
        let fname = Dbfile.path t.dir hnum h_cache.max_snum ^ ".meta" in
        with_file_out
            ~mode:[Open_wronly;Open_creat;Open_excl;Open_binary]
            ~perm:Dbfile.perm fname (fun oc ->
            t.aggr_writer oc aggr) ;
        h_cache.aggr <- None
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
    h_cache.max_snum <- h_cache.max_snum + 1

let create_h_cache t hnum =
    let dir = Dbfile.dir t.dir hnum in
    if t.create then mkdir_all dir ;
    let max_snum = get_max_snum dir in
    { max_snum ;
      file = None ;
      aggr = read_meta t.dir hnum max_snum t.aggr_reader }

let create ?(create=false) dir hash val_writer aggregator aggr_reader aggr_writer =
    { dir ; create ; hash ;
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
    let file, oc = Dbfile.get ?prev:h_cache.file t.dir hnum h_cache.max_snum false in
    h_cache.file <- Some file ;
    t.val_writer oc x ;
    h_cache.aggr <- Some (t.aggregator x h_cache.aggr)

let close t =
    Array.iteri (fun hnum h_opt -> match h_opt with
        | None -> ()
        | Some h_cache ->
            save_meta t hnum h_cache ;
            Dbfile.close ?prev:h_cache.file t.dir hnum h_cache.max_snum ;
        ) t.h_caches ;
    Array.fill t.h_caches 0 (Array.length t.h_caches) None

