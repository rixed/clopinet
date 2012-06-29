open Bricabrac
open Datatype

(* FIXME: factorize all this with DNS *)

let verbose = ref false

let subnets =
    List.map (fun (s, w) -> Unix.inet_addr_of_string s, w)
        [ "0.0.0.0", 2 ; "64.0.0.0", 2 ; "128.0.0.0", 2 ; "192.0.0.0", 2 ]

(* We need a function to tell us when to flush accumulated data. This can be any
   function, here is just a trivial example based on data size: *)
let once_every n =
    let count = ref 0 in
    fun _k _v ->
        incr count ;
        if !count >= n then (
            count := 0 ;
            true
        ) else false

(* another example: flush as soon as the passed value changes (according to the eq function,
   which if free to compare only a given field of x... *)
let when_change eq =
    let prev = ref None in
    fun k v -> match !prev with
        | None -> prev := Some (k, v) ; false
        | Some p when eq p (k, v) -> false
        | _ -> prev := Some (k, v) ; true


(* Lod0: the full request record: client, server, method (int), status-code, ts, rt, host, url *)

module Bounds64 = Tuple2 (Integer64) (Integer64)
module BoundsTS = Tuple2 (Timestamp) (Timestamp)

module Web0 =
struct
    include Altern1 (Tuple8 (InetAddr) (InetAddr) (Integer8) (Integer16) (Timestamp) (Integer32) (Text) (Text))
    let name = "queries"
    (* We hash on the server IP *)
    let hash_on_srv (_clt, srv, _method, _err, _ts, _rt, _host, _url) = InetAddr.hash srv
    (* Metafile stores timestamp range *)
    let meta_aggr (_clt, _srv, _method, _err, ts, _rt, _host, _url) = Aggregator.bounds ~lt:Timestamp.lt ts
    let meta_read = BoundsTS.read
    let meta_write = BoundsTS.write
    let table_name dbdir = dbdir ^ "/" ^ name
    let table dbdir =
        Table.create (table_name dbdir)
            hash_on_srv write
            meta_aggr meta_read meta_write

    (* Function to query the Lod0, ie select a set of individual queries *)
    let dump ?start ?stop ?client ?server ?peer ?meth ?status ?host ?url ?rt_min dbdir f =
        let tdir = table_name dbdir in
        let starts_with e s =
            if String.length e > String.length s then false else
            let rec aux eo so =
                if eo >= String.length e then true else e.[eo] = s.[so] && aux (eo+1) (so+1) in
            aux 0 0 in
        let ends_with e s =
            let eo = String.length e - 1 and so = String.length s - 1 in
            if eo > so then false else
            let rec aux eo so =
                if eo < 0 then true else e.[eo] = s.[so] && aux (eo-1) (so-1) in
            aux eo so in
        let check vopt f = match vopt with
            | None -> true
            | Some v -> f v in
        let iter_hnum hnum =
            Table.iter_snums tdir hnum meta_read (fun snum bounds ->
                let (<<<) = Timestamp.lt in
                let scan_it = match bounds with
                    | None -> true
                    | Some (ts1, ts2) ->
                        check start (fun start -> not (ts2 <<< start)) &&
                        check stop  (fun stop  -> not (stop <<< ts1)) in
                if scan_it then (
                    Table.iter_file tdir hnum snum read (fun ((clt, srv, met, err, ts, rt, h, u) as x) ->
                        if check start  (fun start -> not (ts <<< start)) &&
                           check stop   (fun stop  -> not (stop <<< ts)) &&
                           check rt_min (fun rt_m  -> rt > rt_m) &&
                           check client (fun cidr  -> in_cidr clt cidr) &&
                           check server (fun cidr  -> in_cidr srv cidr) &&
                           check peer   (fun cidr  -> in_cidr srv cidr || in_cidr clt cidr) &&
                           check meth   (fun meth  -> meth = met) &&
                           check status (fun st    -> st = err) &&
                           check host   (fun host  -> ends_with host h) &&
                           check url    (fun url   -> starts_with url u) then
                           f x))) in
        match server with
        | Some cidr when subnet_size cidr < Table.max_hash_size ->
            if !verbose then Printf.fprintf stderr "Using index\n" ;
            (* We have an index for this! Build the list of hnums *)
            let visited = Hashtbl.create 977 in
            iter_ips cidr (fun ip ->
                let hnum = InetAddr.hash ip mod Table.max_hash_size in
                if not (Hashtbl.mem visited hnum) then (
                    Hashtbl.add visited hnum true ;
                    iter_hnum hnum
                ))
        | _ ->
            Table.iter_hnums tdir iter_hnum

end

(* Lod1: degraded client, rounded query_date (to 1min), stripped url, distribution of resptimes *)

module Web1 =
struct
    include Altern1 (Tuple7 (Cidr) (InetAddr) (Integer8) (Integer16) (Integer64) (Distribution) (Text))
    let name = "over-1min"
    (* We hash on the server IP, so that looking for a given IP is faster *)
    let hash_on_srv (_clt, srv, _method, _err, _ts, _rt, _host) = InetAddr.hash srv
    (* Metafile stores timestamp range so that looking of a time period is faster *)
    let meta_aggr (_clt, _srv, _method, _err, ts, _rt, _host) = Aggregator.bounds ~lt:Integer64.lt ts
    let meta_read = Bounds64.read
    let meta_write = Bounds64.write
    let table_name dbdir = dbdir ^ "/" ^ name
    let table dbdir =
        Table.create (table_name dbdir)
            hash_on_srv write
            meta_aggr meta_read meta_write
    let dump dbdir f =
        Table.iter (table_name dbdir) read f
end

(* Lod2: round timestamp to 10 mins *)

module Web2 =
struct
    include Altern1 (Tuple7 (Cidr) (InetAddr) (Integer8) (Integer16) (Integer64) (Distribution) (Text))
    let name = "over-10min"
    let table_name dbdir = dbdir ^ "/" ^ name
    let table dbdir =
        Table.create (table_name dbdir)
            Web1.hash_on_srv write
            Web1.meta_aggr Web1.meta_read Web1.meta_write
    let dump dbdir f =
        Table.iter (table_name dbdir) read f
end

(* Lod3: and finally to hour *)

module Web3 =
struct
    include Altern1 (Tuple7 (Cidr) (InetAddr) (Integer8) (Integer16) (Integer64) (Distribution) (Text))
    let name = "over-1hour"
    let table_name dbdir = dbdir ^ "/" ^ name
    let table dbdir =
        Table.create (table_name dbdir)
            Web1.hash_on_srv write
            Web1.meta_aggr Web1.meta_read Web1.meta_write
    let dump dbdir f =
        Table.iter (table_name dbdir) read f
end

(* Load new data into the database *)

let load dbdir create fname =

    if not create && not (try Sys.is_directory dbdir with Sys_error _ -> false) then (
        failwith (Printf.sprintf "Directory %s does not exist" dbdir)
    ) ;

    let table3 = Web3.table dbdir in
    let accum3, flush3 =
        Aggregator.accum (once_every 10_000)
                         Distribution.combine
                         [ fun (clt, srv, met, err, ts, host) distr ->
                              Table.append table3 (clt, srv, met, err, ts, distr, host) ] in

    let table2 = Web2.table dbdir in
    let accum2, flush2 =
        Aggregator.accum (once_every 10_000)
                         Distribution.combine
                         [ fun (clt, srv, met, err, ts, host) distr ->
                              Table.append table2 (clt, srv, met, err, ts, distr, host) ;
                              let ts = round_sec 3600 ts in
                              accum3 (clt, srv, met, err, ts, host) distr ] in

    let table1 = Web1.table dbdir in
    let accum1, flush1 =
        Aggregator.accum (once_every 10_000)
                         Distribution.distr
                         [ fun (clt, srv, met, err, ts, host) distr ->
                              Table.append table1 (clt, srv, met, err, ts, distr, host) ;
                              let ts = round_sec 600 ts in
                              accum2 (clt, srv, met, err, ts, host) distr ] in

    let table0 = Web0.table dbdir in

    let append0 ((clt, srv, met, err, ts, rt, host, _url) as v) =
        Table.append table0 v ;
        let clt = cidr_of_inetaddr subnets clt
        and ts = round_timestamp 60 ts in
        accum1 (clt, srv, met, err, ts, host) (Int32.to_float rt) in

    let flush_all () =
        if !verbose then Printf.printf "Flushing...\n" ;
        flush1 () ;
        flush2 () ;
        flush3 () ;
        Table.close table0 ;
        Table.close table1 ;
        Table.close table2 ;
        Table.close table3 in

    Sys.(List.iter
        (fun s -> set_signal s (Signal_handle (fun _ -> flush_all ())))
        [ sigabrt; sigfpe; sigill; sigint;
          sigpipe; sigquit; sigsegv; sigterm ]) ;

    let lineno = ref 0 in
    try_finalize (fun () ->
        with_file_in fname (fun ic ->
            let ic = TxtInput.from_file ic in
            try forever (fun () ->
                Web0.read_txt ic |> append0 ;
                let eol = TxtInput.read ic in
                assert (eol = '\n' || eol = '\r') ;
                incr lineno) ()
            with End_of_file ->
                if !verbose then Printf.fprintf stderr "Inserted %d lines\n" !lineno
               | e ->
                Printf.fprintf stderr "Error at line %d\n" !lineno ;
                raise e)) ()
        flush_all ()


let main =
    let dbdir = ref "./" and start = ref None and stop = ref None 
    and rt_min = ref None and url = ref None and status = ref None
    and client = ref None and server = ref None and host = ref None
    and peer = ref None and meth = ref None and create = ref false in
    Arg.(parse [
        "-dir", Set_string dbdir, "database directory (or './')" ;
        "-create", Set create, "create db if it does not exist yet" ;
        "-load", String (fun s -> load !dbdir !create s), "load a CSV file" ;
        "-verbose", Set verbose, "verbose" ;
        "-j", Set_int Table.ncores, "number of cores (default: 1)" ;
        "-dump", Int (function 0 -> Web0.(dump ?start:!start ?stop:!stop ?rt_min:!rt_min
                                               ?client:!client ?server:!server ?peer:!peer
                                               ?meth:!meth ?host:!host
                                               ?url:!url ?status:!status !dbdir
                                               (fun x -> write_txt Output.stdout x ; print_newline ()))
                             | 1 -> Web1.(dump !dbdir (fun x -> write_txt Output.stdout x ; print_newline ()))
                             | 2 -> Web2.(dump !dbdir (fun x -> write_txt Output.stdout x ; print_newline ()))
                             | 3 -> Web2.(dump !dbdir (fun x -> write_txt Output.stdout x ; print_newline ()))
                             | x -> raise (Bad ("Bad LOD: "^string_of_int x))), "dump content of Lod n" ;
        "-start", String (fun s -> start := Some (Timestamp.of_string s)), "limit queries to timestamps after this" ;
        "-stop",  String (fun s -> stop  := Some (Timestamp.of_string s)), "limit queries to timestamps before this" ;
        "-rt-min", String (fun s -> rt_min := Some (Integer32.of_string s)), "limit queries to resptimes greater than this" ;
        "-url", String (fun s -> url := Some s), "limit queries to those which URL starts with this" ;
        "-host", String (fun s -> host := Some s), "limit queries to those which host ends with this" ;
        "-status", Int (fun i -> status := Some i), "select only queries with this status code" ;
        "-method", Int (fun i -> meth := Some i), "select only queries with this method code" ;
        "-client", String (fun s -> client := Some (Cidr.of_string s)), "limit to these clients" ;
        "-server", String (fun s -> server := Some (Cidr.of_string s)), "limit to these servers" ;
        "-peer", String (fun s -> peer := Some (Cidr.of_string s)), "limit to these clients or servers" ]
        (fun x -> raise (Bad x))
        "Operate the HTTP response times DB")

