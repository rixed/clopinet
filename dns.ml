open Bricabrac
open Datatype

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


(* Lod0: the full request record *)

module Bounds64 = Tuple2 (Integer64) (Integer64)
module BoundsTS = Tuple2 (Timestamp) (Timestamp)

module Dns0 =
struct
    include Altern1 (Tuple6 (InetAddr) (InetAddr) (Integer8) (Timestamp) (Integer32) (Text))
    let name = "DNS-query"
    (* We hash on the server IP *)
    let hash_on_srv (_clt, srv, _err, _ts, _rt, _name) = InetAddr.hash srv
    (* Metafile stores timestamp range *)
    let meta_aggr (_clt, _srv, _err, ts, _rt, _name) = Aggregator.bounds ~lt:Timestamp.lt ts
    let meta_read = BoundsTS.read
    let meta_write = BoundsTS.write
    let table_name dbdir = dbdir ^ "/" ^ name
    let table dbdir =
        Table.create (table_name dbdir)
            hash_on_srv read write
            meta_aggr meta_read meta_write
    let dump dbdir =
        Table.iter (table_name dbdir) read (fun x ->
            write_txt stdout x ;
            print_newline ())
end

(* Lod1: degraded client, rounded query_date (to 1min), stripped query_name, distribution of resptimes *)

module Dns1 =
struct
    include Altern1 (Tuple5 (Cidr) (InetAddr) (Integer8) (Integer64) (Distribution))
    let name = "DNS-over-1min"
    (* We hash on the server IP, so that looking for a given IP is faster *)
    let hash_on_srv (_clt, srv, _err, _ts, _rt) = InetAddr.hash srv
    (* Metafile stores timestamp range so that looking of a time period is faster *)
    let meta_aggr (_clt, _srv, _err, ts, _rt) = Aggregator.bounds ~lt:Integer64.lt ts
    let meta_read = Bounds64.read
    let meta_write = Bounds64.write
    let table_name dbdir = dbdir ^ "/" ^ name
    let table dbdir =
        Table.create (table_name dbdir)
            hash_on_srv read write
            meta_aggr meta_read meta_write
    let dump dbdir =
        Table.iter (table_name dbdir) read (fun x ->
            write_txt stdout x ;
            print_newline ())
end

(* Lod2: round timestamp to 10 mins *)

module Dns2 =
struct
    include Altern1 (Tuple5 (Cidr) (InetAddr) (Integer8) (Integer64) (Distribution))
    let name = "DNS-over-10min"
    let table_name dbdir = dbdir ^ "/" ^ name
    let table dbdir =
        Table.create (table_name dbdir)
            Dns1.hash_on_srv read write
            Dns1.meta_aggr Dns1.meta_read Dns1.meta_write
    let dump dbdir =
        Table.iter (table_name dbdir) read (fun x ->
            write_txt stdout x ;
            print_newline ())
end

(* Lod3: and finally to hour *)

module Dns3 =
struct
    include Altern1 (Tuple5 (Cidr) (InetAddr) (Integer8) (Integer64) (Distribution))
    let name = "DNS-over-1hour"
    let table_name dbdir = dbdir ^ "/" ^ name
    let table dbdir =
        Table.create (table_name dbdir)
            Dns1.hash_on_srv read write
            Dns1.meta_aggr Dns1.meta_read Dns1.meta_write
    let dump dbdir =
        Table.iter (table_name dbdir) read (fun x ->
            write_txt stdout x ;
            print_newline ())
end

let load dbdir fname =

    let table3 = Dns3.table dbdir in
    let accum3, flush3 =
        Aggregator.accum (once_every 10_000)
                         Distribution.combine
                         [ fun (clt, srv, err, ts) distr ->
                              Table.append table3 (clt, srv, err, ts, distr) ] in

    let table2 = Dns2.table dbdir in
    let accum2, flush2 =
        Aggregator.accum (once_every 10_000)
                         Distribution.combine
                         [ fun (clt, srv, err, ts) distr ->
                              Table.append table2 (clt, srv, err, ts, distr) ;
                              let ts = round_sec 3600 ts in
                              accum3 (clt, srv, err, ts) distr ] in

    let table1 = Dns1.table dbdir in
    let accum1, flush1 =
        Aggregator.accum (once_every 10_000)
                         Distribution.distr
                         [ fun (clt, srv, err, ts) distr ->
                              Table.append table1 (clt, srv, err, ts, distr) ;
                              let ts = round_sec 600 ts in
                              accum2 (clt, srv, err, ts) distr ] in

    let table0 = Dns0.table dbdir in

    let append0 ((clt, srv, err, ts, rt, _name) as v) =
        Table.append table0 v ;
        let clt = cidr_of_inetaddr subnets clt
        and ts = round_timestamp 60 ts in
        accum1 (clt, srv, err, ts) (Int32.to_float rt) in

    let lineno = ref 0 in
    with_file_in fname (fun ic ->
        let ic = TxtInput.from ic in
        try forever (fun () ->
            Dns0.read_txt ic |> append0 ;
            let eol = TxtInput.read ic in
            assert (eol = '\n' || eol = '\r') ;
            incr lineno) ()
        with End_of_file ->
            if !verbose then Printf.printf "Inserted %d lines\n" !lineno) ;

    flush1 () ;
    flush2 () ;
    flush3 () ;
    Table.close table0 ;
    Table.close table1

let main =
    let dbdir = ref "./" in
    Arg.(parse [
        "-dir", Set_string dbdir, "database directory (or './')" ;
        "-load", String (fun s -> load !dbdir s), "load a CSV file" ;
        "-verbose", Set verbose, "verbose" ;
        "-dump", Int (function 0 -> Dns0.dump !dbdir
                             | 1 -> Dns1.dump !dbdir
                             | 2 -> Dns2.dump !dbdir
                             | 3 -> Dns2.dump !dbdir
                             | x -> raise (Bad ("Bad LOD: "^string_of_int x))), "dump content of Lod n" ]
        (fun x -> raise (Bad x))
        "Operate the DNS response times DB")

