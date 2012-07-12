open Bricabrac
open Datatype

(* FIXME: factorize all this with DNS *)

let verbose = ref false

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

(* Lod0: Traffic stats for periods of 30s, with fields:
  TS1, TS2, count, vlan, src mac, dst mac, proto, eth payload, eth mtu, src ip, dst ip, ip proto, ip payload *)

module Bounds64 = Tuple2.Make (Integer64) (Integer64)
module BoundsTS = Tuple2.Make (Timestamp) (Timestamp)

module Traffic =
struct
    include Altern1 (Tuple13.Make (Timestamp) (Timestamp) (Integer64) (Integer16) (EthAddr) (EthAddr) (Integer16) (Integer64) (Integer16) (InetAddr) (InetAddr) (Integer8) (Integer64))
    (* We hash on the source IP *)
    let hash_on_src (_ts1, _ts2, _count, _vlan, _mac_src, _mac_dst, _proto, _pld, _mtu, ip_src, _ip_dst, _ip_proto, _ip_pld) = InetAddr.hash ip_src
    (* Metafile stores timestamp range *)
    let meta_aggr (ts1, ts2, _count, _vlan, _mac_src, _mac_dst, _proto, _pld, _mtu, _ip_src, _ip_dst, _ip_proto, _ip_pld) bound_opt =
        let bound = Aggregator.bounds ~lt:Timestamp.lt ts1 bound_opt in
        Aggregator.bounds ~lt:Timestamp.lt ts2 (Some bound)
    let meta_read = BoundsTS.read
    let meta_write = BoundsTS.write
    let table_name dbdir name = dbdir ^ "/" ^ name
    let table dbdir name =
        Table.create (table_name dbdir name)
            hash_on_src write
            meta_aggr meta_read meta_write

    (* Function to query the Lod0, ie select a set of individual queries *)
    (* TODO: filters on IP addresses/proto *)
    let dump ?start ?stop ?vlan ?source ?dest ?proto dbdir name f =
        let tdir = table_name dbdir name in
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
                    Table.iter_file tdir hnum snum read (fun ((ts1, ts2, _count, vl, mac_src, mac_dst, mac_prot, _pld, _mtu, _ip_src, _ip_dst, _ip_proto, _ip_pld) as x) ->
                        if check start  (fun start -> not (ts2 <<< start)) &&
                           check stop   (fun stop  -> not (stop <<< ts1)) &&
                           check source (fun mac   -> EthAddr.equal mac mac_src) &&
                           check dest   (fun mac   -> EthAddr.equal mac mac_dst) &&
                           check proto  (fun proto -> proto = mac_prot) &&
                           check vlan   (fun vlan  -> vlan = vl) then
                           f x))) in
        match dest with
        | Some dst ->
            if !verbose then Printf.fprintf stderr "Using index\n" ;
            (* We have an index for this! Build the list of hnums *)
            let hnum = EthAddr.hash dst mod Table.max_hash_size in
            iter_hnum hnum
        | _ ->
            Table.iter_hnums tdir iter_hnum

end

(* Lod1: Accumulated over 10mins *)
(* Lod2: round timestamp to hour *)
(* Load new data into the database *)

let load dbdir create fname =

    if not create && not (try Sys.is_directory dbdir with Sys_error _ -> false) then (
        failwith (Printf.sprintf "Directory %s does not exist" dbdir)
    ) ;

    let accum_pkts ((count, eth_pld, mtu, ip_pld) as v) = function
        | None -> v
        | Some (count', eth_pld', mtu', ip_pld') ->
            Int64.add count count',
            Int64.add eth_pld eth_pld',
            max mtu mtu',
            Int64.add ip_pld ip_pld' in

    let table2 = Traffic.table dbdir "1hour" in
    let accum2, flush2 =
        Aggregator.accum (once_every 1_000) accum_pkts
            [ fun (start, stop, vlan, mac_src, mac_dst, mac_proto, ip_src, ip_dst, ip_proto) (count, eth_pld, mtu, ip_pld) ->
                Table.append table2 (start, stop, count, vlan, mac_src, mac_dst, mac_proto, eth_pld, mtu, ip_src, ip_dst, ip_proto, ip_pld) ] in

    let table1 = Traffic.table dbdir "10mins" in
    let accum1, flush1 =
        Aggregator.accum (once_every 1_000) accum_pkts
            [ fun (start, stop, vlan, mac_src, mac_dst, mac_proto, ip_src, ip_dst, ip_proto) (count, eth_pld, mtu, ip_pld) ->
                Table.append table1 (start, stop, count, vlan, mac_src, mac_dst, mac_proto, eth_pld, mtu, ip_src, ip_dst, ip_proto, ip_pld) ;
                let start = round_timestamp 3600 start
                and stop = round_timestamp ~ceil:true 3600 stop in
                accum2 (start, stop, vlan, mac_src, mac_dst, mac_proto, ip_src, ip_dst, ip_proto) (count, eth_pld, mtu, ip_pld) ] in

    let table0 = Traffic.table dbdir "30secs" in

    let append0 ((start, stop, count, vlan, mac_src, mac_dst, mac_proto, eth_pld, mtu, ip_src, ip_dst, ip_proto, ip_pld) as v) =
        Table.append table0 v ;
        let start = round_timestamp 600 start
        and stop = round_timestamp ~ceil:true 600 stop in
        accum1 (start, stop, vlan, mac_src, mac_dst, mac_proto, ip_src, ip_dst, ip_proto) (count, eth_pld, mtu, ip_pld) in

    let flush_all () =
        if !verbose then Printf.printf "Flushing...\n" ;
        flush1 () ;
        flush2 () ;
        Table.close table0 ;
        Table.close table1 ;
        Table.close table2 in

    Sys.(List.iter
        (fun s -> set_signal s (Signal_handle (fun _ -> flush_all ())))
        [ sigabrt; sigfpe; sigill; sigint;
          sigpipe; sigquit; sigsegv; sigterm ]) ;

    let lineno = ref 0 in
    try_finalize (fun () ->
        with_file_in fname (fun ic ->
            let ic = TxtInput.from_file ic in
            try forever (fun () ->
                Traffic.read_txt ic |> append0 ;
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
    and source = ref None and dest = ref None and vlan = ref None
    and proto = ref None and create = ref false in
    Arg.(parse [
        "-dir", Set_string dbdir, "database directory (or './')" ;
        "-create", Set create, "create db if it does not exist yet" ;
        "-load", String (fun s -> load !dbdir !create s), "load a CSV file" ;
        "-verbose", Set verbose, "verbose" ;
        "-j", Set_int Table.ncores, "number of cores (default: 1)" ;
        "-dump", String (function tbname -> Traffic.(dump ?start:!start ?stop:!stop ?proto:!proto
                                                          ?vlan:!vlan ?source:!source ?dest:!dest !dbdir tbname
                                                          (fun x -> write_txt Output.stdout x ; print_newline ()))), "dump this table" ;
        "-start", String (fun s -> start := Some (Timestamp.of_string s)), "limit queries to timestamps after this" ;
        "-stop",  String (fun s -> stop  := Some (Timestamp.of_string s)), "limit queries to timestamps before this" ;
        "-vlan", String (fun s -> vlan := Some (Integer16.of_string s)), "limit queries to this VLAN" ;
        "-proto", String (fun s -> proto := Some (Integer16.of_string s)), "select only queries with this protocol" ;
        "-source", String (fun s -> source := Some (EthAddr.of_string s)), "limit to these sources" ;
        "-dest", String (fun s -> dest := Some (EthAddr.of_string s)), "limit to these dests" ]
        (fun x -> raise (Bad x))
        "Operate the traffic DB")

