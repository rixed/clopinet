open Bricabrac
open Datatype

(* FIXME: factorize all this with DNS, or generate this? *)

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

(* this one never flush *)
let never_flush _k _v = false

(* Lod0: Traffic stats for periods of 30s, with fields:
  TS1, TS2, count, vlan, src mac, dst mac, proto, eth payload, eth mtu, src ip, dst ip, ip proto, ip payload *)

module Bounds64 = Tuple2.Make (Integer64) (Integer64)
module BoundsTS = Tuple2.Make (Timestamp) (Timestamp)

module Traffic =
struct
    include Altern1 (Tuple16.Make (Timestamp) (Timestamp)         (* start, stop *)
                                  (Integer64)                     (* packet count *)
                                  (Integer16) (EthAddr) (EthAddr) (* Eth vlan, source, dest *)
                                  (Integer16)                     (* Eth proto *)
                                  (Integer64)                     (* Eth payload *)
                                  (Integer16)                     (* Eth MTU *)
                                  (InetAddr) (InetAddr)           (* IP source, dest *)
                                  (Integer8)                      (* IP proto *)
                                  (Integer64)                     (* IP payload *)
                                  (Integer16) (Integer16)         (* Port source, dest *)
                                  (Integer64)                     (* L4 payload *))
    (* We hash on the source IP *)
    let hash_on_src (_ts1, _ts2, _count, _vlan, _mac_src, _mac_dst, _proto, _pld, _mtu, ip_src, _ip_dst, _ip_proto, _ip_pld, _l4_src, _l4_dst, _l4_pld) =
        InetAddr.hash ip_src
    (* Metafile stores timestamp range *)
    let meta_aggr (ts1, ts2, _count, _vlan, _mac_src, _mac_dst, _proto, _pld, _mtu, _ip_src, _ip_dst, _ip_proto, _ip_pld, _l4_src, _l4_dst, _l4_pld) bound_opt =
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
    (* TODO: filters on IP addresses/proto and ports *)
    let dump ?start ?stop ?vlan ?source ?dest ?eth_proto ?ip_proto dbdir name f =
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
                    Table.iter_file tdir hnum snum read (fun ((ts1, ts2, _, vl, mac_src, mac_dst, mac_prot, _, _, _, _, ip_prot, _, _, _, _) as x) ->
                        if check start     (fun start -> not (ts2 <<< start)) &&
                           check stop      (fun stop  -> not (stop <<< ts1)) &&
                           check source    (fun mac   -> EthAddr.equal mac mac_src) &&
                           check dest      (fun mac   -> EthAddr.equal mac mac_dst) &&
                           check eth_proto (fun proto -> proto = mac_prot) &&
                           check ip_proto  (fun proto -> proto = ip_prot) &&
                           check vlan      (fun vlan  -> vlan = vl) then
                           f x))) in
        match dest with
        | Some dst ->
            if !verbose then Printf.fprintf stderr "Using index\n" ;
            (* We have an index for this! Build the list of hnums *)
            let hnum = EthAddr.hash dst mod Table.max_hash_size in
            iter_hnum hnum
        | _ ->
            Table.iter_hnums tdir iter_hnum

    let accum_pkts ((count, eth_pld, mtu, ip_pld, l4_pld) as v) = function
        | None -> v
        | Some (count', eth_pld', mtu', ip_pld', l4_pld') ->
            Int64.add count count',
            Int64.add eth_pld eth_pld',
            max mtu mtu',
            Int64.add ip_pld ip_pld',
            Int64.add l4_pld l4_pld'

    let fold ?start ?stop ?vlan ?source ?dest ?eth_proto ?ip_proto dbdir name f fst merge =
        let tdir = table_name dbdir name in
        let check vopt f = match vopt with
            | None -> true
            | Some v -> f v in
        let fold_hnum hnum fst =
            Table.fold_snums tdir hnum meta_read (fun snum bounds prev ->
                let (<<<) = Timestamp.lt in
                let scan_it = match bounds with
                    | None -> true
                    | Some (ts1, ts2) ->
                        check start (fun start -> not (ts2 <<< start)) &&
                        check stop  (fun stop  -> not (stop <<< ts1)) in
                let res =
                    if scan_it then (
                        Table.fold_file tdir hnum snum read (fun ((ts1, ts2, _, vl, mac_src, mac_dst, mac_prot, _, _, _, _, ip_prot, _, _, _, _) as x) prev ->
                            if check start     (fun start -> not (ts2 <<< start)) &&
                               check stop      (fun stop  -> not (stop <<< ts1)) &&
                               check source    (fun mac   -> EthAddr.equal mac mac_src) &&
                               check dest      (fun mac   -> EthAddr.equal mac mac_dst) &&
                               check eth_proto (fun proto -> proto = mac_prot) &&
                               check ip_proto  (fun proto -> proto = ip_prot) &&
                               check vlan      (fun vlan  -> vlan = vl) then
                               f x prev
                            else prev)
                            prev
                    ) else (
                        prev
                    ) in
                res)
                fst merge in
        match dest with
        | Some dst ->
            if !verbose then Printf.fprintf stderr "Using index\n" ;
            (* We have an index for this! Build the list of hnums *)
            let hnum = EthAddr.hash dst mod Table.max_hash_size in
            fold_hnum hnum fst
        | _ ->
            Table.fold_hnums tdir fold_hnum fst merge

    module Maplot = Map.Make (struct
        type t = Timestamp.t * Integer16.t * EthAddr.t * EthAddr.t * Integer16.t
        let compare = Pervasives.compare
    end)

    (* Plot amount of traffic (eth_pld) against time, with a different plot per MAC sockpair *)
    let plot_vol_time ?start ?stop ?vlan ?source ?dest ?eth_proto ?ip_proto dbdir name =
        let label_of vlan mac_src mac_dst mac_proto =
            (if vlan <> -1 then "vlan:"^string_of_int vlan^"," else "")^
            (EthAddr.to_string mac_src)^"->"^
            (EthAddr.to_string mac_dst)^","^
            (string_of_int mac_proto) in
        let cumul_pld m k mac_pld =
            let prev = try Maplot.find k m with Not_found -> 0L in
            Maplot.add k (Int64.add prev mac_pld) m in
        let h =
            fold ?start ?stop ?vlan ?source ?dest ?eth_proto ?ip_proto dbdir name
                (fun (ts1, _ts2, _count, vlan, mac_src, mac_dst, mac_proto, mac_pld, _mtu, _ip_src, _ip_dst, _ip_proto, _ip_pld, _l4_src, _l4_dst, _l4_pld) h ->
                    let t = ts1 (* TODO: (ts1+ts2)/2? *) in
                    let k = t, vlan, mac_src, mac_dst, mac_proto in
                    cumul_pld h k mac_pld)
                Maplot.empty
                (fun h1 h2 -> (* merge two hashes *)
                    (*Printf.printf "Merging two maps of size %d and %d\n%!" (Maplot.cardinal h1) (Maplot.cardinal h2) ;*)
                    Maplot.fold (fun k v m -> cumul_pld m k v) h1 h2) in
        let plot = Hashtbl.create 71 in
        let float_of_timestamp (s, us) = Int64.to_float s +. (float_of_int us) /. 1000000. in
        Maplot.iter (fun (t, vlan, mac_src, mac_dst, mac_proto) pld ->
            let label = label_of vlan mac_src mac_dst mac_proto in
            let prev = try Hashtbl.find plot label with Not_found -> [] in
            Hashtbl.replace plot label ((float_of_timestamp t, Int64.to_float pld)::prev)) h ;
        Plot.stacked_area plot
end

(* Lod1: Accumulated over 10mins *)
(* Lod2: round timestamp to hour *)
(* Load new data into the database *)

let load dbdir create fname =

    if not create && not (try Sys.is_directory dbdir with Sys_error _ -> false) then (
        failwith (Printf.sprintf "Directory %s does not exist" dbdir)
    ) ;

    let table2 = Traffic.table dbdir "1hour" in
    let accum2, flush2 =
        Aggregator.accum (once_every 100_000) Traffic.accum_pkts
            [ fun (start, stop, vlan, mac_src, mac_dst, mac_proto, ip_src, ip_dst, ip_proto, l4_src, l4_dst) (count, eth_pld, mtu, ip_pld, l4_pld) ->
                Table.append table2
                    (start, stop, count, vlan, mac_src, mac_dst, mac_proto, eth_pld, mtu, ip_src, ip_dst, ip_proto, ip_pld, l4_src, l4_dst, l4_pld) ] in

    let table1 = Traffic.table dbdir "10mins" in
    let accum1, flush1 =
        Aggregator.accum (once_every 100_000) Traffic.accum_pkts
            [ fun (start, stop, vlan, mac_src, mac_dst, mac_proto, ip_src, ip_dst, ip_proto, l4_src, l4_dst) (count, eth_pld, mtu, ip_pld, l4_pld) ->
                Table.append table1
                    (start, stop, count, vlan, mac_src, mac_dst, mac_proto, eth_pld, mtu, ip_src, ip_dst, ip_proto, ip_pld, l4_src, l4_dst, l4_pld) ;
                let start = round_timestamp 3600 start
                and stop = round_timestamp ~ceil:true 3600 stop in
                accum2 (start, stop, vlan, mac_src, mac_dst, mac_proto, ip_src, ip_dst, ip_proto, l4_src, l4_dst) (count, eth_pld, mtu, ip_pld, l4_pld) ] in

    let table0 = Traffic.table dbdir "30secs" in

    let append0 ((start, stop, count, vlan, mac_src, mac_dst, mac_proto, eth_pld, mtu, ip_src, ip_dst, ip_proto, ip_pld, l4_src, l4_dst, l4_pld) as v) =
        Table.append table0 v ;
        let start = round_timestamp 600 start
        and stop = round_timestamp ~ceil:true 600 stop in
        accum1 (start, stop, vlan, mac_src, mac_dst, mac_proto, ip_src, ip_dst, ip_proto, l4_src, l4_dst) (count, eth_pld, mtu, ip_pld, l4_pld) in

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
    and eth_proto = ref None and ip_proto = ref None and create = ref false in
    Arg.(parse [
        "-dir", Set_string dbdir, "database directory (or './')" ;
        "-create", Set create, "create db if it does not exist yet" ;
        "-load", String (fun s -> load !dbdir !create s), "load a CSV file" ;
        "-verbose", Set verbose, "verbose" ;
        "-j", Set_int Table.ncores, "number of cores (default: 1)" ;
        "-dump", String (function tbname -> Traffic.(dump ?start:!start ?stop:!stop ?eth_proto:!eth_proto ?ip_proto:!ip_proto
                                                          ?vlan:!vlan ?source:!source ?dest:!dest !dbdir tbname
                                                          (fun x -> write_txt Output.stdout x ; print_newline ()))), "dump this table" ;
        "-plot", String (function tbname -> Traffic.(plot_vol_time ?start:!start ?stop:!stop ?eth_proto:!eth_proto ?ip_proto:!ip_proto
                                                          ?vlan:!vlan ?source:!source ?dest:!dest !dbdir tbname)), "plot this table" ;
        "-start", String (fun s -> start := Some (Timestamp.of_string s)), "limit queries to timestamps after this" ;
        "-stop",  String (fun s -> stop  := Some (Timestamp.of_string s)), "limit queries to timestamps before this" ;
        "-vlan", String (fun s -> vlan := Some (Integer16.of_string s)), "limit queries to this VLAN" ;
        "-eth-proto", String (fun s -> eth_proto := Some (Integer16.of_string s)), "select only queries with this Eth protocol" ;
        "-ip-proto", String (fun s -> ip_proto := Some (Integer16.of_string s)), "select only queries with this IP protocol" ;
        "-source", String (fun s -> source := Some (EthAddr.of_string s)), "limit to these sources" ;
        "-dest", String (fun s -> dest := Some (EthAddr.of_string s)), "limit to these dests" ]
        (fun x -> raise (Bad x))
        "Operate the traffic DB")

