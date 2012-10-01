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

module Bounds64 = Tuple2.Make (UInteger64) (UInteger64)
module BoundsTS = Tuple2.Make (Timestamp) (Timestamp)

module Traffic =
struct
    include Altern1 (Tuple16.Make (Timestamp) (Timestamp)          (* start, stop *)
                                  (UInteger64)                     (* packet count *)
                                  (Option (UInteger16)) (EthAddr) (EthAddr) (* Eth vlan, source, dest *)
                                  (UInteger16)                     (* Eth proto *)
                                  (UInteger64)                     (* Eth payload *)
                                  (UInteger16)                     (* Eth MTU *)
                                  (InetAddr) (InetAddr)            (* IP source, dest *)
                                  (UInteger8)                      (* IP proto *)
                                  (UInteger64)                     (* IP payload *)
                                  (UInteger16) (UInteger16)        (* Port source, dest *)
                                  (UInteger64)                     (* L4 payload *))
    (* We hash on the source IP *)
    let hash_on_src (_ts1, _ts2, _count, _vlan, _mac_src, _mac_dst, _proto, _pld, _mtu, ip_src, _ip_dst, _ip_proto, _ip_pld, _l4_src, _l4_dst, _l4_pld) =
        InetAddr.hash ip_src
    (* Metafile stores timestamp range *)
    let meta_aggr (ts1, ts2, _count, _vlan, _mac_src, _mac_dst, _proto, _pld, _mtu, _ip_src, _ip_dst, _ip_proto, _ip_pld, _l4_src, _l4_dst, _l4_pld) bound_opt =
        let bound = Aggregator.bounds ~cmp:Timestamp.compare ts1 bound_opt in
        Aggregator.bounds ~cmp:Timestamp.compare ts2 (Some bound)
    let meta_read = BoundsTS.read
    let meta_write = BoundsTS.write
    let table_name dbdir name = dbdir ^ "/" ^ name
    let table dbdir name =
        Table.create (table_name dbdir name)
            hash_on_src write
            meta_aggr meta_read meta_write

    (* TODO: filters on IP addresses/proto and ports *)
    let iter ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_proto dbdir name f =
        let tdir = table_name dbdir name in
        let check vopt f = match vopt with
            | None -> true
            | Some v -> f v in
        let iter_hnum hnum =
            Table.iter_snums tdir hnum meta_read (fun snum bounds ->
                let cmp = Timestamp.compare in
                let scan_it = match bounds with
                    | None -> true
                    | Some (ts1, ts2) ->
                        check start (fun start -> not (cmp ts2 start < 0)) &&
                        check stop  (fun stop  -> not (cmp stop ts1 < 0)) in
                if scan_it then (
                    Table.iter_file tdir hnum snum read (fun ((ts1, ts2, _, vl, mac_src', mac_dst', mac_prot, _, _, _, _, ip_prot, _, _, _, _) as x) ->
                        if check start     (fun start -> not (cmp ts2 start < 0)) &&
                           check stop      (fun stop  -> not (cmp stop ts1 < 0)) &&
                           check mac_src   (fun mac   -> EthAddr.equal mac mac_src') &&
                           check mac_dst   (fun mac   -> EthAddr.equal mac mac_dst') &&
                           check eth_proto (fun proto -> proto = mac_prot) &&
                           check ip_proto  (fun proto -> proto = ip_prot) &&
                           check vlan      (fun vlan  -> vl = Some vlan) then   (* FIXME: we have no way to filter on unset vlan only *)
                           f x))) in
        match mac_dst with
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

    let fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_proto dbdir name f fst merge =
        let tdir = table_name dbdir name in
        let check vopt f = match vopt with
            | None -> true
            | Some v -> f v in
        let fold_hnum hnum fst =
            Table.fold_snums tdir hnum meta_read (fun snum bounds prev ->
                let cmp = Timestamp.compare in
                let scan_it = match bounds with
                    | None -> true
                    | Some (ts1, ts2) ->
                        check start (fun start -> not (cmp ts2 start < 0)) &&
                        check stop  (fun stop  -> not (cmp stop ts1 < 0)) in
                let res =
                    if scan_it then (
                        Table.fold_file tdir hnum snum read (fun ((ts1, ts2, _, vl, mac_src', mac_dst', mac_prot, _, _, _, _, ip_prot, _, _, _, _) as x) prev ->
                            if check start     (fun start -> not (cmp ts2 start < 0)) &&
                               check stop      (fun stop  -> not (cmp stop ts1 < 0)) &&
                               check mac_src   (fun mac   -> EthAddr.equal mac mac_src') &&
                               check mac_dst   (fun mac   -> EthAddr.equal mac mac_dst') &&
                               check eth_proto (fun proto -> proto = mac_prot) &&
                               check ip_proto  (fun proto -> proto = ip_prot) &&
                               check vlan      (fun vlan  -> vl = Some vlan) then
                               f x prev
                            else prev)
                            prev
                    ) else (
                        prev
                    ) in
                res)
                fst merge in
        match mac_dst with
        | Some dst ->
            if !verbose then Printf.fprintf stderr "Using index\n" ;
            (* We have an index for this! Build the list of hnums *)
            let hnum = EthAddr.hash dst mod Table.max_hash_size in
            fold_hnum hnum fst
        | _ ->
            Table.fold_hnums tdir fold_hnum fst merge
end

module EthKey = Tuple4.Make (Option (UInteger16)) (EthAddr) (EthAddr) (UInteger16) (* Eth vlan, source, dest, proto *)
module EthPld = Plot.TimeGraph (Traffic) (EthKey)

(* Plot amount of traffic (eth_pld) against time, with a different plot per MAC sockpair *)
let eth_plot_vol_time ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_proto step dbdir name =
    let label_of_key (vlan, mac_src, mac_dst, mac_proto) =
        (match vlan with Some vl -> "vlan:"^string_of_int vl^"," | None -> "")^
        (EthAddr.to_string mac_src)^"->"^
        (EthAddr.to_string mac_dst)^","^
        (string_of_int mac_proto) in
    let fold = Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_proto dbdir name in
    let extract (ts1, ts2, _, vlan, mac_src, mac_dst, mac_proto, mac_pld, _, _, _, _, _, _, _, _) =
        (vlan, mac_src, mac_dst, mac_proto), ts1, ts2, Int64.to_float mac_pld in
    EthPld.plot step fold extract label_of_key

module IPKey = Tuple2.Make (InetAddr) (InetAddr) (* source, dest *)
module IPPld = Plot.TimeGraph (Traffic) (IPKey)

(* Plot amount of traffic (eth_pld) against time, with a different plot per IP sockpair *)
let ip_plot_vol_time ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_proto step dbdir name =
    let label_of_key (src, dst) =
        (InetAddr.to_string src)^"->"^(InetAddr.to_string dst) in
    let fold = Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_proto dbdir name in
    let extract (ts1, ts2, _, _, _, _, _, mac_pld, _, src, dst, _, _, _, _, _) =
        (src, dst), ts1, ts2, Int64.to_float mac_pld in
    IPPld.plot step fold extract label_of_key

(* FIXME: app should be a string, and we should also report various eth apps *)
module AppKey = Tuple2.Make (UInteger8) (UInteger16)
module AppPld = Plot.TimeGraph (Traffic) (AppKey)

(* Plot amount of traffic (eth_pld) against time, with a different plot per proto/port *)
let app_plot_vol_time ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_proto step dbdir name =
    let label_of_key (proto, port) =
        let proto = try Unix.((getprotobynumber proto).p_name)
                    with Not_found -> "" in
        let serv = try Unix.((getservbyport port proto).s_name)
                   with Not_found -> string_of_int port in
        if String.length proto = 0 then serv
        else if String.length serv = 0 then proto
        else proto ^ "," ^ serv in
    let fold = Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_proto dbdir name in
    let extract (ts1, ts2, _, _, _, _, _, mac_pld, _, _, _, proto, _, _, port, _) =
        (proto, port), ts1, ts2, Int64.to_float mac_pld in
    AppPld.plot step fold extract label_of_key

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


