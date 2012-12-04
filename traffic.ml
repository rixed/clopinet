open Bricabrac
open Datatype
open Metric
module Hashtbl = BatHashtbl

(* FIXME: factorize all this with DNS, or generate this? *)

let verbose = ref false

(* Lod0: Traffic stats for periods of 30s, with fields:
  TS1, TS2, count, vlan, src mac, dst mac, proto, eth payload, eth mtu, src ip, dst ip, ip proto, ip payload, src port, dst port, l4 payload *)

module Traffic =
struct
    include Altern1 (Tuple16.Make (Timestamp) (Timestamp)        (* start, stop *)
                                  (UInteger)                     (* packet count *)
                                  (Option (UInteger16)) (EthAddr) (EthAddr) (* Eth vlan, source, dest *)
                                  (UInteger16)                   (* Eth proto *)
                                  (UInteger)                     (* Eth payload *)
                                  (UInteger16)                   (* Eth MTU *)
                                  (InetAddr) (InetAddr)          (* IP source, dest *)
                                  (UInteger8)                    (* IP proto *)
                                  (UInteger)                     (* IP payload *)
                                  (UInteger16) (UInteger16)      (* Port source, dest *)
                                  (UInteger)                     (* L4 payload *))
    (* We'd rather have an inlined reader: *)
    let read ic =
        let tuple16_read ic =
            let t0 = Timestamp.read ic in
            let t1 = Timestamp.read ic in
            let t2 = UInteger.read ic in
            let t3 =
                let o = Serial.deser8 ic in
                if o <> 0 then (
                    assert (o = 1) ;
                    Some (UInteger16.read ic)
                ) else None in
            let t4 = EthAddr.read ic in
            let t5 = EthAddr.read ic in
            let t6 = UInteger16.read ic in
            let t7 = UInteger.read ic in
            let t8 = UInteger16.read ic in
            let t9 = InetAddr.read ic in
            let t10 = InetAddr.read ic in
            let t11 = UInteger8.read ic in
            let t12 = UInteger.read ic in
            let t13 = UInteger16.read ic in
            let t14 = UInteger16.read ic in
            let t15 = UInteger.read ic in
            t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15 in
        let v = Serial.deser8 ic in
        if v <> 0 then Printf.fprintf stderr "bad version: %d\n%!" v ;
        assert (v = 0) ;
        tuple16_read ic

    (* We hash on the source IP *)
    let hash_on_src (_ts1, _ts2, _count, _vlan, _mac_src, _mac_dst, _proto, _pld, _mtu, ip_src, _ip_dst, _ip_proto, _ip_pld, _l4_src, _l4_dst, _l4_pld) =
        InetAddr.hash ip_src

    (* Metafile stores timestamp range of the whole slice duration *)
    let meta_aggr (ts1, ts2, _count, _vlan, _mac_src, _mac_dst, _proto, _pld, _mtu, _ip_src, _ip_dst, _ip_proto, _ip_pld, _l4_src, _l4_dst, _l4_pld) bound_opt =
        let bound = Aggregator.bounds ~cmp:Timestamp.compare ts1 bound_opt in
        Aggregator.bounds ~cmp:Timestamp.compare ts2 (Some bound)
    let meta_read = BoundsTS.read
    let meta_write = BoundsTS.write

    let table dbdir name =
        Table.create (table_name dbdir name)
            hash_on_src write
            meta_aggr meta_read meta_write

    let iter_fname fname f =
        Table.iter_fname fname read f

    let with_meta fname f =
        match Table.read_meta_fname fname meta_read with
        | Some meta -> f meta
        | None -> ()

    let accum_pkts ((count, eth_pld, mtu, ip_pld, l4_pld) as v) = function
        | None -> v
        | Some (count', eth_pld', mtu', ip_pld', l4_pld') ->
            count + count',
            eth_pld + eth_pld',
            max mtu mtu',
            ip_pld + ip_pld',
            l4_pld + l4_pld'

    (* We look for semi-closed time interval [start;stop[, but tuples timestamps are closed [ts1;ts2] *)
    let fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port dbdir name f make_fst merge =
        let tdir = table_name dbdir name in
        let fold_hnum hnum fst =
            Table.fold_snums tdir hnum meta_read (fun snum bounds prev ->
                let cmp = Timestamp.compare in
                let res =
                    if is_within bounds start stop then (
                        Table.fold_file tdir hnum snum read (fun ((ts1, ts2, _, vl, mac_s, mac_d, mac_prot, _, _, ip_s, ip_d, ip_prot, _, l4src, l4dst, _) as x) prev ->
                            (* ocamlopt won't inline check function here, which hurts! *)
                            if check start     (fun start -> cmp ts2 start >= 0) &&
                               check stop      (fun stop  -> cmp stop ts1 > 0) &&
                               check mac_src   (fun mac   -> EthAddr.equal mac mac_s) &&
                               check mac_dst   (fun mac   -> EthAddr.equal mac mac_d) &&
                               check eth_proto (fun proto -> proto = mac_prot) &&
                               check ip_src    (fun cidr  -> in_cidr ip_s cidr) &&
                               check ip_dst    (fun cidr  -> in_cidr ip_d cidr) &&
                               check ip        (fun cidr  -> in_cidr ip_s cidr || in_cidr ip_d cidr) &&
                               check ip_proto  (fun proto -> proto = ip_prot) &&
                               check port      (fun port  -> port = l4src || port = l4dst) &&
                               check vlan      (fun vlan  -> vl = Some vlan) then
                               f x prev
                            else prev)
                            prev
                    ) else (
                        prev
                    ) in
                res)
                fst merge in
        fold_using_indexed ip_src tdir fold_hnum make_fst merge

    let iter ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto dbdir name f =
        let dummy_merge _ _ = () in
        fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto dbdir name (fun x _ -> f x) ignore dummy_merge

end

(* Querries *)

module EthKey = Tuple2.Make (Option (UInteger16)) (EthAddr) (* Eth vlan, source/dest *)
module EthPld = Plot.DataSet (EthKey)
module EthKey2 = Tuple3.Make (Option (UInteger16)) (EthAddr) (EthAddr) (* Eth vlan, source, dest *)
module EthPld2 = Plot.DataSet (EthKey2)

let optmin a b = match a, b with
    | Some a, Some b -> Some (min a b)
    | _ -> a
let optmax a b = match a, b with
    | Some a, Some b -> Some (max a b)
    | _ -> b

type plot_what = PacketCount | Volume

let label_of_eth_key (vlan, mac_src) =
    (match vlan with Some vl -> "vlan:"^string_of_int vl^"," | None -> "")^
    (EthAddr.to_string mac_src)

let label_of_eth_proto = function
    | 0x0800 -> "IPv4"
    | 0x86DD -> "IPv6"
    | 0x0806 -> "ARP"
    | 0x8100 -> "802.1q"
    | _ -> "not IP"

let label_of_ip_key mac_proto ip =
    if mac_proto = 0x0800 || mac_proto = 0x86DD then
        InetAddr.to_string ip
    else label_of_eth_proto mac_proto

(* Returns traffic against time, with a different plot per MAC sockpair *)
let eth_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs by_src what step dbdir name =
    let start, stop = min start stop, max start stop in
    let fold f i m =
        Traffic.fold ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port dbdir name (fun (t1, t2, count, vlan, mac_src, mac_dst, _, mac_pld, _, _, _, _, _, _, _, _) p ->
            f (vlan, if by_src then mac_src else mac_dst) t1 t2 (float_of_int (if what = PacketCount then count else mac_pld)) p)
            i m in
    EthPld.per_time ?max_graphs start stop step fold label_of_eth_key "others"

(* Returns traffic per pair of MACs *)
let eth_plot_vol_tot ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?(max_graphs=20) what dbdir name =
    let start, stop = optmin start stop, optmax start stop in
    let label_of_key (vlan, mac_src, mac_dst) =
        label_of_eth_key (vlan, mac_src),
        label_of_eth_key (vlan, mac_dst) in
    let fold f i m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port dbdir name
            (fun (t1, t2, count, vlan, mac_src, mac_dst, _, mac_pld, _, _, _, _, _, _, _, _) p ->
                let y = float_of_int (if what = PacketCount then count else mac_pld) in
                let _, _, y = Plot.clip_y ?start ?stop t1 t2 y in
                let k = vlan, mac_src, mac_dst in
                f (k, y) p)
            i m
        in
    assert (max_graphs > 1) ;
    let interm = EthPld2.FindSignificant.pass1 fold (max_graphs-1) in
    let result, rest = EthPld2.FindSignificant.pass2' interm fold (max_graphs-1) in
    (* We want to return a hash of src*dst -> value *)
    let h = Hashtbl.create max_graphs in
    EthPld2.Maplot.iter result (fun k v ->
        Hashtbl.add h (label_of_key k) v) ;
    Hashtbl.add h ("other","") rest ;
    h

(* Returns traffic per pair of MACs *)
let eth_plot_vol_top ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?(max_graphs=20) by_src what dbdir name =
    let start, stop = optmin start stop, optmax start stop in
    let fold f i m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port dbdir name
            (fun (t1, t2, count, vlan, mac_src, mac_dst, _, mac_pld, _, _, _, _, _, _, _, _) p ->
                let y = float_of_int (if what = PacketCount then count else mac_pld) in
                let _, _, y = Plot.clip_y ?start ?stop t1 t2 y in
                let k = vlan, if by_src then mac_src else mac_dst in
                f (k, y) p)
            i m
        in
    assert (max_graphs > 1) ;
    let interm = EthPld.FindSignificant.pass1 fold (max_graphs-1) in
    let result, rest = EthPld.FindSignificant.pass2' interm fold (max_graphs-1) in
    (* We want to return a hash of src*dst -> value *)
    let h = Hashtbl.create max_graphs in
    EthPld.Maplot.iter result (fun k v ->
        Hashtbl.add h (label_of_eth_key k) v) ;
    Hashtbl.add h "other" rest ;
    h

(* Returns traffic per pair of MACs *)
let eth_plot_vol_top_both ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?(max_graphs=20) what dbdir name =
    let start, stop = optmin start stop, optmax start stop in
    let label_of_key (vlan, mac_src, mac_dst) =
        label_of_eth_key (vlan, mac_src) ^"\\u2192"^
        EthAddr.to_string mac_dst in
    let fold f i m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port dbdir name
            (fun (t1, t2, count, vlan, mac_src, mac_dst, _, mac_pld, _, _, _, _, _, _, _, _) p ->
                let y = float_of_int (if what = PacketCount then count else mac_pld) in
                let _, _, y = Plot.clip_y ?start ?stop t1 t2 y in
                let k = vlan, mac_src, mac_dst in
                f (k, y) p)
            i m
        in
    assert (max_graphs > 1) ;
    let interm = EthPld2.FindSignificant.pass1 fold (max_graphs-1) in
    let result, rest = EthPld2.FindSignificant.pass2' interm fold (max_graphs-1) in
    (* We want to return a hash of src*dst -> value *)
    let h = Hashtbl.create max_graphs in
    EthPld2.Maplot.iter result (fun k v ->
        Hashtbl.add h (label_of_key k) v) ;
    Hashtbl.add h "other" rest ;
    h

module IPKey = Tuple2.Make (UInteger16) (InetAddr) (* eth proto, source/dest *)
module IPPld = Plot.DataSet (IPKey)
module IPKey2 = Tuple3.Make (UInteger16) (InetAddr) (InetAddr) (* eth proto, source, dest *)
module IPPld2 = Plot.DataSet (IPKey2)

(* Returns traffic against time, with a different plot per IP sockpair *)
let ip_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs by_src what step dbdir name =
    let start, stop = min start stop, max start stop in
    let fold f i m =
        Traffic.fold ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port dbdir name (fun (t1, t2, count, _, _, _, mac_proto, mac_pld, _, src, dst, _, _, _, _, _) p ->
            f (mac_proto, if by_src then src else dst) t1 t2 (float_of_int (if what = PacketCount then count else mac_pld)) p)
            i m in
    IPPld.per_time ?max_graphs start stop step fold (fun (mac_proto, ip) -> label_of_ip_key mac_proto ip) "others"

(* Returns traffic per pair of IPs *)
let ip_plot_vol_tot ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?(max_graphs=20) what dbdir name =
    let start, stop = optmin start stop, optmax start stop in
    let label_of_key (mac_proto, src, dst) =
        label_of_ip_key mac_proto src,
        label_of_ip_key mac_proto dst in
    let fold f i m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port dbdir name
            (fun (t1, t2, count, _, _, _, mac_proto, mac_pld, _, src, dst, _, _, _, _, _) p ->
                let y = float_of_int (if what = PacketCount then count else mac_pld) in
                let _, _, y = Plot.clip_y ?start ?stop t1 t2 y in
                let k = mac_proto, src, dst in
                f (k, y) p)
            i m
        in
    assert (max_graphs > 1) ;
    let interm = IPPld2.FindSignificant.pass1 fold (max_graphs-1) in
    let result, rest = IPPld2.FindSignificant.pass2' interm fold (max_graphs-1) in
    (* We want to return a hash of src*dst -> value *)
    let h = Hashtbl.create max_graphs in
    IPPld2.Maplot.iter result (fun k v ->
        Hashtbl.add h (label_of_key k) v) ;
    Hashtbl.add h ("other","") rest ;
    h

let ip_plot_vol_top ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?(max_graphs=20) by_src what dbdir name =
    let start, stop = optmin start stop, optmax start stop in
    let fold f i m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port dbdir name
            (fun (t1, t2, count, _, _, _, mac_proto, mac_pld, _, src, dst, _, _, _, _, _) p ->
                let y = float_of_int (if what = PacketCount then count else mac_pld) in
                let _, _, y = Plot.clip_y ?start ?stop t1 t2 y in
                let k = mac_proto, if by_src then src else dst in
                f (k, y) p)
            i m
        in
    assert (max_graphs > 1) ;
    let interm = IPPld.FindSignificant.pass1 fold (max_graphs-1) in
    let result, rest = IPPld.FindSignificant.pass2' interm fold (max_graphs-1) in
    (* We want to return a hash of src*dst -> value *)
    let h = Hashtbl.create max_graphs in
    IPPld.Maplot.iter result (fun (mac_proto, ip) v ->
        Hashtbl.add h (label_of_ip_key mac_proto ip) v) ;
    Hashtbl.add h "other" rest ;
    h

let ip_plot_vol_top_both ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?(max_graphs=20) what dbdir name =
    let start, stop = optmin start stop, optmax start stop in
    let label_of_key (mac_proto, src, dst) =
        label_of_ip_key mac_proto src ^"\\u2192"^
        label_of_ip_key mac_proto dst in
    let fold f i m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port dbdir name
            (fun (t1, t2, count, _, _, _, mac_proto, mac_pld, _, src, dst, _, _, _, _, _) p ->
                let y = float_of_int (if what = PacketCount then count else mac_pld) in
                let _, _, y = Plot.clip_y ?start ?stop t1 t2 y in
                let k = mac_proto, src, dst in
                f (k, y) p)
            i m
        in
    assert (max_graphs > 1) ;
    let interm = IPPld2.FindSignificant.pass1 fold (max_graphs-1) in
    let result, rest = IPPld2.FindSignificant.pass2' interm fold (max_graphs-1) in
    (* We want to return a hash of src*dst -> value *)
    let h = Hashtbl.create max_graphs in
    IPPld2.Maplot.iter result (fun k v ->
        Hashtbl.add h (label_of_key k) v) ;
    Hashtbl.add h "other" rest ;
    h

(* FIXME: app should be a string, and we should also report various eth apps *)
module AppKey = Tuple2.Make (UInteger8) (UInteger16)
module AppPld = Plot.DataSet (AppKey)

let label_of_app_key (proto, port) =
    let proto = try Unix.((getprotobynumber proto).p_name)
                with Not_found -> "" in
    let serv = try Unix.((getservbyport port proto).s_name)
               with Not_found -> string_of_int port in
    if String.length proto = 0 then serv
    else if String.length serv = 0 then proto
    else proto ^ "/" ^ serv

(* Returns traffic against time, with a different plot per proto/port *)
let app_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs what step dbdir name =
    let start, stop = min start stop, max start stop in
    let fold f i m =
        Traffic.fold ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port dbdir name (fun (t1, t2, count, _, _, _, _, mac_pld, _, _, _, proto, _, p1, p2, _) p ->
            f (proto, min p1 p2) t1 t2 (float_of_int (if what = PacketCount then count else mac_pld)) p)
            i m in
    AppPld.per_time ?max_graphs start stop step fold label_of_app_key "others"

let app_plot_vol_top ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?(max_graphs=10) what dbdir name =
    let start, stop = optmin start stop, optmax start stop in
    let fold f i m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port dbdir name
            (fun (t1, t2, count, _, _, _, _, mac_pld, _, _, _, proto, _, p1, p2, _) p ->
                let y = float_of_int (if what = PacketCount then count else mac_pld) in
                let _, _, y = Plot.clip_y ?start ?stop t1 t2 y in
                let k = (proto, min p1 p2) in
                f (k, y) p)
            i m
        in
    assert (max_graphs > 1) ;
    let interm = AppPld.FindSignificant.pass1 fold (max_graphs-1) in
    let result, rest = AppPld.FindSignificant.pass2' interm fold (max_graphs-1) in
    (* We want to return a hash of src*dst -> value *)
    let h = Hashtbl.create max_graphs in
    AppPld.Maplot.iter result (fun k v ->
        Hashtbl.add h (label_of_app_key k) v) ;
    Hashtbl.add h "other" rest ;
    h

type netgraph_key = Mac of (int option * EthAddr.t) | Ip of InetAddr.t
let network_graph start stop ?min_volume ?vlan ?eth_proto ?ip_proto ?port show_mac show_ip dbdir name =
    let start, stop = min start stop, max start stop in
    let label_of_key = function
        | Mac x -> label_of_eth_key x
        | Ip x  -> InetAddr.to_string x in
    let fold f i m =
        Traffic.fold ~start ~stop ?vlan ?eth_proto ?ip_proto ?port dbdir name
            (fun (t1, t2, _, vlan, mac_src, mac_dst, mac_proto, mac_pld, _, ip_src, ip_dst, _, _, _, _, _) p ->
                let y = float_of_int mac_pld in
                let _, _, y = Plot.clip_y ~start ~stop t1 t2 y in
                (* FIXME: wait till Plot.netgraph is done before converting keys
                 * to string representation !*)
                let p = if show_mac then (
                        let src, dst, y = if EthAddr.compare mac_src mac_dst <= 0 then mac_src, mac_dst, y
                                                                                  else mac_dst, mac_src, ~-.y in
                        f (Mac (vlan, src)) (Mac (vlan, dst)) y p
                    ) else p in
                if show_ip && (mac_proto = 0x0800 || mac_proto = 0x86DD) then (
                    if show_mac then (
                        (* link Ip to their mac *)
                        let p' = f (Ip ip_src) (Mac (vlan, mac_src)) y p in
                        f (Ip ip_dst) (Mac (vlan, mac_dst)) y p'
                    ) else (
                        (* show direct link between IPs *)
                        let src, dst, y = if InetAddr.compare ip_src ip_dst <= 0 then ip_src, ip_dst, y
                                                                                 else ip_dst, ip_src, ~-.y in
                        f (Ip src) (Ip dst) y p
                    )
                ) else p)
            i m
        in
    let graph = Plot.netgraph fold (+.) in
    let graph =
        match min_volume with
        | None ->
            graph
        | Some min_volume ->
            let min_volume = float_of_int min_volume in
            Hashtbl.filter_map (fun _k1 n ->
                let n' = Hashtbl.filter (fun y -> y >= min_volume) n in
                if Hashtbl.is_empty n' then None
                else Some n') graph in
    (* Replace keys with user friendly labels *)
    let res = Hashtbl.create (Hashtbl.length graph) in
    Hashtbl.iter (fun k1 n ->
        let n' = Hashtbl.create (Hashtbl.length n) in
        Hashtbl.iter (fun k2 y -> Hashtbl.add n' (label_of_key k2) y) n ;
        Hashtbl.add res (label_of_key k1) n')
        graph ;
    res

(* Lod1: Accumulated over 10mins *)
(* Lod2: round timestamp to hour *)
(* Load new data into the database *)

let load dbdir create fname =

    if not create && not (try Sys.is_directory dbdir with Sys_error _ -> false) then (
        failwith (Printf.sprintf "Directory %s does not exist" dbdir)
    ) ;

    let table2 = Traffic.table dbdir "1hour" in
    let accum2, flush2 =
        Aggregator.(accum (now_and_then (2. *. 3600.))) Traffic.accum_pkts
            [ fun (start, stop, vlan, mac_src, mac_dst, mac_proto, ip_src, ip_dst, ip_proto, l4_src, l4_dst) (count, eth_pld, mtu, ip_pld, l4_pld) ->
                Table.append table2
                    (start, stop, count, vlan, mac_src, mac_dst, mac_proto, eth_pld, mtu, ip_src, ip_dst, ip_proto, ip_pld, l4_src, l4_dst, l4_pld) ] in

    let table1 = Traffic.table dbdir "10mins" in
    let accum1, flush1 =
        Aggregator.(accum (now_and_then (2. *. 600.))) Traffic.accum_pkts
            [ fun (start, stop, vlan, mac_src, mac_dst, mac_proto, ip_src, ip_dst, ip_proto, l4_src, l4_dst) (count, eth_pld, mtu, ip_pld, l4_pld) ->
                Table.append table1
                    (start, stop, count, vlan, mac_src, mac_dst, mac_proto, eth_pld, mtu, ip_src, ip_dst, ip_proto, ip_pld, l4_src, l4_dst, l4_pld) ;
                let start, stop = round_time_interval 3600_000L start stop in
                accum2 (start, stop, vlan, mac_src, mac_dst, mac_proto, ip_src, ip_dst, ip_proto, l4_src, l4_dst) (count, eth_pld, mtu, ip_pld, l4_pld) ] in

    let table0 = Traffic.table dbdir "1min" in

    let append0 ((start, stop, count, vlan, mac_src, mac_dst, mac_proto, eth_pld, mtu, ip_src, ip_dst, ip_proto, ip_pld, l4_src, l4_dst, l4_pld) as v) =
        Table.append table0 v ;
        let start, stop = round_time_interval 600_000L start stop in
        accum1 (start, stop, vlan, mac_src, mac_dst, mac_proto, ip_src, ip_dst, ip_proto, l4_src, l4_dst) (count, eth_pld, mtu, ip_pld, l4_pld) in

    let flush_all () =
        if !verbose then Printf.printf "Flushing...\n" ;
        flush1 () ;
        flush2 () ;
        Table.close table0 ;
        Table.close table1 ;
        Table.close table2 in

    load fname Traffic.read_txt append0 flush_all


