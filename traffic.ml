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
  TS1, TS2, count, vlan, src mac, dst mac, proto, eth payload, eth mtu, src ip, dst ip, ip proto, ip payload, src port, dst port, l4 payload *)

module BoundsTS = Tuple2.Make (Timestamp) (Timestamp)

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

    let iter_fname fname f =
        Table.iter_fname fname read f

    let accum_pkts ((count, eth_pld, mtu, ip_pld, l4_pld) as v) = function
        | None -> v
        | Some (count', eth_pld', mtu', ip_pld', l4_pld') ->
            count + count',
            eth_pld + eth_pld',
            max mtu mtu',
            ip_pld + ip_pld',
            l4_pld + l4_pld'

    (* TODO: filters on ports *)
    (* We look for semi-closed time interval [start;stop[, but tuples timestamps are closed [ts1;ts2] *)
    let fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto dbdir name f fst copy merge =
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
                        check start (fun start -> cmp ts2 start >= 0) &&
                        check stop  (fun stop  -> cmp stop ts1 > 0) in
                let res =
                    if scan_it then (
                        Table.fold_file tdir hnum snum read (fun ((ts1, ts2, _, vl, mac_s, mac_d, mac_prot, _, _, ip_s, ip_d, ip_prot, _, _, _, _) as x) prev ->
                            if check start     (fun start -> cmp ts2 start >= 0) &&
                               check stop      (fun stop  -> cmp stop ts1 > 0) &&
                               check mac_src   (fun mac   -> EthAddr.equal mac mac_s) &&
                               check mac_dst   (fun mac   -> EthAddr.equal mac mac_d) &&
                               check eth_proto (fun proto -> proto = mac_prot) &&
                               check ip_src    (fun cidr  -> in_cidr ip_s cidr) &&
                               check ip_dst    (fun cidr  -> in_cidr ip_d cidr) &&
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
        match ip_src with
        | Some cidr when subnet_size cidr < Table.max_hash_size ->
            if !verbose then Printf.fprintf stderr "Using index\n" ;
            (* We have an index for this! Build the list of hnums *)
            let visited = Hashtbl.create 977 in
            fold_ips cidr (fun ip p ->
                let hnum = InetAddr.hash ip mod Table.max_hash_size in
                if Hashtbl.mem visited hnum then (
                    p
                ) else (
                    Hashtbl.add visited hnum true ;
                    fold_hnum hnum p
                )) fst
        | _ ->
            Table.fold_hnums tdir fold_hnum fst copy merge

    let iter ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto dbdir name f =
        let dummy_merge _ _ = () in
        fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto dbdir name (fun x _ -> f x) () ignore dummy_merge

end

(* Querries *)

module EthKey = Tuple2.Make (Option (UInteger16)) (EthAddr) (* Eth vlan, source/dest *)
module EthPld = Plot.DataSet (EthKey)
module EthKey2 = Tuple3.Make (Option (UInteger16)) (EthAddr) (EthAddr) (* Eth vlan, source, dest *)
module EthPld2 = Plot.DataSet (EthKey2)

type plot_what = PacketCount | Volume

let label_of_eth_key (vlan, mac_src) =
    (match vlan with Some vl -> "vlan:"^string_of_int vl^"," | None -> "")^
    (EthAddr.to_string mac_src)

(* Returns traffic against time, with a different plot per MAC sockpair *)
let eth_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?max_graphs by_src what step dbdir name =
    let fold f i c m =
        Traffic.fold ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto dbdir name (fun (t1, t2, count, vlan, mac_src, mac_dst, _, mac_pld, _, _, _, _, _, _, _, _) p ->
            f (vlan, if by_src then mac_src else mac_dst) t1 t2 (float_of_int (if what = PacketCount then count else mac_pld)) p)
            i c m in
    EthPld.per_time ?max_graphs start stop step fold label_of_eth_key

(* Returns traffic per pair of MACs *)
let eth_plot_vol_tot ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?max_graphs what dbdir name =
    let label_of_key (vlan, mac_src, mac_dst) =
        label_of_eth_key (vlan, mac_src),
        label_of_eth_key (vlan, mac_dst) in
    let fold f i c m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto dbdir name (fun (t1, t2, count, vlan, mac_src, mac_dst, _, mac_pld, _, _, _, _, _, _, _, _) p ->
            f (vlan, mac_src, mac_dst) t1 t2 (float_of_int (if what = PacketCount then count else mac_pld)) p)
            i c m in
    EthPld2.sum ?max_graphs ?start ?stop fold label_of_key

(* Returns traffic per pair of MACs *)
let eth_plot_vol_tot2 ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?(max_graphs=20) what dbdir name =
    let label_of_key (vlan, mac_src, mac_dst) =
        label_of_eth_key (vlan, mac_src),
        label_of_eth_key (vlan, mac_dst) in
    let fold f i c m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto dbdir name
            (fun (t1, t2, count, vlan, mac_src, mac_dst, _, mac_pld, _, _, _, _, _, _, _, _) p ->
                let y = float_of_int (if what = PacketCount then count else mac_pld) in
                let _, _, y = Plot.clip_y ?start ?stop t1 t2 y in
                let k = vlan, mac_src, mac_dst in
                f (k, y) p)
            i c m
        in
    assert (max_graphs > 1) ;
    Printf.fprintf stderr "Pass 1...\n%!" ;
    let interm = EthPld2.FindSignificant.pass1 fold (max_graphs-1) in
    Printf.fprintf stderr "interm:\n" ;
    EthPld2.Maplot.iter interm (fun k v ->
        let src, dst = label_of_key k in
        Printf.fprintf stderr "%s->%s: %f\n" src dst v) ;
    Printf.fprintf stderr "Pass 2...\n%!" ;
    let result, rest = EthPld2.FindSignificant.pass2 interm fold (max_graphs-1) in
    (* We want to return a hash of src*dst -> value *)
    let h = Hashtbl.create max_graphs in
    EthPld2.Maplot.iter result (fun k v ->
        Hashtbl.add h (label_of_key k) v) ;
    Hashtbl.add h ("other","") rest ;
    h

(* Returns traffic per pair of MACs *)
let eth_plot_vol_top ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?(max_graphs=20) by_src what dbdir name =
    let fold f i c m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto dbdir name
            (fun (t1, t2, count, vlan, mac_src, mac_dst, _, mac_pld, _, _, _, _, _, _, _, _) p ->
                let y = float_of_int (if what = PacketCount then count else mac_pld) in
                let _, _, y = Plot.clip_y ?start ?stop t1 t2 y in
                let k = vlan, if by_src then mac_src else mac_dst in
                f (k, y) p)
            i c m
        in
    assert (max_graphs > 1) ;
    let interm = EthPld.FindSignificant.pass1 fold (max_graphs-1) in
    let result, rest = EthPld.FindSignificant.pass2 interm fold (max_graphs-1) in
    (* We want to return a hash of src*dst -> value *)
    let h = Hashtbl.create max_graphs in
    EthPld.Maplot.iter result (fun k v ->
        Hashtbl.add h (label_of_eth_key k) v) ;
    Hashtbl.add h "other" rest ;
    h

(* Returns traffic per pair of MACs *)
let eth_plot_vol_top_both ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?(max_graphs=20) what dbdir name =
    let label_of_key (vlan, mac_src, mac_dst) =
        label_of_eth_key (vlan, mac_src) ^"\\u2192"^
        EthAddr.to_string mac_dst in
    let fold f i c m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto dbdir name
            (fun (t1, t2, count, vlan, mac_src, mac_dst, _, mac_pld, _, _, _, _, _, _, _, _) p ->
                let y = float_of_int (if what = PacketCount then count else mac_pld) in
                let _, _, y = Plot.clip_y ?start ?stop t1 t2 y in
                let k = vlan, mac_src, mac_dst in
                f (k, y) p)
            i c m
        in
    assert (max_graphs > 1) ;
    let interm = EthPld2.FindSignificant.pass1 fold (max_graphs-1) in
    let result, rest = EthPld2.FindSignificant.pass2 interm fold (max_graphs-1) in
    (* We want to return a hash of src*dst -> value *)
    let h = Hashtbl.create max_graphs in
    EthPld2.Maplot.iter result (fun k v ->
        Hashtbl.add h (label_of_key k) v) ;
    Hashtbl.add h "other" rest ;
    h

module IPKey = InetAddr (* source/dest *)
module IPPld = Plot.DataSet (IPKey)
module IPKey2 = Tuple2.Make (InetAddr) (InetAddr) (* source, dest *)
module IPPld2 = Plot.DataSet (IPKey2)

(* Returns traffic against time, with a different plot per IP sockpair *)
let ip_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?max_graphs by_src what step dbdir name =
    let label_of_key = InetAddr.to_string in
    let fold f i c m =
        Traffic.fold ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto dbdir name (fun (t1, t2, count, _, _, _, _, mac_pld, _, src, dst, _, _, _, _, _) p ->
            f (if by_src then src else dst) t1 t2 (float_of_int (if what = PacketCount then count else mac_pld)) p)
            i c m in
    IPPld.per_time ?max_graphs start stop step fold label_of_key

(* Returns traffic per pair of IPs *)
let ip_plot_vol_tot ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?max_graphs what dbdir name =
    let label_of_key (ip_src, ip_dst) =
        InetAddr.to_string ip_src,
        InetAddr.to_string ip_dst in
    let fold f i c m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto dbdir name (fun (t1, t2, count, _, _, _, _, mac_pld, _, src, dst, _, _, _, _, _) p ->
            f (src, dst) t1 t2 (float_of_int (if what = PacketCount then count else mac_pld)) p)
            i c m in
    IPPld2.sum ?max_graphs ?start ?stop fold label_of_key

let ip_plot_vol_tot2 ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?(max_graphs=20) what dbdir name =
    let label_of_key (ip_src, ip_dst) =
        InetAddr.to_string ip_src,
        InetAddr.to_string ip_dst in
    let fold f i c m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto dbdir name
            (fun (t1, t2, count, _, _, _, _, mac_pld, _, src, dst, _, _, _, _, _) p ->
                let y = float_of_int (if what = PacketCount then count else mac_pld) in
                let _, _, y = Plot.clip_y ?start ?stop t1 t2 y in
                let k = src, dst in
                f (k, y) p)
            i c m
        in
    assert (max_graphs > 1) ;
    Printf.fprintf stderr "Pass 1...\n%!" ;
    let interm = IPPld2.FindSignificant.pass1 fold (max_graphs-1) in
    Printf.fprintf stderr "interm:\n" ;
    IPPld2.Maplot.iter interm (fun k v ->
        let src, dst = label_of_key k in
        Printf.fprintf stderr "%s->%s: %f\n" src dst v) ;
    Printf.fprintf stderr "Pass 2...\n%!" ;
    let result, rest = IPPld2.FindSignificant.pass2 interm fold (max_graphs-1) in
    (* We want to return a hash of src*dst -> value *)
    let h = Hashtbl.create max_graphs in
    IPPld2.Maplot.iter result (fun k v ->
        Hashtbl.add h (label_of_key k) v) ;
    Hashtbl.add h ("other","") rest ;
    h

let ip_plot_vol_top ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?(max_graphs=20) by_src what dbdir name =
    let fold f i c m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto dbdir name
            (fun (t1, t2, count, _, _, _, _, mac_pld, _, src, dst, _, _, _, _, _) p ->
                let y = float_of_int (if what = PacketCount then count else mac_pld) in
                let _, _, y = Plot.clip_y ?start ?stop t1 t2 y in
                let k = if by_src then src else dst in
                f (k, y) p)
            i c m
        in
    assert (max_graphs > 1) ;
    let interm = IPPld.FindSignificant.pass1 fold (max_graphs-1) in
    let result, rest = IPPld.FindSignificant.pass2 interm fold (max_graphs-1) in
    (* We want to return a hash of src*dst -> value *)
    let h = Hashtbl.create max_graphs in
    IPPld.Maplot.iter result (fun k v ->
        Hashtbl.add h (InetAddr.to_string k) v) ;
    Hashtbl.add h "other" rest ;
    h

let ip_plot_vol_top_both ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?(max_graphs=20) what dbdir name =
    let label_of_key (ip_src, ip_dst) =
        InetAddr.to_string ip_src ^"\\u2192"^
        InetAddr.to_string ip_dst in
    let fold f i c m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto dbdir name
            (fun (t1, t2, count, _, _, _, _, mac_pld, _, src, dst, _, _, _, _, _) p ->
                let y = float_of_int (if what = PacketCount then count else mac_pld) in
                let _, _, y = Plot.clip_y ?start ?stop t1 t2 y in
                let k = src, dst in
                f (k, y) p)
            i c m
        in
    assert (max_graphs > 1) ;
    let interm = IPPld2.FindSignificant.pass1 fold (max_graphs-1) in
    let result, rest = IPPld2.FindSignificant.pass2 interm fold (max_graphs-1) in
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
let app_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?max_graphs what step dbdir name =
    let fold f i c m =
        Traffic.fold ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto dbdir name (fun (t1, t2, count, _, _, _, _, mac_pld, _, _, _, proto, _, p1, p2, _) p ->
            f (proto, min p1 p2) t1 t2 (float_of_int (if what = PacketCount then count else mac_pld)) p)
            i c m in
    AppPld.per_time ?max_graphs start stop step fold label_of_app_key

let app_plot_vol_top ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?(max_graphs=10) what dbdir name =
    let fold f i c m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto dbdir name
            (fun (t1, t2, count, _, _, _, _, mac_pld, _, _, _, proto, _, p1, p2, _) p ->
                let y = float_of_int (if what = PacketCount then count else mac_pld) in
                let _, _, y = Plot.clip_y ?start ?stop t1 t2 y in
                let k = (proto, min p1 p2) in
                f (k, y) p)
            i c m
        in
    assert (max_graphs > 1) ;
    let interm = AppPld.FindSignificant.pass1 fold (max_graphs-1) in
    let result, rest = AppPld.FindSignificant.pass2 interm fold (max_graphs-1) in
    (* We want to return a hash of src*dst -> value *)
    let h = Hashtbl.create max_graphs in
    AppPld.Maplot.iter result (fun k v ->
        Hashtbl.add h (label_of_app_key k) v) ;
    Hashtbl.add h "other" rest ;
    h

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


