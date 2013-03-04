open Batteries
open Datatype
open Metric

let verbose = ref false

let lods = [| "flows" |];

(* Lod0 (and only): Individual l4 flows
  vlan, src mac, src ip, dst mac, dst ip, ip proto, src port, dst port, start, stop, nb pkts, l4 payload *)

module Flow =
struct
    include Altern1 (Tuple12.Make (VLan)                         (* vlan *)
                                  (EthAddr)                      (* src mac *)
                                  (InetAddr)                     (* src IP *)
                                  (EthAddr)                      (* dst mac *)
                                  (InetAddr)                     (* dst IP *)
                                  (UInteger8)                    (* IP proto *)
                                  (UInteger16) (UInteger16)      (* Port source, dest *)
                                  (Timestamp) (Timestamp)        (* start, stop *)
                                  (ULeast63)                     (* packet count *)
                                  (ULeast63)                     (* L4 payload *))
    (* We'd rather have an inlined reader: *)
    let read ic =
        let tuple12_read ic =
            let t0 =
                let o = Serial.deser8 ic in
                if o <> 0 then (
                    assert (o = 1) ;
                    Some (UInteger16.read ic)
                ) else None in
            let t1 = EthAddr.read ic in
            let t2 = InetAddr.read ic in
            let t3 = EthAddr.read ic in
            let t4 = InetAddr.read ic in
            let t5 = UInteger8.read ic in
            let t6 = UInteger16.read ic in
            let t7 = UInteger16.read ic in
            let t8 = Timestamp.read ic in
            let t9 = Timestamp.read ic in
            let t10 = ULeast63.read ic in
            let t11 = ULeast63.read ic in
            t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11 in
        let v = Serial.deser8 ic in
        if v <> 0 then Printf.fprintf stderr "bad version: %d\n%!" v ;
        assert (v = 0) ;
        tuple12_read ic

    (* We hash on the source IP *)
    let hash_on_src (_vlan, _mac_src, ip_src, _mac_dst, _ip_dst, _ip_proto, _port_src, _port_dst, _ts1, _ts2, _pkts, _pld) =
        InetAddr.hash ip_src

    (* Metafile stores timestamp range of the whole flow duration *)
    let meta_aggr (_vlan, _mac_src, _ip_src, _mac_dst, _ip_dst, _ip_proto, _port_src, _port_dst, ts1, ts2, _pkts, _pld) bound_opt =
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

    (* We look for semi-closed time interval [start;stop[, but tuples timestamps are closed [ts1;ts2] *)
    let fold ?start ?stop ?vlan ?mac_src ?mac_dst ?ip_src ?ip_dst ?ip ?ip_proto ?port_src ?port_dst ?port dbdir name f make_fst merge =
        let tdir = table_name dbdir name in
        let fold_hnum hnum fst =
            Table.fold_snums tdir hnum meta_read (fun snum bounds prev ->
                let cmp = Timestamp.compare in
                let res =
                    if is_within bounds start stop then (
                        Table.fold_file tdir hnum snum read (fun ((vl, mac_s, ip_s, mac_d, ip_d, ip_prot, port_s, port_d, ts1, ts2, _pkts, _pld) as x) prev ->
                            (* ocamlopt won't inline check function here, which hurts! *)
                            if check start     (fun start -> cmp ts2 start >= 0) &&
                               check stop      (fun stop  -> cmp stop ts1 > 0) &&
                               check mac_src   (fun mac   -> EthAddr.equal mac mac_s) &&
                               check mac_dst   (fun mac   -> EthAddr.equal mac mac_d) &&
                               check ip_src    (fun cidr  -> in_cidr ip_s cidr) &&
                               check ip_dst    (fun cidr  -> in_cidr ip_d cidr) &&
                               check ip        (fun cidr  -> in_cidr ip_s cidr || in_cidr ip_d cidr) &&
                               check ip_proto  (fun proto -> proto = ip_prot) &&
                               check port_src  (fun port  -> port = port_s) &&
                               check port_dst  (fun port  -> port = port_d) &&
                               check port      (fun port  -> port = port_s || port = port_d) &&
                               check vlan      (fun vlan  -> vl = vlan) then
                               f x prev
                            else prev)
                            prev
                    ) else (
                        prev
                    ) in
                res)
                fst merge in
        fold_using_indexed ip_src tdir fold_hnum make_fst merge

    let iter ?start ?stop ?vlan ?mac_src ?mac_dst ?ip_src ?ip_dst ?ip ?ip_proto ?port_src ?port_dst ?port dbdir name f =
        let dummy_merge _ _ = () in
        fold ?start ?stop ?vlan ?mac_src ?mac_dst ?ip_src ?ip_dst ?ip ?ip_proto ?port_src ?port_dst ?port dbdir name (fun x _ -> f x) ignore dummy_merge

end

(* Queries *)

type callflow_item_spec = Dt of float (* volume *) | Tx of string (* resp *)
type callflow_item =
   (*  start    *   stop      * peer1  *  peer2 * descr  * group *)
    Timestamp.t * Timestamp.t * string * string * string * string * callflow_item_spec

let clip start stop (x : Flow.t) =
    let ratio tot rem v =
        Int64.div (Int64.mul rem (Int64.of_int v)) tot |> Int64.to_int in
    let clip_lo ((vl, mac_s, ip_s, mac_d, ip_d, ip_proto, port_s, port_d, ts1, ts2, pkts, pld) as x) =
        if Timestamp.compare ts1 start >= 0 then x else (
            let tot = Timestamp.sub ts2 ts1
            and rem = Timestamp.sub start ts1 in
            (vl, mac_s, ip_s, mac_d, ip_d, ip_proto, port_s, port_d, start, ts2, ratio tot rem pkts, ratio tot rem pld)
        )
    and clip_hi ((vl, mac_s, ip_s, mac_d, ip_d, ip_proto, port_s, port_d, ts1, ts2, pkts, pld) as x) =
        if Timestamp.compare ts2 stop <= 0 then x else (
            let tot = Timestamp.sub ts2 ts1
            and rem = Timestamp.sub ts2 stop in
            (vl, mac_s, ip_s, mac_d, ip_d, ip_proto, port_s, port_d, ts1, stop, ratio tot rem pkts, ratio tot rem pld)
        ) in
    clip_lo x |> clip_hi

let get_callflow start stop ?vlan ip_start ?ip_dst ?ip_proto ?port_src ?port_dst ?dns_dbdir ?web_dbdir ?tcp_dbdir dbdir =
    let string_of_port proto port = try Unix.((getservbyport port proto).s_name) with Not_found -> string_of_int port in
    let ts2_of_rt ts1 (rt_count, _rt_min, _rt_max, rt_avg, _rt_sigma) =
        assert (rt_count = 1) ;
        let dt = Int64.of_float (rt_avg *. 0.001) in (* micro to milliseconds *)
        Timestamp.add ts1 dt in
    let flow_of_tuple ((_vl, _mac_s, ip_s, _mac_d, ip_d, ip_proto, port_s, port_d, ts1, ts2, pkts, pld) : Flow.t) : callflow_item =
        ts1, ts2,
        InetAddr.to_string ip_s, InetAddr.to_string ip_d,
        (let proto = try Unix.((getprotobynumber ip_proto).p_name) with Not_found -> "" in
        Printf.sprintf "%s%s%s&#x2192;%s (%dpkt%s/%s)"
            proto (if String.length proto > 0 then "/" else "")
            (string_of_port proto port_s) (string_of_port proto port_d)
            pkts (if pkts > 1 then "s" else "")
            (string_of_volume (float_of_int pld))),
        Traffic.label_of_app_key (ip_proto, min port_s port_d),
        Dt (float_of_int pld)
    and flow_of_dns (_vlan, _clte, clt, _srve, srv, err, ts, rt, name) =
        ts, ts2_of_rt ts rt,
        InetAddr.to_string (fst clt), InetAddr.to_string srv,
        Printf.sprintf "%s?" name,
        "DNS",
        Tx (Dns.string_of_err err)
    and flow_of_web (_vlan, _clte, clt, _srve, srv, _srvp, meth, err, ts, rt, host, url) =
        ts, ts2_of_rt ts rt,
        InetAddr.to_string (fst clt), InetAddr.to_string srv,
        Web.string_of_request ~max_len:60 meth host url,
        "WEB",
        Tx (Printf.sprintf "status: %d" err)
    and flow_of_tcp (_vlan, _clte, clt, _srve, srv, cltp, srvp, ts, syns, ct) =
        ts, ts2_of_rt ts ct,
        InetAddr.to_string (fst clt), InetAddr.to_string srv,
        Printf.sprintf "SYN %d&#x2192;%d" cltp srvp,
        "TCP",
        Tx (if syns > 2 then Printf.sprintf "%d SYNs" syns else "") in
    let start, stop = min start stop, max start stop in
    (* The list of IPs we already used as source (avoids looping).
       FIXME: not very accurate if we start with a restriction on ip_dst or ip_proto... *)
    let srcs = Hashtbl.create 71 in

    let rec add_flows prevs ip_start ?ip_dst ?ip_proto ?port_src ?port_dst start stop =
        (* Add to prevs the flows from ip_start to any other host, and from
         * these hosts to ip_start, occuring between ts_start and ts_stop *)
        if Hashtbl.mem srcs ip_start then (
            prevs
        ) else
        let flows, peers =
            Flow.fold ~start ~stop ?vlan ~ip_src:(cidr_singleton ip_start) ?ip_dst
                      ?ip_proto ?port_src ?port_dst dbdir lods.(0)
                      (fun ((_vl, _mac_s, _ip_s, _mac_d, ip_d, ip_prot, port_s, port_d, ts1, _ts2, _pkts, _pld) as x) (flows, ip_2_tsmin) ->
                          let peer = ip_d,ip_prot,port_s,port_d
                          and ts1 = Timestamp.max start ts1 in
                          (match Hashtbl.find_option ip_2_tsmin peer with
                          | None -> Hashtbl.add ip_2_tsmin peer ts1
                          | Some ts ->
                              if Timestamp.compare ts1 ts < 0 then (
                                  Hashtbl.replace ip_2_tsmin peer ts1
                              )) ;
                          ((flow_of_tuple (clip start stop x)) :: flows, ip_2_tsmin))
                      (fun () -> [], Hashtbl.create 31)
                      (fun (f1, h1) (f2, h2) -> (* expect (f1, h1) to be larger than (f2, h2) *)
                          Hashtbl.iter (fun peer2 ts2 ->
                            match Hashtbl.find_option h1 peer2 with
                            | None -> Hashtbl.add h1 peer2 ts2
                            | Some ts1 ->
                                if Timestamp.compare ts2 ts1 < 0 then
                                    Hashtbl.replace h1 peer2 ts2) h2 ;
                          List.rev_append f2 f1, h1) in
        (* At this point flows has the flows from ip_start to any dest, with
         * peers being a hash from (ip,proto,ports) to min_ts. Let's add the flow in the
         * other dorection, starting after tsmin of this peer: *)
        let flows, peers =
            Hashtbl.fold (fun (ip,ip_proto,port_dst (* <-inversed-> *),port_src) ts_min (flows, peers) ->
                let other_dir, ts_max =
                    Flow.fold ~start:ts_min ~stop ?vlan ~ip_src:(cidr_singleton ip) ~ip_dst:(cidr_singleton ip_start)
                              ~ip_proto ~port_src ~port_dst dbdir lods.(0)
                              (fun ((_vl, _mac_s, _ip_s, _mac_d, _ip_d, _ip_prot, _port_s, _port_d, _ts1, ts2, _pkts, _pld) as x) (flows, tsmax) ->
                                  let ts2 = Timestamp.min stop ts2 in
                                  flow_of_tuple (clip start stop x) :: flows, Timestamp.max tsmax ts2)
                              (fun () -> [], ts_min)
                              (fun (f1, ts1) (f2, ts2) -> (* expect f1 to be larger than f2 *)
                                List.rev_append f2 f1, Timestamp.max ts1 ts2) in
                let this_peer = ip,
                                Timestamp.min ts_min ts_max,
                                Timestamp.max ts_min ts_max in
                List.rev_append other_dir flows,
                this_peer :: peers)
                peers
                (flows, []) in
        (* Now flows have flows in both ways between ip_start and all it's
         * peers, and in peers we have a list of all peers with their ts min
         * and max. Look for all transactions we can find for these peers,
         * and recurse! *)
        Hashtbl.add srcs ip_start true ;
        List.fold_left (fun flows (ip, start, stop) ->
            let empty_flows () = []
            and merge_flows f1 f2 = List.rev_append f2 f1 in
            let dns_flows = BatOption.map_default (fun dbdir ->
                Dns.Dns.fold ~start ~stop ?vlan ~server:(cidr_singleton ip) dbdir Dns.lods.(0) (fun dns flows ->
                    flow_of_dns dns :: flows)
                    empty_flows merge_flows) [] dns_dbdir
            and web_flows = BatOption.map_default (fun dbdir ->
                Web.Web.fold ~start ~stop ?vlan ~server:(cidr_singleton ip) dbdir Web.lods.(0) (fun web flows ->
                    flow_of_web web :: flows)
                    empty_flows merge_flows) [] web_dbdir
            and tcp_flows = BatOption.map_default (fun dbdir ->
                Tcp.Tcp.fold ~start ~stop ?vlan ~server:(cidr_singleton ip) dbdir Tcp.lods.(0) (fun tcp flows ->
                    flow_of_tcp tcp :: flows)
                    empty_flows merge_flows) [] tcp_dbdir
            in
            let all_flows = List.rev_append dns_flows flows |>
                            List.rev_append web_flows |>
                            List.rev_append tcp_flows in
            add_flows all_flows ip start stop)
            (List.rev_append flows prevs) peers
    in
    add_flows [] ip_start ?ip_dst ?ip_proto ?port_src ?port_dst start stop

(* Load new data into the database *)

let load dbdir create fname =

    if not create && not (try Sys.is_directory dbdir with Sys_error _ -> false) then (
        failwith (Printf.sprintf "Directory %s does not exist" dbdir)
    ) ;

    let table0 = Flow.table dbdir lods.(0) in

    let append0 = Table.append table0 in

    let flush_all () =
        if !verbose then Printf.printf "Flushing...\n" ;
        Table.close table0 in

    load fname Flow.parzer append0 flush_all

