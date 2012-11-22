open Bricabrac
open Datatype
open Metric
module Hashtbl = BatHashtbl

let verbose = ref false

(* Lod0 (and only): Individual l4 flows
  vlan, src mac, src ip, dst mac, dst ip, ip proto, src port, dst port, start, stop, nb pkts, l4 payload *)

module Flow =
struct
    include Altern1 (Tuple12.Make (Option (UInteger16))          (* vlan *)
                                  (EthAddr)                      (* src mac *)
                                  (InetAddr)                     (* src IP *)
                                  (EthAddr)                      (* dst mac *)
                                  (InetAddr)                     (* dst IP *)
                                  (UInteger8)                    (* IP proto *)
                                  (UInteger16) (UInteger16)      (* Port source, dest *)
                                  (Timestamp) (Timestamp)        (* start, stop *)
                                  (UInteger)                     (* packet count *)
                                  (UInteger)                     (* L4 payload *))
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
            let t10 = UInteger.read ic in
            let t11 = UInteger.read ic in
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

    let iter ?start ?stop ?vlan ?mac_src ?mac_dst ?ip_src ?ip_dst ?ip ?ip_proto ?port_src ?port_dst ?port dbdir name f =
        let dummy_merge _ _ = () in
        fold ?start ?stop ?vlan ?mac_src ?mac_dst ?ip_src ?ip_dst ?ip ?ip_proto ?port_src ?port_dst ?port dbdir name (fun x _ -> f x) ignore dummy_merge

end

(* Queries *)

                    (*  start    *   stop      * peer1  *  peer2 * descr  * volume * group *)
type callflow_item = Timestamp.t * Timestamp.t * string * string * string * float * string

let get_callflow start stop ?vlan ip_start ?ip_dst ?ip_proto ?port_src ?port_dst dbdir =
    let string_of_port proto port = try Unix.((getservbyport port proto).s_name) with Not_found -> string_of_int port in
    let flow_of_tuple ((_vl, _mac_s, ip_s, _mac_d, ip_d, ip_proto, port_s, port_d, ts1, ts2, pkts, pld) : Flow.t) : callflow_item =
        ts1, ts2,
        InetAddr.to_string ip_s, InetAddr.to_string ip_d,
        (let proto = try Unix.((getprotobynumber ip_proto).p_name) with Not_found -> "" in
        Printf.sprintf "%s%s%s&#x2192;%s (%dpkt%s/%s)"
            proto (if String.length proto > 0 then "/" else "")
            (string_of_port proto port_s) (string_of_port proto port_d)
            pkts (if pkts > 1 then "s" else "")
            (string_of_volume (float_of_int pld))),
        float_of_int pld,
        Traffic.label_of_app_key (ip_proto, min port_s port_d) in
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
                      ?ip_proto ?port_src ?port_dst dbdir "flows"
                      (fun ((_vl, _mac_s, _ip_s, _mac_d, ip_d, ip_prot, port_s, port_d, ts1, _ts2, _pkts, _pld) as x) (flows, ip_2_tsmin) ->
                          let peer = ip_d,ip_prot,port_s,port_d
                          and ts1 = Timestamp.max start ts1 in
                          (match Hashtbl.find_option ip_2_tsmin peer with
                          | None -> Hashtbl.add ip_2_tsmin peer ts1
                          | Some ts ->
                              if Timestamp.compare ts1 ts < 0 then (
                                  Hashtbl.replace ip_2_tsmin peer ts1
                              )) ;
                          (* FIXME: a trimed down version of x? *)
                          ((flow_of_tuple x) :: flows, ip_2_tsmin))
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
                              ~ip_proto ~port_src ~port_dst dbdir "flows"
                              (fun ((_vl, _mac_s, _ip_s, _mac_d, _ip_d, _ip_prot, _port_s, _port_d, _ts1, ts2, _pkts, _pld) as x) (flows, tsmax) ->
                                  let ts2 = Timestamp.min stop ts2 in
                                  flow_of_tuple x :: flows, Timestamp.max tsmax ts2)
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
         * peers, and for in peers we have a list of all peers with their ts min
         * and max. Lets recurse! *)
        Hashtbl.add srcs ip_start true ;
        List.fold_left (fun flows (ip_start,start,stop) ->
            add_flows flows ip_start start stop)
            (List.rev_append flows prevs) peers
    in
    add_flows [] ip_start ?ip_dst ?ip_proto ?port_src ?port_dst start stop

(* Load new data into the database *)

let load dbdir create fname =

    if not create && not (try Sys.is_directory dbdir with Sys_error _ -> false) then (
        failwith (Printf.sprintf "Directory %s does not exist" dbdir)
    ) ;

    let table0 = Flow.table dbdir "flows" in

    let append0 = Table.append table0 in

    let flush_all () =
        if !verbose then Printf.printf "Flushing...\n" ;
        Table.close table0 in

    load fname Flow.read_txt append0 flush_all

