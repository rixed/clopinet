open Batteries
open Datatype
open Metric

let verbose = ref false

(* Lod0: the full socket record *)

module Tcp =
struct
    include Altern1 (Tuple13.Make (Option (Integer16))  (* VLAN *)
                                  (EthAddr)             (* client MAC *)
                                  (Cidr)                (* client IP *)
                                  (EthAddr)             (* server MAC *)
                                  (InetAddr)            (* server IP *)
                                  (UInteger16)          (* client port *)
                                  (UInteger16)          (* server port *)
                                  (Timestamp)           (* first SYN *)
                                  (UInteger16)          (* nb syns *)
                                  (Distribution)        (* connection time distribution *)
                                  (Distribution)        (* socket duration distribution *)
                                  (UInteger)            (* uplink payload *)
                                  (UInteger)            (* downlink payload *))

    (* We'd rather have an inlined reader *)
    let read ic =
        let tuple13_read ic =
            let t0 =
                let o = Serial.deser8 ic in
                if o <> 0 then (
                    assert (o = 1) ;
                    Some (UInteger16.read ic)
                ) else None in
            let t1 = EthAddr.read ic in
            let t2 = Cidr.read ic in
            let t3 = EthAddr.read ic in
            let t4 = InetAddr.read ic in
            let t5 = UInteger16.read ic in
            let t6 = UInteger16.read ic in
            let t7 = Timestamp.read ic in
            let t8 = UInteger16.read ic in
            let t9 = Distribution.read ic in
            let t10 = Distribution.read ic in
            let t11 = UInteger.read ic in
            let t12 = UInteger.read ic in
            t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12 in
        let v = Serial.deser8 ic in
        if v <> 0 then Printf.fprintf stderr "bad version: %d\n%!" v ;
        assert (v = 0) ;
        tuple13_read ic

    (* We hash on the server IP *)
    let hash_on_srv (_vlan, _clte, _clt, _srve, srv, _cltp, _srvp, _ts, _syns, _ct, _dur, _up, _do) =
        InetAddr.hash srv

    (* Metafile stores timestamp range of the whole socket duration *)
    let meta_aggr (_vlan, _clte, _clt, _srve, _srv, _cltp, _srvp, ts, _syns, _ct, dur, _up, _do) bound_opt =
        let _, _, max_dur, _, _ = dur in
        let ts2 = Timestamp.add ts (max_dur |> Int64.of_float) in
        let bound = Aggregator.bounds ~cmp:Timestamp.compare ts bound_opt in
        Aggregator.bounds ~cmp:Timestamp.compare ts2 (Some bound)
    let meta_read = BoundsTS.read
    let meta_write = BoundsTS.write

    let table dbdir name =
        Table.create (table_name dbdir name)
            hash_on_srv write
            meta_aggr meta_read meta_write

    (* Function to query the Lod0, ie select a set of individual sockets *)
    (* Add min_count, server port *)
    let fold ?start ?stop ?vlan ?mac_clt ?client ?mac_srv ?server ?peer ?dur_min ?dur_max dbdir name f make_fst merge =
        let tdir = table_name dbdir name in
        let fold_hnum hnum fst =
            Table.fold_snums tdir hnum meta_read (fun snum bounds prev ->
                let cmp = Timestamp.compare in
                let res =
                    if is_within bounds start stop then (
                        Table.fold_file tdir hnum snum read (fun ((vl, clte, clt, srve, srv, _cltp, _srvp, ts, _syns, _ct, dur, _up, _down) as x) prev ->
                            if check start   (fun start ->
                                    let _, _, max_dur, _, _ = dur in
                                    let ts2 = Timestamp.add ts (max_dur |> Int64.of_float) in
                                    cmp ts2 start >= 0) &&
                               check stop    (fun stop  -> cmp stop ts > 0) &&
                               check dur_min (fun d     -> let _,_,ma,_,_ = dur in ma > d) &&
                               check dur_max (fun d     -> let _,mi,_,_,_ = dur in mi < d) &&
                               check client  (fun cidr  -> inter_cidr clt cidr) &&
                               check server  (fun cidr  -> in_cidr srv cidr) &&
                               check mac_clt (fun mac   -> EthAddr.equal mac clte) &&
                               check mac_srv (fun mac   -> EthAddr.equal mac srve) &&
                               check peer    (fun cidr  -> in_cidr srv cidr || inter_cidr clt cidr) &&
                               check vlan    (fun vlan  -> vl = Some vlan) then
                               f x prev
                            else prev)
                            prev
                    ) else (
                        prev
                    ) in
                res)
                fst merge in
        fold_using_indexed server tdir fold_hnum make_fst merge

    let iter ?start ?stop ?vlan ?mac_clt ?client ?mac_srv ?server ?peer ?dur_min ?dur_max dbdir name f =
        let dummy_merge _ _ = () in
        fold ?start ?stop ?vlan ?mac_clt ?client ?mac_srv ?server ?peer ?dur_min ?dur_max dbdir name (fun x _ -> f x) ignore dummy_merge
end

(* Lod1: cleared client_port, rounded start_date (to 1min) *)
(* Lod2: degraded client, round start_date to 10 mins *)
(* Lod3: and finally to an hour *)

(* Load new data into the database *)

let load dbdir create fname =

    let aggreg_all ((syns, ct, dur, up, down) as x) = function
        | None -> x
        | Some (syns', ct', dur', up', down') ->
            syns + syns',
            Distribution.combine ct  (Some ct'),
            Distribution.combine dur (Some dur'),
            up + up',
            down + down' in

    if not create && not (try Sys.is_directory dbdir with Sys_error _ -> false) then (
        failwith (Printf.sprintf "Directory %s does not exist" dbdir)
    ) ;

    let table3 = Tcp.table dbdir "1hour" in
    let accum3, flush3 =
        Aggregator.(accum (now_and_then (2. *. 3600.)))
            aggreg_all
            [ fun (vlan, clte, clt, srve, srv, srvp, ts) (syns, ct, dur, up, down) ->
                Table.append table3 (vlan, clte, clt, srve, srv, 0, srvp, ts, syns, ct, dur, up, down) ] in

    let table2 = Tcp.table dbdir "10mins" in
    let accum2, flush2 =
        Aggregator.(accum (now_and_then (2. *. 600.)))
            aggreg_all
            [ fun (vlan, clte, clt, srve, srv, srvp, ts) (syns, ct, dur, up, down) ->
                Table.append table2 (vlan, clte, clt, srve, srv, 0, srvp, ts, syns, ct, dur, up, down) ;
                let ts = round_timestamp 3600_000 ts in
                accum3 (vlan, clte, clt, srve, srv, srvp, ts) (syns, ct, dur, up, down) ] in

    let table1 = Tcp.table dbdir "1min" in
    let accum1, flush1 =
        Aggregator.(accum (now_and_then 60.))
            aggreg_all
            [ fun (vlan, clte, clt, srve, srv, srvp, ts) (syns, ct, dur, up, down) ->
                Table.append table1 (vlan, clte, clt, srve, srv, 0, srvp, ts, syns, ct, dur, up, down) ;
                let ts = round_timestamp 600_000 ts
                and clt, _mask = clt in
                let clt = cidr_of_inetaddr Subnet.subnets clt in
                accum2 (vlan, clte, clt, srve, srv, srvp, ts) (syns, ct, dur, up, down) ] in

    let table0 = Tcp.table dbdir "sockets" in

    let append0 ((vlan, clte, clt, srve, srv, _cltp, srvp, ts, syns, ct, dur, up, down) as v) =
        Table.append table0 v ;
        let ts = round_timestamp 60_000 ts in
        accum1 (vlan, clte, clt, srve, srv, srvp, ts) (syns, ct, dur, up, down) in

    let flush_all () =
        if !verbose then Printf.printf "Flushing...\n" ;
        flush1 () ;
        flush2 () ;
        flush3 () ;
        Table.close table0 ;
        Table.close table1 ;
        Table.close table2 ;
        Table.close table3 in

    load fname Tcp.read_txt append0 flush_all

