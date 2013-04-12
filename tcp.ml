open Batteries
open Datatype
open Metric

let verbose = ref false

let lods = [| "sockets"; "1min"; "10mins"; "1hour" |];

(* Lod0: the full socket record *)

module Tcp =
struct
    include Altern1 (Tuple11.Make (Origin)              (* where the record came from *)
                                  (VLan)                (* VLAN *)
                                  (EthAddr)             (* client MAC *)
                                  (Cidr)                (* client IP *)
                                  (EthAddr)             (* server MAC *)
                                  (InetAddr)            (* server IP *)
                                  (UInteger16)          (* client port *)
                                  (UInteger16)          (* server port *)
                                  (Timestamp)           (* first SYN *)
                                  (UInteger16)          (* nb syns *)
                                  (Distribution)        (* connection time distribution *))

    (* We'd rather have an inlined reader *)
    let read ic =
        let tuple11_read ic =
            let t0 = Origin.read ic in
            let t1 =
                let o = Serial.deser8 ic in
                if o <> 0 then (
                    assert (o = 1) ;
                    Some (UInteger16.read ic)
                ) else None in
            let t2 = EthAddr.read ic in
            let t3 = Cidr.read ic in
            let t4 = EthAddr.read ic in
            let t5 = InetAddr.read ic in
            let t6 = UInteger16.read ic in
            let t7 = UInteger16.read ic in
            let t8 = Timestamp.read ic in
            let t9 = UInteger16.read ic in
            let t10 = Distribution.read ic in
            t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10 in
        let v = Serial.deser8 ic in
        if v <> 0 then Printf.fprintf stderr "bad version: %d\n%!" v ;
        assert (v = 0) ;
        tuple11_read ic

    (* We hash on the server IP *)
    let hash_on_srv (_orig, _vlan, _clte, _clt, _srve, srv, _cltp, _srvp, _ts, _syns, _ct) =
        InetAddr.hash srv

    (* Metafile stores timestamp of cnx establishment *)
    let meta_aggr (_orig, _vlan, _clte, _clt, _srve, _srv, _cltp, _srvp, ts, _syns, _ct) bound_opt =
        Aggregator.bounds ~cmp:Timestamp.compare ts bound_opt
    let meta_read = BoundsTS.read
    let meta_write = BoundsTS.write

    let table dbdir name =
        Table.create (table_name dbdir name)
            hash_on_srv write
            meta_aggr meta_read meta_write

    (* Function to query the Lod0, ie select a set of individual sockets *)
    (* Add min_count, server port *)
    let fold ?start ?stop ?vlan ?mac_clt ?client ?mac_srv ?server ?peer dbdir name f make_fst merge =
        let tdir = table_name dbdir name in
        let fold_hnum hnum fst =
            Table.fold_snums tdir hnum meta_read (fun snum bounds prev ->
                let cmp = Timestamp.compare in
                let res =
                    if is_within bounds start stop then (
                        Table.fold_file tdir hnum snum read (fun ((_orig, vl, clte, clt, srve, srv, _cltp, _srvp, ts, _syns, _ct) as x) prev ->
                            if check start   (fun start -> cmp ts start >= 0) &&
                               check stop    (fun stop  -> cmp stop ts > 0) &&
                               check client  (fun cidr  -> inter_cidr clt cidr) &&
                               check server  (fun cidr  -> in_cidr srv cidr) &&
                               check mac_clt (fun mac   -> EthAddr.equal mac clte) &&
                               check mac_srv (fun mac   -> EthAddr.equal mac srve) &&
                               check peer    (fun cidr  -> in_cidr srv cidr || inter_cidr clt cidr) &&
                               check vlan    (fun vlan  -> vl = vlan) then
                               f x prev
                            else prev)
                            prev
                    ) else (
                        prev
                    ) in
                res)
                fst merge in
        fold_using_indexed server tdir fold_hnum make_fst merge

    let iter ?start ?stop ?vlan ?mac_clt ?client ?mac_srv ?server ?peer dbdir name f =
        let dummy_merge _ _ = () in
        fold ?start ?stop ?vlan ?mac_clt ?client ?mac_srv ?server ?peer dbdir name (fun x _ -> f x) ignore dummy_merge
end

(* Lod1: cleared client_port, rounded start_date (to 1min) *)
(* Lod2: degraded client, round start_date to 10 mins *)
(* Lod3: and finally to an hour *)

(* Load new data into the database *)

let load dbdir create fname =

    let aggreg_all ((syns, ct) as x) = function
        | 0, (0, _, _, _, _) -> x
        | syns', ct' ->
            syns + syns',
            Distribution.combine ct ct' in

    if not create && not (try Sys.is_directory dbdir with Sys_error _ -> false) then (
        failwith (Printf.sprintf "Directory %s does not exist" dbdir)
    ) ;

    let table3 = Tcp.table dbdir lods.(3) in
    let accum3, flush3 =
        Aggregator.(accum (now_and_then (buffer_duration_of_lod lods.(3) "tcp")))
            aggreg_all
            [ fun (orig, vlan, clte, clt, srve, srv, srvp, ts) (syns, ct) ->
                Table.append table3 (orig, vlan, clte, clt, srve, srv, 0, srvp, ts, syns, ct) ] in

    let table2 = Tcp.table dbdir lods.(2) in
    let rti = rti_of_lod lods.(3) "tcp" in
    let accum2, flush2 =
        Aggregator.(accum (now_and_then (buffer_duration_of_lod lods.(2) "tcp")))
            aggreg_all
            [ fun (orig, vlan, clte, clt, srve, srv, srvp, ts) (syns, ct) ->
                Table.append table2 (orig, vlan, clte, clt, srve, srv, 0, srvp, ts, syns, ct) ;
                BatOption.may (fun rti ->
                    let ts = round_timestamp rti ts in
                    accum3 (orig, vlan, clte, clt, srve, srv, srvp, ts) (syns, ct))
                    rti ] in

    let table1 = Tcp.table dbdir lods.(1) in
    let rti = rti_of_lod lods.(2) "tcp" in
    let accum1, flush1 =
        Aggregator.(accum (now_and_then (buffer_duration_of_lod lods.(1) "tcp")))
            aggreg_all
            [ fun (orig, vlan, clte, clt, srve, srv, srvp, ts) (syns, ct) ->
                Table.append table1 (orig, vlan, clte, clt, srve, srv, 0, srvp, ts, syns, ct) ;
                BatOption.may (fun rti ->
                    let ts = round_timestamp rti ts
                    and clt, _mask = clt in
                    let clt = cidr_of_inetaddr Subnet.subnets clt in
                    accum2 (orig, vlan, clte, clt, srve, srv, srvp, ts) (syns, ct))
                    rti ] in

    let table0 = Tcp.table dbdir lods.(0) in
    let rti = rti_of_lod lods.(1) "tcp" in
    let append0 ((orig, vlan, clte, clt, srve, srv, _cltp, srvp, ts, syns, ct) as v) =
        Table.append table0 v ;
        BatOption.may (fun rti ->
            let ts = round_timestamp rti ts in
            accum1 (orig, vlan, clte, clt, srve, srv, srvp, ts) (syns, ct))
            rti in

    let flush_all () =
        if !verbose then Printf.printf "Flushing...\n" ;
        flush1 () ;
        flush2 () ;
        flush3 () ;
        Table.close table0 ;
        Table.close table1 ;
        Table.close table2 ;
        Table.close table3 in

    load fname Tcp.parzer append0 flush_all

(*$T
  match Tcp.parzer (String.to_list \
    "iface eth0\tSome 250\t88:43:e1:1d:6d:01\t193.48.57.46\tb4:a4:e3:4d:5c:01\t194.98.114.133\t48242\t80\t1323766045s 962156us\t2\t1\t0.005900\t0.005900\t0.005900\t0") with \
        | Peg.Res (_, []) -> true \
        | _ -> false
 *)
