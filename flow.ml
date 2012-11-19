open Bricabrac
open Datatype
open Metric

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

