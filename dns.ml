open Bricabrac
open Datatype

let verbose = ref false

let subnets =
    List.map (fun (s, w) -> Unix.inet_addr_of_string s, w)
        [ "0.0.0.0", 2 ; "64.0.0.0", 2 ; "128.0.0.0", 2 ; "192.0.0.0", 2 ]

(* another example: flush as soon as the passed value changes (according to the eq function,
   which is free to compare only a given field of x... *)
let when_change eq =
    let prev = ref None in
    fun k v -> match !prev with
        | None -> prev := Some (k, v) ; false
        | Some p when eq p (k, v) -> false
        | _ -> prev := Some (k, v) ; true


(* Lod0: the full request record *)

module Bounds64 = Tuple2.Make (Integer64) (Integer64)
module BoundsTS = Tuple2.Make (Timestamp) (Timestamp)

module Dns =
struct
    include Altern1 (Tuple9.Make (Option (Integer16))   (* VLAN *)
                                 (EthAddr)              (* client MAC *)
                                 (Cidr)                 (* client IP *)
                                 (EthAddr)              (* server MAC *)
                                 (InetAddr)             (* server IP *)
                                 (Integer8)             (* err code *)
                                 (Timestamp)            (* timestamp *)
                                 (Distribution)         (* resp time *)
                                 (Text)                 (* query name *))
    (* We'd rather have an inlined reader *)
    let read ic : t =
        let tuple9_read ic =
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
            let t5 = Integer8.read ic in
            let t6 = Timestamp.read ic in
            let t7 = Distribution.read ic in
            let t8 = Text.read ic in
            t0,t1,t2,t3,t4,t5,t6,t7,t8 in
        let v = Serial.deser8 ic in
        if v <> 0 then Printf.fprintf stderr "bad version: %d\n%!" v ;
        assert (v = 0) ;
        tuple9_read ic

    (* We hash on the server IP *)
    let hash_on_srv (_vlan, _clte, _clt, _srve, srv, _err, _ts, _rt, _name) =
        InetAddr.hash srv
    (* Metafile stores timestamp range *)
    let meta_aggr (_vlan, _clte, _clt, _srve, _srv, _err, ts, _rt, _name) =
        Aggregator.bounds ~cmp:Timestamp.compare ts
    let meta_read = BoundsTS.read
    let meta_write = BoundsTS.write
    let table_name dbdir name = dbdir ^ "/" ^ name
    let table dbdir name =
        Table.create (table_name dbdir name)
            hash_on_srv write
            meta_aggr meta_read meta_write

    (* Function to query the Lod0, ie select a set of individual queries *)
    let fold ?start ?stop ?vlan ?mac_clt ?client ?mac_srv ?server ?peer ?error ?qname ?rt_min ?rt_max ?tx_min dbdir name f make_fst merge =
        let tdir = table_name dbdir name in
        let ends_with e s =
            let eo = String.length e - 1 and so = String.length s - 1 in
            if eo > so then false else
            let rec aux eo so =
                if eo < 0 then true else e.[eo] = s.[so] && aux (eo-1) (so-1) in
            aux eo so in
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
                        Table.fold_file tdir hnum snum read (fun ((vl, clte, clt, srve, srv, err, ts, rt, name) as x) prev ->
                            if check start   (fun start -> not (cmp ts start < 0)) &&
                               check stop    (fun stop  -> not (cmp stop ts < 0)) &&
                               check rt_min  (fun rt_m  -> let _c,_mi,ma,_avg,_std = rt in ma > rt_m) &&
                               check rt_max  (fun rt_m  -> let _c,mi,_ma,_avg,_std = rt in mi < rt_m) &&
                               check tx_min  (fun req_m -> let c,_mi,_ma,_avg,_std = rt in c > req_m) &&
                               check client  (fun cidr  -> inter_cidr clt cidr) &&
                               check server  (fun cidr  -> in_cidr srv cidr) &&
                               check peer    (fun cidr  -> in_cidr srv cidr || inter_cidr clt cidr) &&
                               check mac_clt (fun mac   -> EthAddr.equal mac clte) &&
                               check mac_srv (fun mac   -> EthAddr.equal mac srve) &&
                               check error   (fun error -> error = err) &&
                               check vlan    (fun vlan  -> vl = Some vlan) &&
                               check qname   (fun qname -> ends_with qname name) then
                               f x prev
                            else prev)
                            prev
                    ) else (
                        prev
                    ) in
                res)
                fst merge in
        match server with
        | Some cidr when subnet_size cidr < Table.max_hash_size ->
            if !verbose then Printf.fprintf stderr "Using index\n" ;
            (* We have an index for this! Build the list of hnums *)
            let visited = Hashtbl.create 977 in
            let hnums = fold_ips cidr (fun ip p ->
                let hnum = InetAddr.hash ip mod Table.max_hash_size in
                if Hashtbl.mem visited hnum then (
                    p
                ) else (
                    Hashtbl.add visited hnum true ;
                    hnum :: p
            )) [] in
            Table.fold_some_hnums hnums fold_hnum make_fst merge
        | _ ->
            Table.fold_hnums tdir fold_hnum make_fst merge

    let iter ?start ?stop ?vlan ?mac_clt ?client ?mac_srv ?server ?peer ?error ?qname ?rt_min ?tx_min dbdir name f =
        let dummy_merge _ _ = () in
        fold ?start ?stop ?vlan ?mac_clt ?client ?mac_srv ?server ?peer ?error ?qname ?rt_min ?tx_min dbdir name (fun x _ -> f x) ignore dummy_merge

end

let plot_resp_time start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?rt_min ?rt_max ?tx_min step dbdir name =
    let start, stop = min start stop, max start stop in
    let fold f i m =
        Dns.fold ~start ~stop ?vlan ?mac_clt ?client ?mac_srv ?server ?rt_min ?rt_max ?tx_min dbdir name
            (fun (_vlan, _mac_clt, _clt, _mac_srv, _srv, _err, ts, rt, _name) p ->
                f ts rt p)
            i m in
    Plot.per_date start stop step fold

let top_requests start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?rt_min ?rt_max ?error ?qname dbdir n sort_order =
    let start, stop = min start stop, max start stop in
    let fold = Dns.fold ~start ~stop ?vlan ?mac_clt ?client ?mac_srv ?server ?rt_min ?rt_max ?error ?qname dbdir "queries" in
    let cmp (_vl1, _eclt1, _clt1, _esrv1, _srv1, _err1, _ts1, (_, _, _, rt1, _), _nm1)
            (_vl2, _eclt2, _clt2, _esrv2, _srv2, _err2, _ts2, (_, _, _, rt2, _), _nm2) =
        Float.compare rt1 rt2 in
    Plot.top_table n sort_order cmp fold


(* Lod1: degraded client, rounded query_date (to 1min), distribution of resptimes *)
(* Lod2: round timestamp to 10 mins *)
(* Lod3: and finally to hour *)

(* Load new data into the database *)

let load dbdir create fname =

    if not create && not (try Sys.is_directory dbdir with Sys_error _ -> false) then (
        failwith (Printf.sprintf "Directory %s does not exist" dbdir)
    ) ;

    let table3 = Dns.table dbdir "1hour" in
    let accum3, flush3 =
        Aggregator.(accum (now_and_then (2. *. 3600.)))
                         Distribution.combine
                         [ fun (vlan, clte, clt, srve, srv, err, ts, name) distr ->
                              Table.append table3 (vlan, clte, clt, srve, srv, err, ts, distr, name) ] in

    let table2 = Dns.table dbdir "10mins" in
    let accum2, flush2 =
        Aggregator.(accum (now_and_then (2. *. 600.)))
                         Distribution.combine
                         [ fun (vlan, clte, clt, srve, srv, err, ts, name) distr ->
                              Table.append table2 (vlan, clte, clt, srve, srv, err, ts, distr, name) ;
                              let ts = round_timestamp 3600_000 ts in
                              accum3 (vlan, clte, clt, srve, srv, err, ts, "") distr ] in

    let table1 = Dns.table dbdir "1min" in
    let accum1, flush1 =
        Aggregator.(accum (now_and_then 60.))
                         Distribution.combine
                         [ fun (vlan, clte, clt, srve, srv, err, ts, name) distr ->
                              Table.append table1 (vlan, clte, clt, srve, srv, err, ts, distr, name) ;
                              let ts = round_timestamp 600_000 ts in
                              (* TODO: keep only the last host name + TLD in the name *)
                              accum2 (vlan, clte, clt, srve, srv, err, ts, name) distr ] in

    let table0 = Dns.table dbdir "queries" in

    let append0 ((vlan, clte, clt, srve, srv, err, ts, distr, name) as v) =
        Table.append table0 v ;
        let clt, mask = clt in
        assert (mask = 32 || mask = 128) ; (* the mask is supposed to be total *)
        let clt = cidr_of_inetaddr subnets clt
        and ts = round_timestamp 60_000 ts in
        accum1 (vlan, clte, clt, srve, srv, err, ts, name) distr in

    let lineno = ref 0 in

    let flush_all () =
        if !verbose then Printf.printf "Flushing...\n" ;
        flush1 () ;
        flush2 () ;
        flush3 () ;
        Table.close table0 ;
        Table.close table1 ;
        Table.close table2 ;
        Table.close table3 in

    Sys.(List.iter
        (fun s -> set_signal s (Signal_handle (fun _ -> flush_all ())))
        [ sigabrt; sigfpe; sigill; sigint;
          sigpipe; sigquit; sigsegv; sigterm ]) ;

    try_finalize (fun () ->
        with_file_in fname (fun ic ->
            let ic = TxtInput.from_file ic in
            try forever (fun () ->
                Dns.read_txt ic |> append0 ;
                let eol = TxtInput.read ic in
                assert (eol = '\n' || eol = '\r') ;
                incr lineno) ()
            with End_of_file ->
                if !verbose then Printf.fprintf stderr "Inserted %d lines\n" !lineno
               | e ->
                Printf.fprintf stderr "Error at line %d\n" !lineno ;
                raise e)) ()
        flush_all ()

