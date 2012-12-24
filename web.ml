open Batteries
open Datatype
open Metric

(* FIXME: factorize all this with DNS *)

let verbose = ref false

(* Convert from junkie's integer to strings *)
let http_methods = [| "GET"; "HEAD"; "POST"; "CONNECT"; "PUT";
                      "OPTIONS"; "TRACE"; "DELETE" |]
let string_of_method = Array.get http_methods

let string_of_request ?max_len meth host url =
    let s = Printf.sprintf "%s http://%s/%s" (string_of_method meth) host url in
    match max_len with
    | None -> s
    | Some l ->
        let l = max 3 l in
        if String.length s <= l then s
        else (String.sub s 0 (l - 3)) ^ "..."

let string_of_err err =
    if err = 200 then "" else Printf.sprintf "err: %d" err

let lods = [| "queries"; "1min"; "10mins"; "1hour" |];

(* Lod0: the full request record: client, server, method (int), status-code, ts, rt, host, url *)

module Web =
struct
    include Altern1 (Tuple12.Make (VLan)                (* VLAN *)
                                  (EthAddr)             (* client MAC *)
                                  (Cidr)                (* client IP *)
                                  (EthAddr)             (* server MAC *)
                                  (InetAddr)            (* server IP *)
                                  (UInteger16)          (* server port *)
                                  (UInteger8)           (* query method *)
                                  (UInteger16)          (* error code *)
                                  (Timestamp)           (* start *)
                                  (Distribution)        (* duration, aka count, min, max, avg, sigma (See distribumtion.ml) *)
                                  (Text)                (* host *)
                                  (Text)                (* URL *))
    (* We'd rather have an inlined reader *)
    let read ic =
        let tuple12_read ic =
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
            let t5 = Integer16.read ic in
            let t6 = Integer8.read ic in
            let t7 = Integer16.read ic in
            let t8 = Timestamp.read ic in
            let t9 = Distribution.read ic in
            let t10 = Text.read ic in
            let t11 = Text.read ic in
            t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11 in
        let v = Serial.deser8 ic in
        if v <> 0 then Printf.fprintf stderr "bad version: %d\n%!" v ;
        assert (v = 0) ;
        tuple12_read ic

    (* We hash on the server IP *)
    let hash_on_srv (_vlan, _clte, _clt, _srve, srv, _srvp, _method, _err, _ts, _rt, _host, _url) =
        InetAddr.hash srv

    (* Metafile stores timestamp range *)
    let meta_aggr (_vlan, _clte, _clt, _srve, _srv, _srvp, _method, _err, ts, _rt, _host, _url) =
        Aggregator.bounds ~cmp:Timestamp.compare ts
    let meta_read = BoundsTS.read
    let meta_write = BoundsTS.write

    let table dbdir name =
        Table.create (table_name dbdir name)
            hash_on_srv write
            meta_aggr meta_read meta_write

    (* Function to query the Lod0, ie select a set of individual queries *)
    (* Add min_count *)
    let fold ?start ?stop ?vlan ?mac_clt ?client ?mac_srv ?server ?peer ?methd ?status ?host ?url ?rt_min ?rt_max dbdir name f make_fst merge =
        let tdir = table_name dbdir name in
        let fold_hnum hnum fst =
            let starts_with e s =
                if String.length e > String.length s then false else
                let rec aux eo so =
                    if eo >= String.length e then true else e.[eo] = s.[so] && aux (eo+1) (so+1) in
                aux 0 0 in
            let ends_with e s =
                let eo = String.length e - 1 and so = String.length s - 1 in
                if eo > so then false else
                let rec aux eo so =
                    if eo < 0 then true else e.[eo] = s.[so] && aux (eo-1) (so-1) in
                aux eo so in
            Table.fold_snums tdir hnum meta_read (fun snum bounds prev ->
                let cmp = Timestamp.compare in
                let res =
                    if is_within bounds start stop then (
                        Table.fold_file tdir hnum snum read (fun ((vl, clte, clt, srve, srv, _srvp, met, err, ts, rt, h, u) as x) prev ->
                            if check start   (fun start -> cmp ts start >= 0) &&
                               check stop    (fun stop  -> cmp stop ts >= 0) &&
                               check rt_min  (fun rt_m  -> let _, _,ma,_,_ = rt in ma > rt_m) &&
                               check rt_max  (fun rt_m  -> let _, mi,_,_,_ = rt in mi < rt_m) &&
                               check client  (fun cidr  -> inter_cidr clt cidr) &&
                               check server  (fun cidr  -> in_cidr srv cidr) &&
                               check mac_clt (fun mac   -> EthAddr.equal mac clte) &&
                               check mac_srv (fun mac   -> EthAddr.equal mac srve) &&
                               check peer    (fun cidr  -> in_cidr srv cidr || inter_cidr clt cidr) &&
                               check methd   (fun methd -> methd = met) &&
                               check status  (fun st    -> st = err) &&
                               check host    (fun host  -> ends_with host h) &&
                               check url     (fun url   -> starts_with url u) &&
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

    let iter ?start ?stop ?vlan ?mac_clt ?client ?mac_srv ?server ?peer ?methd ?status ?host ?url ?rt_min dbdir name f =
        let dummy_merge _ _ = () in
        fold ?start ?stop ?vlan ?mac_clt ?client ?mac_srv ?server ?peer ?methd ?status ?host ?url ?rt_min dbdir name (fun x _ -> f x) ignore dummy_merge
end

let plot_resp_time start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?methd ?status ?host ?url ?rt_min ?rt_max step dbdir name =
    let start, stop = min start stop, max start stop in
    let fold f i m =
        Web.fold ~start ~stop ?vlan ?mac_clt ?client ?mac_srv ?server ?methd ?status ?host ?url ?rt_min ?rt_max dbdir name
            (fun (_vlan, _mac_clt, _clt, _mac_srv, _srv, _srvp, _met, _err, ts, rt, _h, _u) p ->
                f ts rt p)
            i m in
    Plot.per_date start stop step fold

let top_requests start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?methd ?status ?host ?url ?rt_min ?rt_max dbdir n sort_order =
    let start, stop = min start stop, max start stop in
    let fold = Web.fold ~start ~stop ?vlan ?mac_clt ?client ?mac_srv ?server ?methd ?status ?host ?url ?rt_min ?rt_max dbdir lods.(0) in
    let cmp (_vl1, _ec1, _c1, _es1, _s1, _p1, _mt1, _er1, _ts1, (_, _, _, rt1, _), _h1, _u1)
            (_vl2, _ec2, _c2, _es2, _s2, _p2, _mt2, _er2, _ts2, (_, _, _, rt2, _), _h2, _u2) =
        Float.compare rt1 rt2 in
    Plot.top_table n sort_order cmp fold

(* Display in it's own color the servers that represent more than 1/top_nth share of the tot
 * number of queries *)
module WebDataSet = Plot.DataSet (InetAddr)
let plot_distrib start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?methd ?status ?host ?url ?rt_min ?rt_max ?(prec=0.05) ?(top_nth=100) dbdir tblname =
     let fold f i m =
        Web.fold ~start ~stop ?vlan ?mac_clt ?client ?mac_srv ?server ?methd ?status ?host ?url ?rt_min ?rt_max dbdir tblname
            (fun (_vl, _mac_clt, _clt, _mac_srv, srv, _srvp, _met, _err, _ts, rt, _h, _u) p ->
                let nb_queries, _, _, _, _ = rt in
                f (srv, nb_queries) p)
            i m in
    let interm = WebDataSet.FindSignificant.pass1 fold top_nth in
    let fold2 f i m =
        Web.fold ~start ~stop ?vlan ?mac_clt ?client ?mac_srv ?server ?methd ?status ?host ?url ?rt_min ?rt_max dbdir tblname
            (fun (_vl, _mac_clt, _clt, _mac_srv, srv, _srvp, _met, _err, _ts, rt, _h, _u) p ->
                let nb_queries, _, _, avg, _ = rt in
                (* TODO: instead of a single [avg], fake a distribution of nb_queries values *)
                f (srv, nb_queries, [avg]) p)
            i m
    and aggr_rts prev_rts rts = List.rev_append rts prev_rts in
    let result, _rest_count, rest_rts = WebDataSet.FindSignificant.pass2 interm fold2 aggr_rts [] top_nth in
    WebDataSet.distributions_of_response_times prec result rest_rts InetAddr.to_string

(* Lod1: degraded client, rounded query_date (to 1min), stripped url, distribution of resptimes *)
(* Lod2: round timestamp to 10 mins *)
(* Lod3: and finally to hour *)

(* Load new data into the database *)

let load dbdir create fname =

    if not create && not (try Sys.is_directory dbdir with Sys_error _ -> false) then (
        failwith (Printf.sprintf "Directory %s does not exist" dbdir)
    ) ;

    let table3 = Web.table dbdir lods.(3) in
    let accum3, flush3 =
        Aggregator.(accum (now_and_then (2. *. 3600.)))
                         Distribution.combine
                         [ fun (vlan, clte, clt, srve, srv, srvp, met, err, ts, host, url) distr ->
                              Table.append table3 (vlan, clte, clt, srve, srv, srvp, met, err, ts, distr, host, url) ] in

    let table2 = Web.table dbdir lods.(2) in
    let accum2, flush2 =
        Aggregator.(accum (now_and_then (2. *. 600.)))
                         Distribution.combine
                         [ fun (vlan, clte, clt, srve, srv, srvp, met, err, ts, host, url) distr ->
                              Table.append table2 (vlan, clte, clt, srve, srv, srvp, met, err, ts, distr, host, url) ;
                              let ts = round_timestamp 3600_000L ts in
                              accum3 (vlan, clte, clt, srve, srv, srvp, met, err, ts, host, url) distr ] in

    let table1 = Web.table dbdir lods.(1) in
    let accum1, flush1 =
        Aggregator.(accum (now_and_then 60.))
                         Distribution.combine
                         [ fun (vlan, clte, clt, srve, srv, srvp, met, err, ts, host, url) distr ->
                              Table.append table1 (vlan, clte, clt, srve, srv, srvp, met, err, ts, distr, host, url) ;
                              let ts = round_timestamp 600_000L ts in
                              accum2 (vlan, clte, clt, srve, srv, srvp, met, err, ts, host, url) distr ] in

    let table0 = Web.table dbdir lods.(0) in

    let shorten_url s =
        try let i = String.index s '?' in
            String.sub s 0 i
        with Not_found -> s in

    let append0 ((vlan, clte, clt, srve, srv, srvp, met, err, ts, distr, host, url) as v) =
        Table.append table0 v ;
        let clt, _mask = clt in
        let clt = cidr_of_inetaddr Subnet.subnets clt
        and ts = round_timestamp 60_000L ts
        and url = shorten_url url in
        accum1 (vlan, clte, clt, srve, srv, srvp, met, err, ts, host, url) distr in

    let flush_all () =
        if !verbose then Printf.printf "Flushing...\n" ;
        flush1 () ;
        flush2 () ;
        flush3 () ;
        Table.close table0 ;
        Table.close table1 ;
        Table.close table2 ;
        Table.close table3 in

    load fname Web.read_txt append0 flush_all

