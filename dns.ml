open Batteries
open Datatype
open Metric

let string_of_err err =
    if err = 0 then "" else Printf.sprintf "err: %d" err

let lods = [| "queries"; "1min"; "1hour" |];

(* Lod0: the full request record *)

module Dns =
struct
    include Altern1 (Tuple10.Make (Origin)               (* where the record came from *)
                                  (VLan)                 (* VLAN *)
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
        let tuple10_read ic =
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
            let t6 = Integer8.read ic in
            let t7 = Timestamp.read ic in
            let t8 = Distribution.read ic in
            let t9 = Text.read ic in
            t0,t1,t2,t3,t4,t5,t6,t7,t8,t9 in
        let v = Serial.deser8 ic in
        if v <> 0 then Printf.fprintf stderr "bad version: %d\n%!" v ;
        assert (v = 0) ;
        tuple10_read ic

    (* We hash on the server IP *)
    let hash_on_srv (_orig, _vlan, _clte, _clt, _srve, srv, _err, _ts, _rt, _name) =
        InetAddr.hash srv

    (* Metafile stores timestamp range *)
    let meta_aggr (_orig, _vlan, _clte, _clt, _srve, _srv, _err, ts, _rt, _name) =
        Aggregator.bounds ~cmp:Timestamp.compare ts
    let meta_read = BoundsTS.read
    let meta_write = BoundsTS.write

    let table name =
        Table.create (table_name "dns" name)
            hash_on_srv write
            meta_aggr meta_read meta_write

    (* Field description *)
    let fields = [
        "origin", {
            disp_name = "origin" ;
            help = "origin of this record" ;
            from_prevfields = "" ;
            expr_type = TOrigin ;
            aggrs = [] ;
            sortable = "" ;
            keyable = true ;
            datatype = "Datatype.Origin" ;
            display = "Datatype.Origin.to_string" } ;
        "vlan", {
            disp_name = "VLAN" ;
            help = "802.1q vlan id" ;
            from_prevfields = "" ;
            expr_type = TVLan ;
            aggrs = [] ;
            sortable = "" ;
            keyable = true ;
            datatype = "Datatype.VLan" ;
            display = "Datatype.VLan.to_string" } ;
        "mac_clt", {
            disp_name = "client MAC" ;
            help = "client ethernet address" ;
            from_prevfields = "" ;
            expr_type = TEthAddr ;
            aggrs = [] ;
            sortable = "" ;
            keyable = true ;
            datatype = "Datatype.EthAddr" ;
            display = "Datatype.EthAddr.to_string" } ;
        "ip_clt", {
            disp_name = "client IP" ;
            help = "client IP address" ;
            from_prevfields = "" ;
            expr_type = TIp ;
            aggrs = [] ;
            sortable = "" ;
            keyable = true ;
            datatype = "Datatype.Cidr" ;
            display = "Datatype.Cidr.to_string" } ;
        "mac_srv", {
            disp_name = "server MAC" ;
            help = "server ethernet address" ;
            from_prevfields = "" ;
            expr_type = TEthAddr ;
            aggrs = [] ;
            sortable = "" ;
            keyable = true ;
            datatype = "Datatype.EthAddr" ;
            display = "Datatype.EthAddr.to_string" } ;
        "ip_srv", {
            disp_name = "server IP" ;
            help = "server IP address" ;
            from_prevfields = "" ;
            expr_type = TIp ;
            aggrs = [] ;
            sortable = "" ;
            keyable = true ;
            datatype = "Datatype.InetAddr" ;
            display = "Datatype.InetAddr.to_string" } ;
        "error", {
            disp_name = "error" ;
            help = "response code" ;
            from_prevfields = "" ;
            expr_type = TInteger ;
            aggrs = [] ;
            sortable = "" ;
            keyable = true ;
            datatype = "Datatype.Integer8" ;
            display = "Datatype.Integer8.to_string" } ;
        "start", {
            disp_name = "start" ;
            help = "timestamp of the query" ;
            from_prevfields = "" ;
            expr_type = TTimestamp ;
            aggrs = [] ;
            sortable = "" ;
            keyable = false ;
            datatype = "Datatype.Timestamp" ;
            display = "Datatype.Timestamp.to_string" } ;
        "resptime", {
            disp_name = "response time" ;
            help = "response time" ;
            from_prevfields = "" ;
            expr_type = TFloat ;
            aggrs = aggrs_distribution ;
            sortable = "(fun d -> Int.of_float (1000. *. Distribution.avg d))" ; (* we sort by avg value (in msec) *)
            keyable = false ;
            datatype = "Distribution" ;
            display = "Distribution.to_string" } ;
        "queries", {
            disp_name = "query count" ;
            help = "number of queries" ;
            from_prevfields = "Distribution.count resptime" ;
            expr_type = TInteger ;
            aggrs = aggrs_int ;
            sortable = "identity" ;
            keyable = false ;
            datatype = "Least63" ;
            display = "Least63.to_string" } ;
        "name", {
            disp_name = "name" ;
            help = "query name" ;
            from_prevfields = "" ;
            expr_type = TText ;
            aggrs = [] ;
            sortable = "" ; (* TODO: an int_of_string_for_sort *)
            keyable = true ;
            datatype = "Datatype.Text" ;
            display = "Datatype.Text.to_string" } ;
        "stop", {
            disp_name = "stop" ;
            help = "timestamp of the end of the transaction" ;
            from_prevfields = "Datatype.Timestamp.add_secs start (Distribution.maxi resptime)" ;
            expr_type = TTimestamp ;
            aggrs = [] ;
            sortable = "" ;
            keyable = false ;
            datatype = "Datatype.Timestamp" ;
            display = "Datatype.Timestamp.to_string" }
    ]

    (* This is just a way for our dynamically loaded code to reach us *)
    let filter_ = ref (fun (_ : t) -> true)
    let set_filter f = filter_ := f
    let compile_filter ?start ?stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?peer ?error ?qname ?rt_min ?rt_max ?tx_min ?usr_filter () =
        Dynlinker.(load_filter "Dns.Dns" ?usr_filter fields
            [ check start     Timestamp.to_imm  "Datatype.Timestamp.compare stop %s >= 0" ;
              check stop      Timestamp.to_imm  "Datatype.Timestamp.compare %s start > 0" ;
              check mac_clt   EthAddr.to_imm    "Datatype.EthAddr.equal eth_clt %s" ;
              check mac_srv   EthAddr.to_imm    "Datatype.EthAddr.equal eth_srv %s" ;
              check ip_clt    Cidr.to_imm       "Datatype.in_cidr ip_clt %s" ;
              check ip_srv    Cidr.to_imm       "Datatype.in_cidr ip_srv %s" ;
              check peer      Cidr.to_imm       "(let x = %s in Datatype.in_cidr ip_clt x || Datatype.in_cidr ip_srv x)" ;
              check vlan      VLan.to_imm       "vlan = %s" ;
              check error     Integer8.to_imm   "error = %s" ;
              check qname     Text.to_imm       "Metric.string_ends_with %s name" ;
              check rt_min    Float.to_imm      "Distribution.avg resptime >= %s" ;
              check rt_max    Float.to_imm      "Distribution.avg resptime <= %s" ;
              check tx_min    ULeast63.to_imm   "Distribution.count resptime >= %s" ]) ;
        !filter_

    let fold_all ?start ?stop ?hash_val name f make_fst merge =
        let tdir = table_name "dns" name in
        let fold_hnum hnum fst =
            Table.fold_snums tdir hnum meta_read (fun snum bounds prev ->
                let res =
                    if is_within bounds start stop then (
                        Table.fold_file tdir hnum snum read f prev
                    ) else (
                        prev
                    ) in
                res)
                fst merge in
        fold_using_indexed hash_val tdir fold_hnum make_fst merge

    let fold ?start ?stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?peer ?error ?qname ?rt_min ?rt_max ?tx_min ?usr_filter name f make_fst merge =
        let filter = compile_filter ?start ?stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?peer ?error ?qname ?rt_min ?rt_max ?tx_min ?usr_filter () in
        fold_all ?start ?stop ?hash_val:ip_srv name (fun x prev ->
            if filter x then f x prev else prev)
            make_fst merge

    let iter ?start ?stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?peer ?error ?qname ?rt_min ?rt_max ?tx_min name f =
        let dummy_merge _ _ = () in
        fold ?start ?stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?peer ?error ?qname ?rt_min ?rt_max ?tx_min name (fun x _ -> f x) ignore dummy_merge

end

let plot_resp_time start stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?rt_min ?rt_max ?tx_min step name =
    let start, stop = min start stop, max start stop in
    let fold f i m =
        Dns.fold ~start ~stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?rt_min ?rt_max ?tx_min name
            (fun (_orig, _vlan, _mac_clt, _clt, _mac_srv, _srv, _err, ts, rt, _name) p ->
                f ts rt p)
            i m in
    Plot.per_date start stop step fold

let top_requests start stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?rt_min ?rt_max ?error ?qname n sort_order =
    let start, stop = min start stop, max start stop in
    let fold = Dns.fold ~start ~stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?rt_min ?rt_max ?error ?qname lods.(0) in
    let cmp (_o1, _vl1, _eclt1, _clt1, _esrv1, _srv1, _err1, _ts1, (_, _, _, rt1, _), _nm1)
            (_o2, _vl2, _eclt2, _clt2, _esrv2, _srv2, _err2, _ts2, (_, _, _, rt2, _), _nm2) =
        Float.compare rt1 rt2 in
    Plot.top_table n sort_order cmp fold

(* Display in it's own color the ip_srvs that represent more than 1/top_nth share of the tot
 * number of queries *)
let plot_distrib start stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?rt_min ?rt_max ?prec ?(top_nth=100) tblname =
    let fold f i m =
        Dns.fold ~start ~stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?rt_min ?rt_max tblname
            (fun (_orig, _vl, _clte, _clt, _srve, srv, _err, _ts, rt, _name) p ->
                let nb_queries, _, _, _, _ = rt in
                f (srv, nb_queries) p)
            i m in
    let interm = Plot.FindSignificant.pass1 fold top_nth in
    let fold2 f i m =
        Dns.fold ~start ~stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?rt_min ?rt_max tblname
            (fun (_orig, _vl, _clte, _clt, _srve, srv, _err, _ts, rt, _name) p ->
                let nb_queries, _, _, avg, _ = rt in
                (* FIXME: instead of a single [avg], return the whole distrib so that we can
                 *        later fake a (gaussian) distribution of nb_queries values, or merely
                 *        reports the correct number of queries *)
                f (srv, nb_queries, [avg]) p)
            i m
    and aggr_rts prev_rts rts = List.rev_append rts prev_rts in
    let result, _rest_count, rest_rts, _sum_v, _sum_tv = Plot.FindSignificant.pass2 interm fold2 aggr_rts [] top_nth in
    Plot.distributions_of_response_times ?prec result rest_rts InetAddr.to_string

(* Contrary to top request which return a list of queries from the query table, here we can query freely anything *)
type top_fun = unit -> ((string array option * string array * int * int) list) * int * string array
let dyn_top : top_fun ref = ref (fun () -> failwith "Cannot specialize top function")
let get_top ?start ?stop ?ip_srv ?usr_filter ?(max_graphs=20) ?(single_pass=true) sort_by key_fields aggr_fields name =
    let start = optmin start stop
    and stop = optmax start stop in
    let aggr_fields = List.map (fun n -> BatString.split n ".") aggr_fields in
    Dynlinker.((if single_pass then load_top_single_pass else load_top_two_pass) "Dns" Dns.fields ?start ?stop ?hash_val:ip_srv ?usr_filter ~max_graphs sort_by key_fields aggr_fields name) ;
    !dyn_top ()

(* Lod1: degraded client, rounded query_date (to 1min), distribution of resptimes *)
(* Lod2: round timestamp to 10 mins *)
(* Lod3: and finally to hour *)

(* Load new data into the database *)

let load fname =
    let table2 = Dns.table lods.(2) in
    let accum2, flush2 =
        Aggregator.(accum (now_and_then (buffer_duration_of_lod lods.(2) "dns")))
                         Distribution.combine
                         [ fun (orig, vlan, clte, clt, srve, srv, err, ts, name) distr ->
                              Table.append table2 (orig, vlan, clte, clt, srve, srv, err, ts, distr, name) ] in

    let table1 = Dns.table lods.(1) in
    let rti = rti_of_lod lods.(2) "dns" in
    let accum1, flush1 =
        Aggregator.(accum (now_and_then (buffer_duration_of_lod lods.(1) "dns")))
                         Distribution.combine
                         [ fun (orig, vlan, clte, clt, srve, srv, err, ts, name) distr ->
                              Table.append table1 (orig, vlan, clte, clt, srve, srv, err, ts, distr, name) ;
                              BatOption.may (fun rti ->
                                  let ts = round_timestamp rti ts in
                                  (* TODO: keep only the last host name + TLD in the name *)
                                  accum2 (orig, vlan, clte, clt, srve, srv, err, ts, name) distr)
                                  rti ] in

    let table0 = Dns.table lods.(0) in
    let rti = rti_of_lod lods.(1) "dns" in
    let append0 ((orig, vlan, clte, clt, srve, srv, err, ts, distr, name) as v) =
        Table.append table0 v ;
        BatOption.may (fun rti ->
            let clt, mask = clt in
            assert (mask = 32 || mask = 128) ; (* the mask is supposed to be total *)
            let clt = cidr_of_inetaddr Subnet.subnets clt
            and ts = round_timestamp rti ts in
            accum1 (orig, vlan, clte, clt, srve, srv, err, ts, name) distr)
            rti in

    let flush_all () =
        flush1 () ;
        flush2 () ;
        Table.close table0 ;
        Table.close table1 ;
        Table.close table2 in

    load fname Dns.parzer append0 flush_all

