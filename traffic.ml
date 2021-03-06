open Datatype
open Metric
module Hashtbl = BatHashtbl
let (|>) = BatPervasives.(|>)
let (|?) = BatOption.(|?)

(* FIXME: factorize all this with DNS, or generate this? *)

(* Lod0: Traffic stats for periods of 30s, with fields:
  TS1, TS2, count, vlan, src mac, dst mac, proto, eth payload, eth mtu, src ip, dst ip, ip proto, ip payload, src port, dst port, l4 payload *)

let lods = [| "1min"; "10mins"; "1hour" |];

module Traffic =
struct
    include Altern1 (Tuple17.Make (Origin)                       (* where the record came from *)
                                  (Timestamp) (Timestamp)        (* start, stop *)
                                  (ULeast63)                     (* packet count *)
                                  (VLan) (EthAddr) (EthAddr)     (* Eth vlan, source, dest *)
                                  (UInteger16)                   (* Eth proto *)
                                  (ULeast63)                     (* Eth payload *)
                                  (UInteger16)                   (* Eth MTU *)
                                  (InetAddr) (InetAddr)          (* IP source, dest *)
                                  (UInteger8)                    (* IP proto *)
                                  (ULeast63)                     (* IP payload *)
                                  (UInteger16) (UInteger16)      (* Port source, dest *)
                                  (ULeast63)                     (* L4 payload *))
    (* We'd rather have an inlined reader: *)
    let read ic =
        let tuple17_read ic =
            let t0 = Origin.read ic in
            let t1 = Timestamp.read ic in
            let t2 = Timestamp.read ic in
            let t3 = ULeast63.read ic in
            let t4 =
                let o = Serial.deser8 ic in
                if o <> 0 then (
                    assert (o = 1) ;
                    Some (UInteger16.read ic)
                ) else None in
            let t5 = EthAddr.read ic in
            let t6 = EthAddr.read ic in
            let t7 = UInteger16.read ic in
            let t8 = ULeast63.read ic in
            let t9 = UInteger16.read ic in
            let t10 = InetAddr.read ic in
            let t11 = InetAddr.read ic in
            let t12 = UInteger8.read ic in
            let t13 = ULeast63.read ic in
            let t14 = UInteger16.read ic in
            let t15 = UInteger16.read ic in
            let t16 = ULeast63.read ic in
            t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16 in
        let v = Serial.deser8 ic in
        if v <> 0 then Printf.fprintf stderr "bad version: %d\n%!" v ;
        assert (v = 0) ;
        tuple17_read ic

    (* We hash on the source IP *)
    let hash_on_src (_o, _ts1, _ts2, _count, _vlan, _mac_src, _mac_dst, _proto, _pld, _mtu, ip_src, _ip_dst, _ip_proto, _ip_pld, _l4_src, _l4_dst, _l4_pld) =
        InetAddr.hash ip_src

    (* Metafile stores timestamp range of the whole slice duration *)
    let meta_aggr (_o, ts1, ts2, _count, _vlan, _mac_src, _mac_dst, _proto, _pld, _mtu, _ip_src, _ip_dst, _ip_proto, _ip_pld, _l4_src, _l4_dst, _l4_pld) bound_opt =
        let bound = Aggregator.bounds ~cmp:Timestamp.compare ts1 bound_opt in
        Aggregator.bounds ~cmp:Timestamp.compare ts2 (Some bound)
    let meta_read = BoundsTS.read
    let meta_write = BoundsTS.write

    let table name =
        Table.create (table_name "traffic" name)
            hash_on_src write
            meta_aggr meta_read meta_write

    let iter_fname fname f =
        Table.iter_fname fname read f

    let with_meta fname f =
        match Table.read_meta_fname fname meta_read with
        | Some meta -> f meta
        | None -> ()

    let accum_pkts ((count, eth_pld, mtu, ip_pld, l4_pld) as v) = function
        | 0, _, _, _, _ -> v
        | count', eth_pld', mtu', ip_pld', l4_pld' ->
            count + count',
            eth_pld + eth_pld',
            Pervasives.max mtu mtu',
            ip_pld + ip_pld',
            l4_pld + l4_pld'

    (* Field description for code templates, HTML forms, etc... *)
    let fields = [
        "origin", {
            disp_name = "origin" ;
            help = "origin of this record" ;
            valunit = None ;
            from_prevfields = "" ;
            expr_type = TOrigin ;
            aggrs = [] ;
            sortable = "" ;
            keyable = true ;
            datatype = "Datatype.Origin" ;
            display = "Datatype.Origin.to_string" } ;
        "start", {
            disp_name = "start" ;
            help = "start time of the flow" ;
            valunit = None ;
            from_prevfields = "" ;
            expr_type = TTimestamp ;
            aggrs = [] ;
            sortable = "" ;
            keyable = false ;
            datatype = "Datatype.Timestamp" ;
            display = "Datatype.Timestamp.to_string" } ;
        "stop", {
            disp_name = "stop" ;
            help = "timestamp of the last packet of this flow" ;
            valunit = None ;
            from_prevfields = "" ;
            expr_type = TTimestamp ;
            aggrs = [] ;
            sortable = "" ;
            keyable = false ;
            datatype = "Datatype.Timestamp" ;
            display = "Datatype.Timestamp.to_string" } ;
        "packets", {
            disp_name = "packet count" ;
            help = "number of packets in this flow" ;
            valunit = None ;
            from_prevfields = "" ;
            expr_type = TInteger ;
            aggrs = aggrs_int ;
            sortable = "identity" ;
            keyable = false ;
            datatype = "Datatype.ULeast63" ;
            display = "Datatype.string_of_inumber" } ;
        "vlan", {
            disp_name = "VLAN" ;
            help = "802.1q vlan id" ;
            valunit = None ;
            from_prevfields = "" ;
            expr_type = TVLan ;
            aggrs = [] ;
            sortable = "" ;
            keyable = true ;
            datatype = "Datatype.VLan" ;
            display = "Datatype.VLan.to_string" } ;
        "eth_src", {
            disp_name = "source MAC" ;
            help = "source ethernet address" ;
            valunit = None ;
            from_prevfields = "" ;
            expr_type = TEthAddr ;
            aggrs = [] ;
            sortable = "" ;
            keyable = true ;
            datatype = "Datatype.EthAddr" ;
            display = "Datatype.EthAddr.to_string" } ;
        "eth_dst", {
            disp_name = "destination MAC" ;
            help = "destination ethernet address" ;
            valunit = None ;
            from_prevfields = "" ;
            expr_type = TEthAddr ;
            aggrs = [] ;
            sortable = "" ;
            keyable = true ;
            datatype = "Datatype.EthAddr" ;
            display = "Datatype.EthAddr.to_string" } ;
        "eth_proto", {
            disp_name = "MAC type" ;
            help = "ethernet protocol" ;
            valunit = None ;
            from_prevfields = "" ;
            expr_type = TInteger ;
            aggrs = [] ;
            sortable = "identity" ;
            keyable = true ;
            datatype = "Datatype.UInteger16" ;
            display = "Datatype.string_of_inumber" } ;
        "eth_payload", {
            disp_name = "MAC payload" ;
            help = "total ethernet frame payload" ;
            valunit = Some "bytes" ;
            from_prevfields = "" ;
            expr_type = TInteger ;
            aggrs = aggrs_int ;
            sortable = "identity" ;
            keyable = true ;
            datatype = "Datatype.ULeast63" ;
            display = "Datatype.string_of_inumber" } ;
        "mtu", {
            disp_name = "MTU" ;
            help = "max observed ethernet frame payload" ;
            valunit = Some "bytes" ;
            from_prevfields = "" ;
            expr_type = TInteger ;
            aggrs = ["max", aggr_max_int] ;
            sortable = "identity" ;
            keyable = true ;
            datatype = "Datatype.UInteger16" ;
            display = "Datatype.string_of_inumber" } ;
        "ip_src", {
            disp_name = "source IP" ;
            help = "source IP address (if IP)" ;
            valunit = None ;
            from_prevfields = "" ;
            expr_type = TIp ;
            aggrs = [] ;
            sortable = "" ;
            keyable = true ;
            datatype = "Datatype.InetAddr" ;
            display = "Datatype.InetAddr.to_string" } ;
        "ip_dst", {
            disp_name = "destination IP" ;
            help = "destination IP address (if IP)" ;
            valunit = None ;
            from_prevfields = "" ;
            expr_type = TIp ;
            aggrs = [] ;
            sortable = "" ;
            keyable = true ;
            datatype = "Datatype.InetAddr" ;
            display = "Datatype.InetAddr.to_string" } ;
        "ip_proto", {
            disp_name = "IP protocol" ;
            help = "IP protocol (if IP)" ;
            valunit = None ;
            from_prevfields = "" ;
            expr_type = TInteger ;
            aggrs = [] ;
            sortable = "identity" ;
            keyable = true ;
            datatype = "Datatype.UInteger8" ;
            display = "Datatype.string_of_inumber" } ;
        "ip_payload", {
            disp_name = "IP payload" ;
            help = "IP payload (if IP)" ;
            valunit = Some "bytes" ;
            from_prevfields = "" ;
            expr_type = TInteger ;
            aggrs = aggrs_int ;
            sortable = "identity" ;
            keyable = true ;
            datatype = "Datatype.ULeast63" ;
            display = "Datatype.string_of_inumber" } ;
        "port_src", {
            disp_name = "source port" ;
            help = "source port (if UDP or TCP)" ;
            valunit = None ;
            from_prevfields = "" ;
            expr_type = TInteger ;
            aggrs = aggrs_int ;
            sortable = "identity" ;
            keyable = true ;
            datatype = "Datatype.UInteger16" ;
            display = "Datatype.string_of_inumber" } ;
        "port_dst", {
            disp_name = "destination port" ;
            help = "destination port (if UDP or TCP)" ;
            valunit = None ;
            from_prevfields = "" ;
            expr_type = TInteger ;
            aggrs = aggrs_int ;
            sortable = "identity" ;
            keyable = true ;
            datatype = "Datatype.UInteger16" ;
            display = "Datatype.string_of_inumber" } ;
        "l4_payload", {
            disp_name = "l4 payload" ;
            help = "UDP/TCP payload (if UDP or TCP)" ;
            valunit = Some "bytes" ;
            from_prevfields = "" ;
            expr_type = TInteger ;
            aggrs = aggrs_int ;
            sortable = "identity" ;
            keyable = true ;
            datatype = "Datatype.ULeast63" ;
            display = "Datatype.string_of_inumber" } ]

    (* This is just a way for our dynamically loaded code to reach us *)
    let filter_ = ref (fun (_ : t) -> true)
    let set_filter f = filter_ := f
    let compile_filter ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter () =
        Dynlinker.(load_filter "Traffic.Traffic" ?usr_filter fields
            [ check start     Timestamp.to_imm  "Datatype.Timestamp.compare stop %s >= 0" ;
              check stop      Timestamp.to_imm  "Datatype.Timestamp.compare %s start > 0" ;
              check mac_src   EthAddr.to_imm    "Datatype.EthAddr.equal eth_src %s" ;
              check mac_dst   EthAddr.to_imm    "Datatype.EthAddr.equal eth_dst %s" ;
              check eth_proto UInteger16.to_imm "eth_proto = %s" ;
              check ip_src    Cidr.to_imm       "Datatype.in_cidr ip_src %s" ;
              check ip_dst    Cidr.to_imm       "Datatype.in_cidr ip_dst %s" ;
              check ip        Cidr.to_imm       "(let x = %s in Datatype.in_cidr ip_src x || Datatype.in_cidr ip_dst x)" ;
              check ip_proto  UInteger8.to_imm  "ip_proto = %s" ;
              check port      UInteger16.to_imm "(let x = %s in port_src = x || port_dst = x)" ;
              check vlan      VLan.to_imm       "vlan = %s" ]) ;
        !filter_

    (* We look for semi-closed time interval [start;stop[, but tuples timestamps are closed [ts1;ts2] *)
    let fold_all ?start ?stop ?hash_val name f make_fst merge =
        let tdir = table_name "traffic" name in
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

    let fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter name f make_fst merge =
        let filter = compile_filter ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter () in
        fold_all ?start ?stop ?hash_val:ip_src name (fun x prev ->
            if filter x then f x prev else prev)
            make_fst merge

    let iter ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter name f =
        let dummy_merge _ _ = () in
        fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter name (fun x _ -> f x) ignore dummy_merge

end

(* Querries *)

type plot_what = PacketCount | Volume

let label_of_eth_key vlan_mac = Mac vlan_mac

let label_of_eth_proto = function
    | 0x0800 -> Other "IPv4"
    | 0x86DD -> Other "IPv6"
    | 0x0806 -> Other "ARP"
    | 0x8100 -> Other "802.1q"
    | x -> Other (Printf.sprintf "proto 0x%x" x)

let label_of_ip_key mac_proto ip =
    if mac_proto = 0x0800 || mac_proto = 0x86DD then
        Ip ip
    else label_of_eth_proto mac_proto

(* Returns traffic against time, with a different plot per MAC sockpair *)
let eth_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter ?max_graphs by_src what step name =
    let start, stop = min start stop, max start stop in
    let fold f i m =
        Traffic.fold ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter name (fun (_, t1, t2, count, vlan, mac_src, mac_dst, _, mac_pld, _, _, _, _, _, _, _, _) p ->
            f (vlan, if by_src then mac_src else mac_dst) t1 t2 (if what = PacketCount then count else mac_pld) p)
            i m in
    Plot.per_time ?max_graphs start stop step fold label_of_eth_key (Other "others")

(* Returns traffic per pair of MACs *)
let eth_plot_vol_tot ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter ?(max_graphs=200) what name =
    let start, stop = optmin start stop, optmax start stop in
    let label_of_key (vlan, mac_src, mac_dst) =
        label_of_eth_key (vlan, mac_src),
        label_of_eth_key (vlan, mac_dst) in
    let fold f i m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter name
            (fun (_, t1, t2, count, vlan, mac_src, mac_dst, _, mac_pld, _, _, _, _, _, _, _, _) p ->
                let y = if what = PacketCount then count else mac_pld in
                let y = Plot.clip_y_only ?start ?stop t1 t2 y in
                let k = vlan, mac_src, mac_dst in
                f (k, y) p)
            i m
        in
    assert (max_graphs > 1) ;
    let interm = Plot.FindSignificant.pass1 fold (max_graphs-1) in
    let result, rest = Plot.FindSignificant.pass2' interm fold (max_graphs-1) in
    (* We want to return a hash of src*dst -> value *)
    let h = Hashtbl.create max_graphs in
    Hashtbl.iter (fun k v ->
        Hashtbl.add h (label_of_key k) v)
        result ;
    Hashtbl.add h (Other "other", Other "") rest ;
    Hashtbl.map (fun _k v -> float_of_int v) h (* FIXME: return ints *)

let ip_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter ?(max_graphs=100) by_src what step name =
    let start, stop = min start stop, max start stop in
    let nb_steps = Plot.row_of_time start step stop |> succ in
    (* First pass: find the biggest contributors *)
    let fold1 f i m =
        Traffic.fold ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter name (fun (_, _, _, count, _, _, _, mac_proto, mac_pld, _, src, dst, _, _, _, _, _) p ->
            let key = mac_proto, if by_src then src else dst
            and value = if what = PacketCount then count else mac_pld in
            f (key, value) p)
            i m in
    let interm = Plot.FindSignificant.pass1 fold1 max_graphs in
    let fold2 f i m =
        Traffic.fold ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter name (fun (_, t1, t2, count, _, _, _, mac_proto, mac_pld, _, src, dst, _, _, _, _, _) p ->
            let key = mac_proto, if by_src then src else dst
            and value = if what = PacketCount then count else mac_pld in
            let r1, r2, y = Plot.clip_y_int start stop step t1 t2 value in
            (* Compute the total value for this, ie. the array of volumes *)
            let tv = Plot.Chunk (r1, r2, y) in
            f (key, y, tv) p)
            i m in
    let tv_aggr a1 a2 = Plot.merge_y_array nb_steps a1 a2 in
    let vols, _rest_t, rest_vols, _sum_v, _sum_vols =
        Plot.FindSignificant.pass2 interm fold2 tv_aggr Plot.Empty max_graphs in
    let label_of_key (mac_proto, ip) = label_of_ip_key mac_proto ip in
    (* returns a (string * float array) list *)
    Plot.arrays_of_volume_chunks nb_steps vols rest_vols label_of_key
    (* FIXME: pass2 should return the same as pass2 |> arrays_of_volume_chunks... *)

(* Returns traffic per pair of IPs *)
let ip_plot_vol_tot ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter ?(max_graphs=200) what name =
    let start, stop = optmin start stop, optmax start stop in
    let label_of_key (mac_proto, src, dst) =
        label_of_ip_key mac_proto src,
        label_of_ip_key mac_proto dst in
    let fold f i m =
        Traffic.fold ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter name
            (fun (_, t1, t2, count, _, _, _, mac_proto, mac_pld, _, src, dst, _, _, _, _, _) p ->
                let y = if what = PacketCount then count else mac_pld in
                let y = Plot.clip_y_only ?start ?stop t1 t2 y in
                let k = mac_proto, src, dst in
                f (k, y) p)
            i m
        in
    assert (max_graphs > 1) ;
    let interm = Plot.FindSignificant.pass1 fold (max_graphs-1) in
    let result, rest = Plot.FindSignificant.pass2' interm fold (max_graphs-1) in
    (* We want to return a hash of src*dst -> value *)
    let h = Hashtbl.create max_graphs in
    Hashtbl.iter (fun k v ->
        Hashtbl.add h (label_of_key k) v)
        result ;
    Hashtbl.add h (Other "other", Other "") rest ;
    Hashtbl.map (fun _k v -> float_of_int v) h   (* FIXME: don't convert to float *)


type top_fun = unit -> ((string array option * string array * int * int) list) * int * string array
let dyn_top : top_fun ref = ref (fun () -> failwith "Cannot specialize top function")

let get_top ?start ?stop ?ip_src ?usr_filter ?(max_graphs=20) ?(single_pass=true) sort_by key_fields aggr_fields name =
    let start = optmin start stop
    and stop = optmax start stop in
    let aggr_fields = List.map (fun n -> BatString.split n ".") aggr_fields in
    Dynlinker.((if single_pass then load_top_single_pass else load_top_two_pass) "Traffic" Traffic.fields ?start ?stop ?hash_val:ip_src ?usr_filter ~max_graphs sort_by key_fields aggr_fields name) ;
    !dyn_top ()

(* FIXME: app should be a string, and we should also report various eth apps *)

let label_of_app_key (proto, port) =
    let proto = try Unix.((getprotobynumber proto).p_name)
                with Not_found -> "" in
    let serv = try Unix.((getservbyport port proto).s_name)
               with Not_found -> string_of_int port in
    if String.length proto = 0 then Other serv
    else if String.length serv = 0 then Other proto
    else Other (proto ^ "/" ^ serv)

(* Returns traffic against time, with a different plot per proto/port *)
let app_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter ?max_graphs what step name =
    let start, stop = min start stop, max start stop in
    let fold f i m =
        Traffic.fold ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter name (fun (_, t1, t2, count, _, _, _, _, mac_pld, _, _, _, proto, _, p1, p2, _) p ->
            f (proto, min p1 p2) t1 t2 (if what = PacketCount then count else mac_pld) p)
            i m in
    Plot.per_time ?max_graphs start stop step fold label_of_app_key (Other "others")

let network_graph start stop ?min_volume ?vlan ?eth_proto ?ip_proto ?port ?usr_filter show_mac show_ip name =
    let start, stop = min start stop, max start stop in
    let fold f i m =
        Traffic.fold ~start ~stop ?vlan ?eth_proto ?ip_proto ?port ?usr_filter name
            (fun (_, t1, t2, _, vlan, mac_src, mac_dst, mac_proto, mac_pld, _, ip_src, ip_dst, _, _, _, _, _) p ->
                let y = mac_pld in
                let y = Plot.clip_y_only ~start ~stop t1 t2 y in
                (* FIXME: wait till Plot.netgraph is done before converting keys
                 * to string representation !*)
                let p = if show_mac then (
                        let src, dst, y = if EthAddr.compare mac_src mac_dst <= 0 then mac_src, mac_dst, y
                                                                                  else mac_dst, mac_src, ~-y in
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
                                                                                 else ip_dst, ip_src, ~-y in
                        f (Ip src) (Ip dst) y p
                    )
                ) else p)
            i m
        in
    let graph = Plot.netgraph fold (+) in
    let graph =
        match min_volume with
        | None ->
            graph
        | Some min_volume ->
            Hashtbl.filter_map (fun _k1 n ->
                let n' = Hashtbl.filter (fun y -> y >= min_volume) n in
                if Hashtbl.is_empty n' then None
                else Some n') graph in
    (* Convert values to float (TODO: don't) *)
    let res = Hashtbl.create (Hashtbl.length graph) in
    Hashtbl.iter (fun k1 n ->
        let n' = Hashtbl.create (Hashtbl.length n) in
        Hashtbl.iter (fun k2 y -> Hashtbl.add n' k2 (float_of_int y)) n ;
        Hashtbl.add res k1 n')
        graph ;
    res

let network_map start stop ?min_volume ?vlan ?eth_proto ?ip_proto ?port ?usr_filter name =
    let max_ips = Integer.of_pref "CPN_GEOIP_MAX_IPS" 10 in
    let start, stop = min start stop, max start stop in
    Geoip.init () ;
    let location ip =
        try Geoip.location ip
        with Failure _ -> ("", 0., 0.) in
    let fold f i m =
        Traffic.fold ~start ~stop ?vlan ?eth_proto ?ip_proto ?port ?usr_filter name
            (fun (_, t1, t2, _, _, _, _, mac_proto, mac_pld, _, ip_src, ip_dst, _, _, _, _, _) p ->
                if mac_proto = 0x0800 || mac_proto = 0x86DD then (
                    let y = mac_pld in
                    let y = Plot.clip_y_only ~start ~stop t1 t2 y in
                    let loc1 = location ip_src and loc2 = location ip_dst in
                    let loc1, loc2, ip1, ip2, y_up, y_do =
                        if compare loc1 loc2 <= 0 then loc1, loc2, ip_src, ip_dst, y, 0
                                                  else loc2, loc1, ip_dst, ip_src, 0, y in
                    f loc1 loc2 ([ip1], [ip2], y_up, y_do) p
                ) else p)
            i m
    and aggr (src1, dst1, y_up1, y_do1) (src2, dst2, y_up2, y_do2) =
        list_merge_lim max_ips src1 src2,
        list_merge_lim max_ips dst1 dst2,
        y_up1+y_up2, y_do1+y_do2 in
    let graph = Plot.netgraph fold aggr in
    let graph =
        match min_volume with
        | None ->
            graph
        | Some min_volume ->
            Hashtbl.filter_map (fun _k1 n ->
                let n' = Hashtbl.filter (fun (_,_,y_up,y_do) -> y_up+y_do >= min_volume) n in
                if Hashtbl.is_empty n' then None
                else Some n') graph in
    graph

(* Lod1: Accumulated over 10mins *)
(* Lod2: round timestamp to hour *)
(* Load new data into the database *)

let load fname =
    let table2 = Traffic.table lods.(2) in
    let accum2, flush2 =
        Aggregator.(accum (now_and_then (buffer_duration_of_lod lods.(2) "traffic"))) Traffic.accum_pkts
            [ fun (orig, start, stop, vlan, mac_src, mac_dst, mac_proto, ip_src, ip_dst, ip_proto, l4_src, l4_dst) (count, eth_pld, mtu, ip_pld, l4_pld) ->
                Table.append table2
                    (orig, start, stop, count, vlan, mac_src, mac_dst, mac_proto, eth_pld, mtu, ip_src, ip_dst, ip_proto, ip_pld, l4_src, l4_dst, l4_pld) ] in

    let table1 = Traffic.table lods.(1) in
    let rti = rti_of_lod lods.(2) "traffic" in
    let accum1, flush1 =
        Aggregator.(accum (now_and_then (buffer_duration_of_lod lods.(1) "traffic"))) Traffic.accum_pkts
            [ fun (orig, start, stop, vlan, mac_src, mac_dst, mac_proto, ip_src, ip_dst, ip_proto, l4_src, l4_dst) (count, eth_pld, mtu, ip_pld, l4_pld) ->
                Table.append table1
                    (orig, start, stop, count, vlan, mac_src, mac_dst, mac_proto, eth_pld, mtu, ip_src, ip_dst, ip_proto, ip_pld, l4_src, l4_dst, l4_pld) ;
                BatOption.may (fun rti ->
                    let start, stop = round_time_interval rti start stop in
                    accum2 (orig, start, stop, vlan, mac_src, mac_dst, mac_proto, ip_src, ip_dst, ip_proto, l4_src, l4_dst) (count, eth_pld, mtu, ip_pld, l4_pld))
                    rti ] in

    let table0 = Traffic.table lods.(0) in
    let rti = rti_of_lod lods.(1) "traffic" in
    let append0 ((orig, start, stop, count, vlan, mac_src, mac_dst, mac_proto, eth_pld, mtu, ip_src, ip_dst, ip_proto, ip_pld, l4_src, l4_dst, l4_pld) as v) =
        Table.append table0 v ;
        BatOption.may (fun rti ->
            let start, stop = round_time_interval rti start stop in
            accum1 (orig, start, stop, vlan, mac_src, mac_dst, mac_proto, ip_src, ip_dst, ip_proto, l4_src, l4_dst) (count, eth_pld, mtu, ip_pld, l4_pld))
            rti in

    let flush_all () =
        flush1 () ;
        flush2 () ;
        Table.close table0 ;
        Table.close table1 ;
        Table.close table2 in

    load fname Traffic.parzer append0 flush_all

(*$T
  match Traffic.parzer (String.to_list \
    "iface eth0\t1323766040s 992799us\t1323766042s 539807us\t274\tSome 250\tb4:a4:e3:4d:5c:01\t88:43:e1:1d:6d:01\t2048\t309884\t1409\t193.48.95.81\t134.206.1.47\t17\t304404\t54694\t49164\t302212") with \
    | Peg.Res (_, []) -> true \
    | _ -> false
 *)
