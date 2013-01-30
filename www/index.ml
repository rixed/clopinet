(* CGI script for visualizing mlrrd datas *)
open Batteries
module View = My_view
module Forms = My_forms
open Html
open Input.Ops
open Datatype

let dbdir = Prefs.get_string "db/base_dir" "./test.db"

let i2s ?min ?max i =
    let (>>=) = BatOption.bind in
    BatOption.map Interval.to_secs i >>=
    (fun v -> match min with
        | Some mi -> Some (Pervasives.max v mi)
        | None -> Some v) >>=
    (fun v -> match max with
        | Some ma -> Some (Pervasives.min v ma)
        | None -> Some v)

(* TODO: we can't easily reset prev_str because we don't know when plotters will use this
 * function in anothern axis... for instance imagine we have dates for both X and Y axis... *)
let prev_str = ref ("","","")
let string_of_date ts =
    let full = Timestamp.of_unixfloat ts |>
               Timestamp.to_string in
    (* Compare with previous displayed value *)
    let d,rest = try String.split full ~by:" "
                 with Not_found -> full, "" in
    let h,m = try String.split rest ~by:"."
              with Not_found -> rest, "" in
    let prev_d, prev_h, prev_m = !prev_str in
    let show_date = d <> prev_d in
    let show_msecs = m <> prev_m in
    let show_hour = (show_msecs && show_date) || h <> prev_h in
    let buf = Buffer.create (String.length full) in
    if show_date then Buffer.add_string buf d ;
    if show_hour then (
        if show_date then Buffer.add_string buf "\n" ;
        Buffer.add_string buf h
    ) ;
    if show_msecs then (
        if show_hour then Buffer.add_string buf "." ;
        Buffer.add_string buf m
    ) ;
    prev_str := d, h, m ;
    Buffer.contents buf

module Ctrl =
struct
    include Ctrl

    (* returns the 'user-id' *)
    let auth name passwd =
        if name = "admin" && passwd = "secret" then Some 1 else None

    (* Main app page *)
    let main _getter =
        let username = try Sys.getenv "REMOTE_USER"
                       with Not_found -> "you!" in
        let msg = "Hello "^username in
        [ cdata msg ]

    let display_errs (f : 'a -> 'b -> 'c) (p1 : 'a) (p2 : 'b) : 'c option =
        try Some (f p1 p2)
        with InputError str -> View.add_err str ; None
           | exc -> View.add_exc exc ; None

    (* DB search pages *)
    module Traffic =
    struct
        include Traffic
        let dbdir = dbdir^"/traffic"

        let bandwidth_chart = function
            | None ->
                [ p ~cls:"err" [ cdata "No selection" ] ]
            | Some (start, (stop, (vlan, (mac_src, (mac_dst, (eth_proto, (ip_src, (ip_dst, (ip, (ip_proto, (port, (usr_filter, (time_step, (tblname, (what, (group_by, (max_graphs, ()))))))))))))))))) ->
                let time_step = Interval.to_ms time_step
                and start = My_time.to_timeval start
                and stop  = My_time.to_timeval stop
                and tblname = Forms.Traffic.TblNames.options.(tblname)
                and what = if what = 0 then Volume else PacketCount in
                let datasets = match group_by with
                    | 1 (* src-mac *) | 2 (* dst-mac *) as sd ->
                        eth_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter ?max_graphs (sd = 1) what time_step dbdir tblname
                    | 3 (* src-ip *) | 4 (* dst-ip *) as sd ->
                        ip_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter ?max_graphs (sd = 3) what time_step dbdir tblname
                    | _ (* default, apps *) ->
                        app_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter ?max_graphs what time_step dbdir tblname in
                if datasets = [] then []
                else
                    let what = if what = PacketCount then "Packets" else "Bytes" in
                    let nb_vx =
                        List.fold_left (fun ma (_l, a) ->
                            max ma (Array.length a))
                            0 datasets in
                    let time_step = (Int64.to_float time_step) *. 0.001
                    and svg_width  = Prefs.get_float "gui/svg/width" 1000.
                    and svg_height = Prefs.get_float "gui/svg/height" 600. in
                    let fold f i =
                        List.fold_left (fun prev (l, a) ->
                            let get i = Array.get a i in
                            f prev l true get)
                            i datasets in
                    Chart.xy_plot ~svg_width ~svg_height ~stacked:Chart.Stacked
                                  ~force_show_0:true ~string_of_x:string_of_date
                                  "time" what
                                  (Timestamp.to_unixfloat start) time_step nb_vx
                                  { Chart.fold = fold }

        let bandwidth getter =
            let selection = display_errs Forms.Traffic.Bandwidth.from "filter" getter in
            let chart = bandwidth_chart selection
            and filters_form = form "Traffic/bandwidth" (Forms.Traffic.Bandwidth.to_edit "filter" getter) in
            View.make_chart "Bandwidth" filters_form chart

        let peers_chart = function
            | Some (start, (stop, (vlan, (mac_src, (mac_dst, (eth_proto, (ip_src, (ip_dst, (ip, (ip_proto, (port, (usr_filter, (tblname, (what, (group_by, (max_graphs, ())))))))))))))))) ->
                let tblname = Forms.Traffic.TblNames.options.(tblname)
                and start = My_time.to_timeval start
                and stop  = My_time.to_timeval stop
                and what = if what = 0 then Volume else PacketCount in
                let datasets = match group_by with
                    | 0 (* mac *) ->
                        eth_plot_vol_tot ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter ?max_graphs what dbdir tblname
                    | _ (* ip *) ->
                        ip_plot_vol_tot ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?usr_filter ?max_graphs what dbdir tblname in
                if Hashtbl.is_empty datasets then
                    []
                else
                    let is_bytes = what = Volume in
                    View.peers_chart ~is_bytes datasets
            | None -> []

        let peers getter =
            let selection = display_errs Forms.Traffic.Peers.from "filter" getter in
            let chart = peers_chart selection
            and filters_form = form "Traffic/peers" (Forms.Traffic.Peers.to_edit "filter" getter) in
            View.make_chart "Peers" filters_form chart

        let graph getter =
            let filters_form = form "Traffic/graph" (Forms.Traffic.Graph.to_edit "filter" getter) in
            let disp_graph = match display_errs Forms.Traffic.Graph.from "filter" getter with
                | Some (start, (stop, (vlan, (eth_proto, (ip_proto, (port, (min_volume, (usr_filter, (layout, (tblname, (group_by, ()))))))))))) ->
                    let tblname = Forms.Traffic.TblNames.options.(tblname)
                    and start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop
                    and show_ip = group_by <> 2 and show_mac = group_by <> 1 in
                    let datasets = network_graph start stop ?min_volume ?vlan ?eth_proto ?ip_proto ?port ?usr_filter show_mac show_ip dbdir tblname in
                    if Hashtbl.is_empty datasets then
                        []
                    else
                        View.peers_graph datasets Forms.Traffic.LayoutType.options.(layout)
                | None -> [] in
            View.make_chart "Network" filters_form disp_graph

        let top getter =
            let filters_form = form "Traffic/top/show" (Forms.Traffic.Tops.to_edit "filter" getter) in
            View.make_filter "Top Traffic" filters_form

        let top_show getter =
            let filters_form = form "Traffic/top/show" (Forms.Traffic.Tops.to_edit "filter" getter) in
            let disp_graph = match display_errs Forms.Traffic.Tops.from "filter" getter with
                | Some (start, (stop, (_vlan, (_mac_src, (_mac_dst, (_eth_proto, (ip_src, (_ip_dst, (_ip, (_ip_proto, (_port, (usr_filter, (tblname, (group_by, (aggr_fields, (sort_by, (max_graphs, ()))))))))))))))))) ->
                    let tblname = Forms.Traffic.TblNames.options.(tblname)
                    and start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop in
                    let datasets = get_top ~start ~stop ?ip_src ?usr_filter ?max_graphs sort_by group_by aggr_fields dbdir tblname in
                    View.table_of_datasets group_by aggr_fields sort_by datasets
                | None -> [] in
            View.make_chart "Top Traffic" filters_form disp_graph

        let map getter =
            let filters_form = form "Traffic/map/show" (Forms.Traffic.Map.to_edit "filter" getter) in
            View.make_filter "Traffic Map" filters_form

        let map_show getter =
            let filters_form = form "Traffic/map/show" (Forms.Traffic.Map.to_edit "filter" getter) in
            let ip_map = match display_errs Forms.Traffic.Map.from "filter" getter with
                | Some (start, (stop, (vlan, (eth_proto, (ip_proto, (port, (min_volume, (usr_filter, (tblname, ()))))))))) ->
                    let tblname = Forms.Traffic.TblNames.options.(tblname)
                    and start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop in
                    let datasets = network_map start stop ?min_volume ?vlan ?eth_proto ?ip_proto ?port ?usr_filter dbdir tblname in
                    if Hashtbl.is_empty datasets then []
                    else
                        View.peers_map datasets
                | None -> [] in
            View.make_chart "Traffic Map" filters_form ip_map

    end

    module Flow =
    struct
        include Flow
        let flow_dbdir = dbdir^"/flow"
        let callflow getter =
            let filters_form = form "Traffic/callflow" (Forms.Flow.Callflow.to_edit "filter" getter) in
            let disp_graph = match display_errs Forms.Flow.Callflow.from "filter" getter with
                | Some (start, (stop, (vlan, (ip_start, (ip_dst, (ip_proto, (port_src, (port_dst, ())))))))) ->
                    let start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop in
                    let datasets =
                        get_callflow start stop ?vlan ip_start ?ip_dst ?ip_proto ?port_src ?port_dst
                                     ~dns_dbdir:(dbdir^"/dns") ~web_dbdir:(dbdir^"/web")
                                     ~tcp_dbdir:(dbdir^"/tcp") flow_dbdir in
                    View.callflow_chart (InetAddr.to_string ip_start) datasets
                | None -> [] in
            View.make_chart "Call Flow" filters_form disp_graph
    end

    module Web =
    struct
        include Web
        let dbdir = dbdir^"/web"
        let url_name host port url =
            host ^
            (if port = 80 then "" else (":" ^ string_of_int port)) ^
            url

        let top getter =
            let filters_form = form "Web/top/show" (Forms.Web.Top.to_edit "filter" getter) in
            View.make_filter "Web Top Requests" filters_form

        let top_show getter =
            let filters_form = form "Web/top" (Forms.Web.Top.to_edit "filter" getter) in
            let disp_graph = match display_errs Forms.Web.Top.from "filter" getter with
                | Some (start, (stop, (vlan, (mac_clt, (mac_srv, (client, (server, (methd, (status, (host, (url, (rt_min, (rt_max, (n, (sort_order, ()))))))))))))))) ->
                    let n = BatOption.default 30 n
                    and start  = My_time.to_timeval start
                    and stop   = My_time.to_timeval stop in
                    let rt_min = i2s ~min:0. rt_min in
                    let rt_max = i2s ?min:rt_min rt_max in
                    let sort_order = match sort_order with 0 -> Plot.Asc | _ -> Plot.Desc in
                    let tops = top_requests start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?methd ?status ?host ?url ?rt_min ?rt_max dbdir n sort_order in
                    let field_display_names =
                        [ "VLAN" ;
                          "Client MAC" ; "Client IP" ;
                          "Server MAC" ; "Server IP" ;
                          "Method"     ; "Error Code" ;
                          "Timestamp" ;
                          "Response Time (&#x00B5s)" ;
                          "URL" ] in
                    View.tops_table tops field_display_names (fun (vlan, eclt, clt, esrv, srv, port, meth, err, ts, (_, _, _, rt, _), host, url) ->
                        [ string_of_vlan vlan ;
                          EthAddr.to_string eclt ;
                          Cidr.to_string clt ;
                              EthAddr.to_string esrv ;
                              InetAddr.to_string srv ;
                              string_of_method meth ;
                              string_of_int err ;
                              Timestamp.to_string ts ;
                              string_of_float rt ;
                              url_name host port url ])
                | None -> [] in
            View.make_chart "Web Top Requests" filters_form disp_graph

        let resp_time getter =
            let filters_form = form "Web/resptime" (Forms.Web.RespTime.to_edit "filter" getter) in
            let disp_graph = match display_errs Forms.Web.RespTime.from "filter" getter with
                | Some (start, (stop, (vlan, (mac_clt, (mac_srv, (client, (server, (methd, (status, (host, (url, (rt_min, (rt_max, (time_step, (tblname, ()))))))))))))))) ->
                    let time_step = Interval.to_ms time_step (* FIXME: plot_resp_time should take seconds instead *)
                    and start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop in
                    let rt_min = i2s ~min:0. rt_min in
                    let rt_max = i2s ?min:rt_min rt_max in
                    let tblname = Forms.Web.TblNames.options.(tblname) in
                    let datasets = plot_resp_time start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?methd ?status ?host ?url ?rt_min ?rt_max time_step dbdir tblname in
                    (* TODO: plot_resp_time should return 0 in count instead of None... *)
                    let fold f i =
                        let i = f i "Min" true
                            (fun i -> Distribution.min datasets.(i)) in
                        let i = f i "Avg-&sigma;" true
                            (fun i -> max 0. Distribution.(avg datasets.(i) -. std_dev datasets.(i))) in
                        let i = f i "Avg" true
                            (fun i -> Distribution.avg datasets.(i)) in
                        let i = f i "Avg+&sigma;" true
                            (fun i -> Distribution.(avg datasets.(i) +. std_dev datasets.(i))) in
                        let i = f i "Max" true
                            (fun i -> Distribution.max datasets.(i)) in
                        let i = f i "#Transactions" false
                            (fun i -> Distribution.count datasets.(i) |> float_of_int) in
                        i in
                    let nb_vx = Array.length datasets in
                    let time_step = (Int64.to_float time_step) *. 0.001
                    and svg_width  = Prefs.get_float "gui/svg/width" 1000.
                    and svg_height = Prefs.get_float "gui/svg/height" 600. in
                    Chart.xy_plot ~svg_width ~svg_height
                                  ~string_of_x:string_of_date ~stacked:Chart.Stacked
                                  "time" "response time (s)"
                                  (Timestamp.to_unixfloat start) time_step nb_vx
                                  { Chart.fold = fold }
                | None -> [] in
            View.make_chart "Web Response Time" filters_form disp_graph

        let distrib getter =
            let filters_form = form "Web/distrib/show" (Forms.Web.Distrib.to_edit "filter" getter) in
            View.make_filter "Web Response Times" filters_form

        let distrib_show getter =
            let filters_form = form "Web/distrib/show" (Forms.Web.Distrib.to_edit "filter" getter) in
            let disp_graph = match display_errs Forms.Web.Distrib.from "filter" getter with
                | Some (start, (stop, (vlan, (mac_clt, (mac_srv, (client, (server, (methd, (status, (host, (url, (rt_min, (rt_max, (prec, (top_nth, (tblname, ())))))))))))))))) ->
                    let tblname = Forms.Dns.TblNames.options.(tblname)
                    and start  = My_time.to_timeval start
                    and stop   = My_time.to_timeval stop in
                    let rt_min = i2s ~min:0. rt_min in
                    let rt_max = i2s ?min:rt_min rt_max in
                    let prec   = i2s ~min:0.00001 ~max:1. prec in
                    let vx_step, bucket_min, bucket_max, datasets =
                        plot_distrib start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?methd ?status ?host ?url ?rt_min ?rt_max ?prec ?top_nth dbdir tblname in
                    let vx_min = (float_of_int bucket_min +. 0.5) *. vx_step
                    and nb_vx = bucket_max - bucket_min |> succ in
                    let fold f i =
                        List.fold_left (fun p (label, mi, d) ->
                            let get i =
                                let i = i - (mi - bucket_min) in
                                if i < 0 || i >= Array.length d then 0.
                                else float_of_int d.(i) in
                            f p label true get)
                            i datasets
                    and svg_width  = Prefs.get_float "gui/svg/width" 1000.
                    and svg_height = Prefs.get_float "gui/svg/height" 600. in
                    Chart.xy_plot ~svg_width ~svg_height ~stacked:Chart.StackedCentered
                                  ~vxmin_filter:"filter.minrt" ~vxmax_filter:"filter.maxrt"
                                  ~vxstep_filter:"filter.distr-prec"
                                  "response time (s)" "#queries"
                                  vx_min vx_step nb_vx
                                  { Chart.fold = fold }
                | None -> [] in
            View.make_chart "Web Response Times" filters_form disp_graph

    end

    module Dns =
    struct
        include Dns
        let dbdir = dbdir^"/dns"

        let top getter =
            let filters_form = form "DNS/top/show" (Forms.Dns.Top.to_edit "filter" getter) in
            View.make_filter "DNS Top Requests" filters_form

        let top_show getter =
            let filters_form = form "DNS/top" (Forms.Dns.Top.to_edit "filter" getter) in
            let disp_graph = match display_errs Forms.Dns.Top.from "filter" getter with
                | Some (start, (stop, (vlan, (mac_clt, (mac_srv, (client, (server, (rt_min, (rt_max, (error, (qname, (n, (sort_order, ()))))))))))))) ->
                    let start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop in
                    let rt_min = i2s ~min:0. rt_min in
                    let rt_max = i2s ?min:rt_min rt_max in
                    let n = BatOption.default 30 n
                    and sort_order = match sort_order with 0 -> Plot.Asc | _ -> Plot.Desc in
                    let tops = top_requests start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?rt_min ?rt_max ?error ?qname dbdir n sort_order in
                    let field_display_names =
                        [ "VLAN" ;
                          "Client MAC" ; "Client IP" ;
                          "Server MAC" ; "Server IP" ;
                          "Error Code" ; "Timestamp" ;
                          "Response Time (&#x00B5s)" ; "Query Name" ] in
                    View.tops_table tops field_display_names (fun (vlan, eclt, clt, esrv, srv, err, ts, (_, _, _, rt, _), name) ->
                        [ string_of_vlan vlan ;
                          EthAddr.to_string eclt ;
                          Cidr.to_string clt ;
                          EthAddr.to_string esrv ;
                          InetAddr.to_string srv ;
                          string_of_int err ;
                          Timestamp.to_string ts ;
                          string_of_float rt ;
                          name ])
                | None -> [] in
            View.make_chart "DNS Top Requests" filters_form disp_graph

        let resp_time getter =
            let filters_form = form "DNS/resptime" (Forms.Dns.RespTime.to_edit "filter" getter) in
            let disp_graph = match display_errs Forms.Dns.RespTime.from "filter" getter with
                | Some (start, (stop, (vlan, (mac_clt, (mac_srv, (client, (server, (tx_min, (rt_min, (rt_max, (time_step, (tblname, ())))))))))))) ->
                    let time_step = Interval.to_ms time_step
                    and tblname = Forms.Dns.TblNames.options.(tblname)
                    and start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop in
                    let rt_min = i2s ~min:0. rt_min in
                    let rt_max = i2s ?min:rt_min rt_max in
                    let datasets = plot_resp_time start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?rt_min ?rt_max ?tx_min time_step dbdir tblname in
                    (* TODO: plot_resp_time should return 0 in count instead of None... *)
                    let fold f i =
                        let i = f i "Min" true
                            (fun i -> Distribution.min datasets.(i)) in
                        let i = f i "Avg-&sigma;" true
                            (fun i -> max 0. Distribution.(avg datasets.(i) -. std_dev datasets.(i))) in
                        let i = f i "Avg" true
                            (fun i -> Distribution.avg datasets.(i)) in
                        let i = f i "Avg+&sigma;" true
                            (fun i -> Distribution.(avg datasets.(i) +. std_dev datasets.(i))) in
                        let i = f i "Max" true
                            (fun i -> Distribution.max datasets.(i)) in
                        let i = f i "#Transactions" false
                            (fun i -> Distribution.count datasets.(i) |> float_of_int) in
                        i in
                    let nb_vx = Array.length datasets in
                    let time_step = (Int64.to_float time_step) *. 0.001
                    and svg_width  = Prefs.get_float "gui/svg/width" 1000.
                    and svg_height = Prefs.get_float "gui/svg/height" 600. in
                    Chart.xy_plot ~svg_width ~svg_height
                                  ~string_of_x:string_of_date ~stacked:Chart.Stacked
                                  "time" "response time (s)"
                                  (Timestamp.to_unixfloat start) time_step nb_vx
                                  { Chart.fold = fold }
                | None -> [] in
            View.make_chart "DNS Response Time" filters_form disp_graph

        let distrib getter =
            let filters_form = form "DNS/distrib/show" (Forms.Dns.Distrib.to_edit "filter" getter) in
            View.make_filter "DNS Response Times" filters_form

        let distrib_show getter =
            let filters_form = form "DNS/distrib/show" (Forms.Dns.Distrib.to_edit "filter" getter) in
            let disp_graph = match display_errs Forms.Dns.Distrib.from "filter" getter with
                | Some (start, (stop, (vlan, (mac_clt, (mac_srv, (client, (server, (rt_min, (rt_max, (prec, (top_nth, (tblname, ())))))))))))) ->
                    let tblname = Forms.Dns.TblNames.options.(tblname)
                    and start  = My_time.to_timeval start
                    and stop   = My_time.to_timeval stop in
                    let rt_min = i2s ~min:0. rt_min in
                    let rt_max = i2s ?min:rt_min rt_max in
                    let prec   = i2s ~min:0.00001 ~max:1. prec in
                    let vx_step, bucket_min, bucket_max, datasets =
                        plot_distrib start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?rt_min ?rt_max ?prec ?top_nth dbdir tblname in
                    let vx_min = (float_of_int bucket_min +. 0.5) *. vx_step
                    and nb_vx = bucket_max - bucket_min |> succ in
                    let fold f i =
                        List.fold_left (fun p (label, mi, d) ->
                            let get i =
                                let i = i - (mi - bucket_min) in
                                if i < 0 || i >= Array.length d then 0.
                                else float_of_int d.(i) in
                            f p label true get)
                            i datasets
                    and svg_width  = Prefs.get_float "gui/svg/width" 1000.
                    and svg_height = Prefs.get_float "gui/svg/height" 600. in
                    Chart.xy_plot ~svg_width ~svg_height ~stacked:Chart.StackedCentered
                                  ~vxmin_filter:"filter.minrt" ~vxmax_filter:"filter.maxrt"
                                  ~vxstep_filter:"filter.distr-prec"
                                  "response time (s)" "#queries"
                                  vx_min vx_step nb_vx
                                  { Chart.fold = fold }
                | None -> [] in
            View.make_chart "DNS Response Times" filters_form disp_graph

    end

    module Admin =
    struct
        let preferences getter =
            (* fill getter from preferences *)
            let getter' name =
                if String.starts_with "prefs/" name then (
                    match Prefs.get_option (String.split ~by:"/" name |> snd) with
                    | Some v -> [v]
                    | None   -> getter name
                ) else getter name in
            let prefs_form = form "Admin/preferences/save" (Forms.Admin.Preferences.to_edit "prefs" getter') in
            [ h1 "DNS Response Times" ;
              div ~id:"preferences" [ prefs_form ] ]

        let save_preferences getter =
            (match display_errs Forms.Admin.Preferences.from "prefs" getter with
            | Some (svg_width, (svg_height, (resolve_ip, (resolve_mac, (ncores, ()))))) ->
                (* Save as much as possible in cookies as base64 string (not marshalled) *)
                let save_param n v to_string =
                    BatOption.may (fun v ->
                        Dispatch.add_cookie n (Base64.str_encode (to_string v))) v in
                save_param "resolver/ip" resolve_ip string_of_bool ;
                save_param "resolver/mac" resolve_mac string_of_bool ;
                save_param "gui/svg/width" svg_width string_of_float ;
                save_param "gui/svg/height" svg_height string_of_float ;
                save_param "db/#cores" ncores string_of_int ;
                View.add_msg "Saved"
            | None -> ()) ;
            preferences getter
    end

    module Report =
    struct
        type report_page = { page_no : int ;
                             title : string option ;
                             descr : string option ;
                             chart : string ;
                             getter : string -> string list }

        let report_page_prefix name page_no =
            "report/"^ name ^"/"^ string_of_int page_no

        let get_report_page name page_no =
            let pn n = report_page_prefix name page_no ^"/"^ n in
            { page_no ;
              title = Prefs.get_option (pn "title") ;
              descr = Prefs.get_option (pn "descr") ;
              chart = Prefs.get_option (pn "chart") |> BatOption.get ;
              getter = (fun n ->
                match Prefs.get_option n with
                | Some x -> [x]
                | None -> []) }
        
        let get_report name =
            Enum.unfold 0 (fun page_no ->
                Log.info "try to fetch page %d..." page_no ;
                try Some (get_report_page name page_no, succ page_no)
                with Invalid_argument _ -> None)

        let chart_of report_name page =
            let rn = report_page_prefix report_name page.page_no in
            match page.chart with
            | "Traffic/bandwidth" ->
                display_errs Forms.Traffic.Bandwidth.from rn page.getter |>
                Traffic.bandwidth_chart
            | "Traffic/peers" ->
                display_errs Forms.Traffic.Peers.from rn page.getter |>
                Traffic.peers_chart
            | x ->
                [ p ~cls:"err" [ cdata "No such chart " ;
                                 tag "i" [ cdata x ] ] ]

        let build report_name _args =
            get_report report_name /@
            (fun page ->
                let chart = chart_of report_name page in
                View.make_report_page page.page_no page.title page.descr chart) |>
            List.of_enum |> List.concat |>
            List.cons (h1 report_name)

        let get_page = function
            | ["info"] ->
                Info.run
            | [""] | ["main"] ->
                main
            | ["Traffic"; "bandwidth"] ->
                Traffic.bandwidth
            | ["Traffic"; "peers"] ->
                Traffic.peers
            | ["Traffic"; "graph"] ->
                Traffic.graph
            | ["Traffic"; "map"] ->
                Traffic.map
            | ["Traffic"; "map"; "show"] ->
                Traffic.map_show
            | ["Traffic"; "top"] ->
                Traffic.top
            | ["Traffic"; "top"; "show"] ->
                Traffic.top_show
            | ["Traffic"; "callflow"] ->
                Flow.callflow
            | ["Web"; "resptime"] ->
                Web.resp_time
            | ["Web"; "top"] ->
                Web.top
            | ["Web"; "top"; "show"] ->
                Web.top_show
            | ["Web"; "distrib"] ->
                Web.distrib
            | ["Web"; "distrib"; "show"] ->
                Web.distrib_show
            | ["DNS"; "resptime"] ->
                Dns.resp_time
            | ["DNS"; "top"] ->
                Dns.top
            | ["DNS"; "top"; "show"] ->
                Dns.top_show
            | ["DNS"; "distrib"] ->
                Dns.distrib
            | ["DNS"; "distrib"; "show"] ->
                Dns.distrib_show
            | ["Reports"; name] ->
                build name
            | ["Admin"; "preferences"] ->
                Admin.preferences
            | ["Admin"; "preferences"; "save"] ->
                Admin.save_preferences
            | _ ->
                Invalid.run
    end

end

let _ =
    (* Get preferences from the session *)
    let get_from_cookie name =
        try let s = List.assoc name !Dispatch.current_cookies in
            Some (decode_cookie s)
        with Not_found -> None in
    Prefs.set_overwrite_function get_from_cookie ;
    Dispatch.run (fun name getter -> Ctrl.Report.get_page name getter |> View.make_app_page)

