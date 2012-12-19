(* CGI script for visualizing mlrrd datas *)
open Batteries
module View = My_view
module Forms = My_forms
open Html
open Input.Ops
open Datatype

let dbdir = "../test.db"

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
    let main _args =
        let username = try Sys.getenv "REMOTE_USER"
                       with Not_found -> "you!" in
        let msg = "Hello "^username in
        View.make_app_page [cdata msg]

    let display_errs (from_args : 'a -> 'b -> 'c) (name : 'a) (args : 'b) : 'c option =
        try Some (from_args name args)
        with InputError str -> View.add_err str ; None
           | exc -> View.add_exc exc ; None

    (* DB search pages *)
    module Traffic =
    struct
        include Traffic
        let dbdir = dbdir^"/traffic"
        let bandwidth args =
            let filters_form = form "Traffic/bandwidth" (Forms.Traffic.Bandwidth.to_edit "filter" args) in
            let disp_graph = match display_errs Forms.Traffic.Bandwidth.from_args "filter" args with
                | Some (start, (stop, (vlan, (mac_src, (mac_dst, (eth_proto, (ip_src, (ip_dst, (ip, (ip_proto, (port, (time_step, (tblname, (what, (group_by, (max_graphs, ())))))))))))))))) ->
                    let time_step = Interval.to_ms time_step
                    and start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop
                    and tblname = Forms.Traffic.TblNames.options.(tblname)
                    and what = if what = 0 then Volume else PacketCount in
                    let datasets = match group_by with
                        | 1 (* src-mac *) | 2 (* dst-mac *) as sd ->
                            eth_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs (sd = 1) what time_step dbdir tblname
                        | 3 (* src-ip *) | 4 (* dst-ip *) as sd ->
                            ip_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs (sd = 3) what time_step dbdir tblname
                        | _ (* default, apps *) ->
                            app_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs what time_step dbdir tblname in
                    if datasets = [] then
                        []
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
                | None -> [] in
            View.make_graph_page "Bandwidth" filters_form disp_graph

        let peers args =
            let filters_form = form "Traffic/peers" (Forms.Traffic.Peers.to_edit "filter" args) in
            let disp_graph = match display_errs Forms.Traffic.Peers.from_args "filter" args with
                | Some (start, (stop, (vlan, (mac_src, (mac_dst, (eth_proto, (ip_src, (ip_dst, (ip, (ip_proto, (port, (tblname, (what, (group_by, (max_graphs, ()))))))))))))))) ->
                    let tblname = Forms.Traffic.TblNames.options.(tblname)
                    and start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop
                    and what = if what = 0 then Volume else PacketCount in
                    let datasets = match group_by with
                        | 0 (* mac *) ->
                            eth_plot_vol_tot ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs what dbdir tblname
                        | _ (* ip *) ->
                            ip_plot_vol_tot ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs what dbdir tblname in
                    if Hashtbl.is_empty datasets then
                        []
                    else
                        let is_bytes = what = Volume in
                        View.peers_chart ~is_bytes datasets @
                        [ tag "hr" [] ] @
                        View.peers_table ~is_bytes "src" "dst" datasets
                | None -> [] in
            View.make_graph_page "Peers" filters_form disp_graph

        let graph args =
            let filters_form = form "Traffic/graph" (Forms.Traffic.Graph.to_edit "filter" args) in
            let disp_graph = match display_errs Forms.Traffic.Graph.from_args "filter" args with
                | Some (start, (stop, (vlan, (eth_proto, (ip_proto, (port, (min_volume, (layout, (tblname, (group_by, ())))))))))) ->
                    let tblname = Forms.Traffic.TblNames.options.(tblname)
                    and start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop
                    and show_ip = group_by <> 2 and show_mac = group_by <> 1 in
                    let datasets = network_graph start stop ?min_volume ?vlan ?eth_proto ?ip_proto ?port show_mac show_ip dbdir tblname in
                    if Hashtbl.is_empty datasets then
                        []
                    else
                        View.peers_graph datasets Forms.Traffic.LayoutType.options.(layout)
                | None -> [] in
            View.make_graph_page "Network" filters_form disp_graph

        let tops args =
            let filters_form = form "Traffic/tops" (Forms.Traffic.Tops.to_edit "filter" args) in
            let disp_graph = match display_errs Forms.Traffic.Tops.from_args "filter" args with
                | Some (start, (stop, (vlan, (mac_src, (mac_dst, (eth_proto, (ip_src, (ip_dst, (ip, (ip_proto, (port, (tblname, (what, (group_by, (max_graphs, ()))))))))))))))) ->
                    let tblname = Forms.Traffic.TblNames.options.(tblname)
                    and start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop
                    and what = if what = 0 then Volume else PacketCount in
                    let key, datasets = match group_by with
                        | 1 (* src-mac *) ->
                            "src mac", eth_plot_vol_top ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs true what dbdir tblname
                        | 2 (* dst-mac *) ->
                            "dst mac", eth_plot_vol_top ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs false what dbdir tblname
                        | 3 (* mac (both) *) ->
                            "mac (both)", eth_plot_vol_top_both ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip ?ip_dst ?ip_proto ?port ?max_graphs what dbdir tblname
                        | 4 (* src-ip *) ->
                            "src IP", ip_plot_vol_top ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs true what dbdir tblname
                        | 5 (* dst-ip *) ->
                            "dst IP", ip_plot_vol_top ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs false what dbdir tblname
                        | 6 (* ip (both) *) ->
                            "IP (both)", ip_plot_vol_top_both ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs what dbdir tblname
                        | _ (* app *) ->
                            "Port", app_plot_vol_top ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs what dbdir tblname in

                    if Hashtbl.is_empty datasets then
                        []
                    else
                        let units = if what = PacketCount then "Packets" else "Bytes" in
                        View.top_chart key datasets units
                | None -> [] in
            View.make_graph_page "Tops" filters_form disp_graph

    end

    module Flow =
    struct
        include Flow
        let flow_dbdir = dbdir^"/flow"
        let callflow args =
            let filters_form = form "Traffic/callflow" (Forms.Flow.Callflow.to_edit "filter" args) in
            let disp_graph = match display_errs Forms.Flow.Callflow.from_args "filter" args with
                | Some (start, (stop, (vlan, (ip_start, (ip_dst, (ip_proto, (port_src, (port_dst, ())))))))) ->
                    let start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop in
                    let datasets =
                        get_callflow start stop ?vlan ip_start ?ip_dst ?ip_proto ?port_src ?port_dst
                                     ~dns_dbdir:(dbdir^"/dns") ~web_dbdir:(dbdir^"/web")
                                     ~tcp_dbdir:(dbdir^"/tcp") flow_dbdir in
                    View.callflow_chart (InetAddr.to_string ip_start) datasets
                | None -> [] in
            View.make_graph_page "Call Flow" filters_form disp_graph
    end

    module Web =
    struct
        include Web
        let dbdir = dbdir^"/web"
        let url_name host port url =
            host ^
            (if port = 80 then "" else (":" ^ string_of_int port)) ^
            url

        let top args =
            let filters_form = form "Web/top" (Forms.Web.Top.to_edit "filter" args) in
            let disp_graph = match display_errs Forms.Web.Top.from_args "filter" args with
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
            View.make_graph_page "Web Top Requests" filters_form disp_graph

        let resp_time args =
            let filters_form = form "Web/resptime" (Forms.Web.RespTime.to_edit "filter" args) in
            let disp_graph = match display_errs Forms.Web.RespTime.from_args "filter" args with
                | Some (start, (stop, (vlan, (mac_clt, (mac_srv, (client, (server, (methd, (status, (host, (url, (rt_min, (rt_max, (time_step, (tblname, ()))))))))))))))) ->
                    let time_step = Interval.to_ms time_step (* FIXME: plot_resp_time should take seconds instead *)
                    and start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop in
                    let rt_min = i2s ~min:0. rt_min in
                    let rt_max = i2s ?min:rt_min rt_max in
                    let tblname = Forms.Web.TblNames.options.(tblname) in
                    let datasets = plot_resp_time start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?methd ?status ?host ?url ?rt_min ?rt_max time_step dbdir tblname in
                    (* TODO: plot_resp_time should return 0 in count instead of None... *)
                    let datasets = Array.map (BatOption.default (0, 0.,0.,0.,0.)) datasets in
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
            View.make_graph_page "Web Response Time" filters_form disp_graph

        let distrib args =
            let filters_form = form "Web/distrib/show" (Forms.Web.Distrib.to_edit "filter" args) in
            View.make_filter_page "Web Response Times" filters_form

        let distrib_show args =
            let filters_form = form "Web/distrib/show" (Forms.Web.Distrib.to_edit "filter" args) in
            let disp_graph = match display_errs Forms.Web.Distrib.from_args "filter" args with
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
                    pre [ raw (Printf.sprintf "vx_min=%f, vx_step=%f, nb_vx=%d, bucket_min=%d\n" vx_min vx_step nb_vx bucket_min) ] ::
                    Chart.xy_plot ~svg_width ~svg_height ~stacked:Chart.StackedCentered
                                  ~vxmin_filter:"filter.minrt" ~vxmax_filter:"filter.maxrt"
                                  ~vxstep_filter:"filter.distr-prec"
                                  "response time (s)" "#queries"
                                  vx_min vx_step nb_vx
                                  { Chart.fold = fold }
                | None -> [] in
            View.make_graph_page "Web Response Times" filters_form disp_graph

    end

    module Dns =
    struct
        include Dns
        let dbdir = dbdir^"/dns"

        let top args =
            let filters_form = form "DNS/top" (Forms.Dns.Top.to_edit "filter" args) in
            let disp_graph = match display_errs Forms.Dns.Top.from_args "filter" args with
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
            View.make_graph_page "DNS Top Requests" filters_form disp_graph

        let resp_time args =
            let filters_form = form "DNS/resptime" (Forms.Dns.RespTime.to_edit "filter" args) in
            let disp_graph = match display_errs Forms.Dns.RespTime.from_args "filter" args with
                | Some (start, (stop, (vlan, (mac_clt, (mac_srv, (client, (server, (tx_min, (rt_min, (rt_max, (time_step, (tblname, ())))))))))))) ->
                    let time_step = Interval.to_ms time_step
                    and tblname = Forms.Dns.TblNames.options.(tblname)
                    and start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop in
                    let rt_min = i2s ~min:0. rt_min in
                    let rt_max = i2s ?min:rt_min rt_max in
                    let datasets = plot_resp_time start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?rt_min ?rt_max ?tx_min time_step dbdir tblname in
                    (* TODO: plot_resp_time should return 0 in count instead of None... *)
                    let datasets = Array.map (BatOption.default (0, 0.,0.,0.,0.)) datasets in
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
            View.make_graph_page "DNS Response Time" filters_form disp_graph

        let distrib args =
            let filters_form = form "DNS/distrib/show" (Forms.Dns.Distrib.to_edit "filter" args) in
            View.make_filter_page "DNS Response Times" filters_form

        let distrib_show args =
            let filters_form = form "DNS/distrib/show" (Forms.Dns.Distrib.to_edit "filter" args) in
            let disp_graph = match display_errs Forms.Dns.Distrib.from_args "filter" args with
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
                    pre [ raw (Printf.sprintf "vx_min=%f, vx_step=%f, nb_vx=%d, bucket_min=%d\n" vx_min vx_step nb_vx bucket_min) ] ::
                    Chart.xy_plot ~svg_width ~svg_height ~stacked:Chart.StackedCentered
                                  ~vxmin_filter:"filter.minrt" ~vxmax_filter:"filter.maxrt"
                                  ~vxstep_filter:"filter.distr-prec"
                                  "response time (s)" "#queries"
                                  vx_min vx_step nb_vx
                                  { Chart.fold = fold }
                | None -> [] in
            View.make_graph_page "DNS Response Times" filters_form disp_graph

    end

    module Admin =
    struct
        let preferences args =
            (* fill args from preferences *)
            let pref_to_args name =
                if not (Hashtbl.mem args name) then
                    Prefs.get_option name |>
                    BatOption.may (fun v ->
                        Hashtbl.add args ("prefs."^name) v) in
            List.iter pref_to_args [ "gui/svg/width" ; "gui/svg/height" ;
                                     "resolver/ip" ; "resolver/mac" ;
                                     "db/#cores" ] ;
            let prefs_form = form "Admin/preferences/save" (Forms.Admin.Preferences.to_edit "prefs" args) in
            View.make_app_page [
                h1 "DNS Response Times" ;
                div ~id:"preferences" [ prefs_form ]
            ]

        let save_preferences args =
            (match display_errs Forms.Admin.Preferences.from_args "prefs" args with
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
            preferences args
    end
end

let _ =
    (* Get preferences from the session *)
    let get_from_cookie name =
        try let s = List.assoc name !Dispatch.current_cookies in
            Some (decode_cookie s)
        with Not_found -> None in
    Prefs.set_overwrite_function get_from_cookie ;
    Dispatch.run (function
        | ["info"] -> Ctrl.Info.run
        | [""] | ["main"] ->
            Ctrl.main
        | ["Traffic"; "bandwidth"] ->
            Ctrl.Traffic.bandwidth
        | ["Traffic"; "peers"] ->
            Ctrl.Traffic.peers
        | ["Traffic"; "graph"] ->
            Ctrl.Traffic.graph
        | ["Traffic"; "tops"] ->
            Ctrl.Traffic.tops
        | ["Traffic"; "callflow"] ->
            Ctrl.Flow.callflow
        | ["Web"; "resptime"] ->
            Ctrl.Web.resp_time
        | ["Web"; "top"] ->
            Ctrl.Web.top
        | ["Web"; "distrib"] ->
            Ctrl.Web.distrib
        | ["Web"; "distrib"; "show"] ->
            Ctrl.Web.distrib_show
        | ["DNS"; "resptime"] ->
            Ctrl.Dns.resp_time
        | ["DNS"; "top"] ->
            Ctrl.Dns.top
        | ["DNS"; "distrib"] ->
            Ctrl.Dns.distrib
        | ["DNS"; "distrib"; "show"] ->
            Ctrl.Dns.distrib_show
        | ["Admin"; "preferences"] ->
            Ctrl.Admin.preferences
        | ["Admin"; "preferences"; "save"] ->
            Ctrl.Admin.save_preferences
        | _ -> Ctrl.Invalid.run)

