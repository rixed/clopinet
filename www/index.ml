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

    (* DB search pages *)
    module Traffic =
    struct
        include Traffic
        let dbdir = dbdir^"/traffic"
        let bandwidth args =
            let filters = Forms.Traffic.Bandwidth.from_args "filter" args in
            let filters_form = form "Traffic/bandwidth" (Forms.Traffic.Bandwidth.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_src, (Value mac_dst, (Value eth_proto, (Value ip_src, (Value ip_dst, (Value ip, (Value ip_proto, (Value port, (Value time_step, (Value tblname, (Value what, (Value group_by, (Value max_graphs, ()))))))))))))))) ->
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
                        View.bandwidth_chart ("Traffic - "^what^"/sec") time_step start datasets
                | _ -> [] in
            View.make_graph_page "Bandwidth" filters_form disp_graph

        let peers args =
            let filters = Forms.Traffic.Peers.from_args "filter" args in
            let filters_form = form "Traffic/peers" (Forms.Traffic.Peers.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_src, (Value mac_dst, (Value eth_proto, (Value ip_src, (Value ip_dst, (Value ip, (Value ip_proto, (Value port, (Value tblname, (Value what, (Value group_by, (Value max_graphs, ())))))))))))))) ->
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
                | _ -> [] in
            View.make_graph_page "Peers" filters_form disp_graph

        let graph args =
            let filters = Forms.Traffic.Graph.from_args "filter" args in
            let filters_form = form "Traffic/graph" (Forms.Traffic.Graph.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value eth_proto, (Value ip_proto, (Value port, (Value min_volume, (Value layout, (Value tblname, (Value group_by, ()))))))))) ->
                    let tblname = Forms.Traffic.TblNames.options.(tblname)
                    and start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop
                    and show_ip = group_by <> 2 and show_mac = group_by <> 1 in
                    let datasets = network_graph start stop ?min_volume ?vlan ?eth_proto ?ip_proto ?port show_mac show_ip dbdir tblname in
                    if Hashtbl.is_empty datasets then
                        []
                    else
                        View.peers_graph datasets Forms.Traffic.LayoutType.options.(layout)
                | _ -> [] in
            View.make_graph_page "Network" filters_form disp_graph

        let tops args =
            let filters = Forms.Traffic.Tops.from_args "filter" args in
            let filters_form = form "Traffic/tops" (Forms.Traffic.Tops.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_src, (Value mac_dst, (Value eth_proto, (Value ip_src, (Value ip_dst, (Value ip, (Value ip_proto, (Value port, (Value tblname, (Value what, (Value group_by, (Value max_graphs, ())))))))))))))) ->
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
                | _ -> [] in
            View.make_graph_page "Tops" filters_form disp_graph

    end

    module Flow =
    struct
        include Flow
        let flow_dbdir = dbdir^"/flow"
        let callflow args =
            let filters = Forms.Flow.Callflow.from_args "filter" args in
            let filters_form = form "Traffic/callflow" (Forms.Flow.Callflow.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value ip_start, (Value ip_dst, (Value ip_proto, (Value port_src, (Value port_dst, ()))))))) ->
                    let start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop in
                    let datasets =
                        get_callflow start stop ?vlan ip_start ?ip_dst ?ip_proto ?port_src ?port_dst
                                     ~dns_dbdir:(dbdir^"/dns") ~web_dbdir:(dbdir^"/web")
                                     ~tcp_dbdir:(dbdir^"/tcp") flow_dbdir in
                    View.callflow_chart (InetAddr.to_string ip_start) datasets
                | _ -> [] in
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
            let filters = Forms.Web.Top.from_args "filter" args in
            let filters_form = form "Web/top" (Forms.Web.Top.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_clt, (Value mac_srv, (Value client, (Value server, (Value methd, (Value status, (Value host, (Value url, (Value rt_min, (Value rt_max, (Value n, (Value sort_order, ())))))))))))))) ->
                    let n = BatOption.default 30 n
                    and start  = My_time.to_timeval start
                    and stop   = My_time.to_timeval stop
                    and rt_min = i2s rt_min
                    and rt_max = i2s rt_max
                    and sort_order = match sort_order with 0 -> Plot.Asc | _ -> Plot.Desc in
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
                | _ -> [] in
            View.make_graph_page "Web Top Requests" filters_form disp_graph

        let resp_time args =
            let filters = Forms.Web.RespTime.from_args "filter" args in
            let filters_form = form "Web/resptime" (Forms.Web.RespTime.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_clt, (Value mac_srv, (Value client, (Value server, (Value methd, (Value status, (Value host, (Value url, (Value rt_min, (Value rt_max, (Value time_step, (Value tblname, ())))))))))))))) ->
                    let time_step = Interval.to_ms time_step (* FIXME: plot_resp_time should take seconds instead *)
                    and start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop
                    and rt_min = i2s rt_min
                    and rt_max = i2s rt_max
                    and tblname = Forms.Web.TblNames.options.(tblname) in
                    let datasets = plot_resp_time start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?methd ?status ?host ?url ?rt_min ?rt_max time_step dbdir tblname in
                    View.resp_times_chart "Web - Average Response Time (sec)" time_step start datasets
                | _ -> [] in
            View.make_graph_page "Web Response Time" filters_form disp_graph

        let distrib args =
            let filters = Forms.Web.Distrib.from_args "filter" args in
            let filters_form = form "Web/distrib" (Forms.Web.Distrib.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_clt, (Value mac_srv, (Value client, (Value server, (Value methd, (Value status, (Value host, (Value url, (Value rt_min, (Value rt_max, (Value prec, (Value top_nth, (Value tblname, ()))))))))))))))) ->
                    let tblname = Forms.Dns.TblNames.options.(tblname)
                    and start  = My_time.to_timeval start
                    and stop   = My_time.to_timeval stop
                    and rt_min = i2s rt_min
                    and rt_max = i2s rt_max
                    and prec   = i2s ~min:0.000001 ~max:1. prec in
                    let datasets = plot_distrib start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?methd ?status ?host ?url ?rt_min ?rt_max ?prec ?top_nth dbdir tblname in
                    View.distrib_chart "response time (s)" "#queries" datasets
                | _ -> [] in
            View.make_graph_page "Web Response Times" filters_form disp_graph

    end

    module Dns =
    struct
        include Dns
        let dbdir = dbdir^"/dns"

        let top args =
            let filters = Forms.Dns.Top.from_args "filter" args in
            let filters_form = form "DNS/top" (Forms.Dns.Top.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_clt, (Value mac_srv, (Value client, (Value server, (Value rt_min, (Value rt_max, (Value error, (Value qname, (Value n, (Value sort_order, ())))))))))))) ->
                    let start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop
                    and rt_min = i2s rt_min
                    and rt_max = i2s rt_max
                    and n = BatOption.default 30 n
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
                | _ -> [] in
            View.make_graph_page "DNS Top Requests" filters_form disp_graph

        let resp_time args =
            let filters = Forms.Dns.RespTime.from_args "filter" args in
            let filters_form = form "DNS/resptime" (Forms.Dns.RespTime.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_clt, (Value mac_srv, (Value client, (Value server, (Value tx_min, (Value rt_min, (Value rt_max, (Value time_step, (Value tblname, ()))))))))))) ->
                    let time_step = Interval.to_ms time_step
                    and tblname = Forms.Dns.TblNames.options.(tblname)
                    and start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop
                    and rt_min = i2s rt_min
                    and rt_max = i2s rt_max in
                    let datasets = plot_resp_time start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?rt_min ?rt_max ?tx_min time_step dbdir tblname in
                    View.resp_times_chart "DNS - Average Response Time (sec)" time_step start datasets
                | _ -> [] in
            View.make_graph_page "DNS Response Time" filters_form disp_graph

        let distrib args =
            let filters = Forms.Dns.Distrib.from_args "filter" args in
            let filters_form = form "DNS/distrib" (Forms.Dns.Distrib.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_clt, (Value mac_srv, (Value client, (Value server, (Value rt_min, (Value rt_max, (Value prec, (Value top_nth, (Value tblname, ()))))))))))) ->
                    let tblname = Forms.Dns.TblNames.options.(tblname)
                    and start  = My_time.to_timeval start
                    and stop   = My_time.to_timeval stop
                    and rt_min = i2s rt_min
                    and rt_max = i2s rt_max
                    and prec   = i2s ~min:0.000001 ~max:1. prec in
                    let datasets = plot_distrib start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?rt_min ?rt_max ?prec ?top_nth dbdir tblname in
                    View.distrib_chart "response time (s)" "#queries" datasets
                | _ -> [] in
            View.make_graph_page "DNS Response Times" filters_form disp_graph

    end

    module Admin =
    struct
        let preferences args =
            let prefs = Forms.Admin.Preferences.from_args "prefs" args in
            let prefs_form = form "Admin/preferences/save" (Forms.Admin.Preferences.edit "prefs" prefs) in
            View.make_app_page [
                h1 "DNS Response Times" ;
                div ~id:"preferences" [ prefs_form ]
            ]
        let save_preferences args =
            (match Forms.Admin.Preferences.from_args "prefs" args with
            | Value svg_width, (Value svg_height, (Value resolve_ip, (Value resolve_mac, (Value ncores, ())))) ->
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
            | _ ->
                View.add_err "Cannot Save") ;
            preferences args
    end
end

let _ =
    let get_marshalled_from_cookie name =
        try let s = List.assoc name !Dispatch.current_cookies in
            Some (Base64.str_decode s)
        with Not_found -> None in
    Prefs.set_overwrite_function (fun name ->
        let unparser = match name with
        | "gui/svg/width"
        | "gui/svg/height" -> (fun s -> BatOption.map string_of_float (Marshal.from_string s 0))
        | "resolver/ip"
        | "resolver/mac"   -> (fun s -> BatOption.map string_of_bool (Marshal.from_string s 0))
        | "db/#cores"      -> (fun s -> BatOption.map string_of_int (Marshal.from_string s 0))
        | _                -> (fun s -> Marshal.from_string s 0) in
        BatOption.bind
            (get_marshalled_from_cookie name)
            unparser) ;
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
        | ["DNS"; "resptime"] ->
            Ctrl.Dns.resp_time
        | ["DNS"; "top"] ->
            Ctrl.Dns.top
        | ["DNS"; "distrib"] ->
            Ctrl.Dns.distrib
        | ["Admin"; "preferences"] ->
            Ctrl.Admin.preferences
        | ["Admin"; "preferences"; "save"] ->
            Ctrl.Admin.save_preferences
        | _ -> Ctrl.Invalid.run)

