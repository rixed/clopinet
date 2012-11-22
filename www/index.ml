(* CGI script for visualizing mlrrd datas *)
open Batteries
module View = My_view
module Forms = My_forms
open Html
open Input.Ops
open Datatype

let dbdir = "../test.db"

module Ctrl =
struct
    include Ctrl

    (* returns the 'user-id' *)
    let auth name passwd =
        if name = "admin" && passwd = "secret" then Some 1 else None

    (* Main app page *)
    let main _args =
        let msg = "yo!" in
        View.make_app_page [cdata msg]

    let login args =
        let login = Forms.Login.from_args "login" args in
        let login_page () =
            View.make_app_page [ h1 "Hello! Who are you?" ;
                                 form "login" (Forms.Login.edit "login" login) ] in
        match login with
        | Value name, (Value passwd, ()) ->
            if auth name passwd <> None then (
                View.add_msg "Login OK" ;
                (* TODO: this should not be in dispatch! *)
                (* TODO: better auth with a nonce *)
                Dispatch.add_cookie "login" name ;
                Dispatch.add_cookie "password" passwd ;
                main args
            ) else (
                View.add_err "Login failure" ;
                login_page ()
            )
        | _ ->
            login_page ()

    let logout _args =
        Dispatch.add_cookie "login" "" ;
        Dispatch.add_cookie "password" "" ;
        View.make_app_page [ p [ cdata "You are now logged out" ] ]

    let ensure_logged runner args =
        match Hashtbl.find_option args "login",
              Hashtbl.find_option args "password" with
        | None, _ | _, None -> login args
        | Some name, Some passwd ->
            if auth name passwd <> None then (
                runner args
            ) else (
                View.add_err "Bad login" ;
                login args
            )

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

    module Web =
    struct
        include Web
        let dbdir = dbdir^"/web"
        let s2m x = x *. 1_000_000.
        let url_name host port url =
            host ^
            (if port = 80 then "" else (":" ^ string_of_int port)) ^
            url

        let top args =
            let filters = Forms.Web.Top.from_args "filter" args in
            let filters_form = form "Web/top" (Forms.Web.Top.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_clt, (Value mac_srv, (Value client, (Value server, (Value methd, (Value status, (Value host, (Value url, (Value rt_min, (Value rt_max, (Value n, (Value sort_order, ())))))))))))))) ->
                    let rt_min = BatOption.map s2m rt_min
                    and rt_max = BatOption.map s2m rt_max
                    and n = BatOption.default 30 n
                    and start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop
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
                        [ View.string_of_vlan vlan ;
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
                    let time_step = Interval.to_ms time_step
                    and start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop
                    and tblname = Forms.Web.TblNames.options.(tblname)
                    and rt_min = BatOption.map s2m rt_min
                    and rt_max = BatOption.map s2m rt_max in
                    let datasets = plot_resp_time start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?methd ?status ?host ?url ?rt_min ?rt_max time_step dbdir tblname in
                    View.resp_times_chart "Web - Average Response Time (sec)" time_step start datasets
                | _ -> [] in
            View.make_graph_page "Web Response Time" filters_form disp_graph

    end

    module Dns =
    struct
        include Dns
        let dbdir = dbdir^"/dns"
        let s2m x = x *. 1_000_000.

        let top args =
            let filters = Forms.Dns.Top.from_args "filter" args in
            let filters_form = form "DNS/top" (Forms.Dns.Top.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_clt, (Value mac_srv, (Value client, (Value server, (Value rt_min, (Value rt_max, (Value error, (Value qname, (Value n, (Value sort_order, ())))))))))))) ->
                    let rt_min = BatOption.map s2m rt_min
                    and rt_max = BatOption.map s2m rt_max
                    and start = My_time.to_timeval start
                    and stop  = My_time.to_timeval stop
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
                        [ View.string_of_vlan vlan ;
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
                    and rt_min = BatOption.map s2m rt_min
                    and rt_max = BatOption.map s2m rt_max in
                    let datasets = plot_resp_time start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?rt_min ?rt_max ?tx_min time_step dbdir tblname in
                    View.resp_times_chart "DNS - Average Response Time (sec)" time_step start datasets
                | _ -> [] in
            View.make_graph_page "DNS Response Time" filters_form disp_graph

    end

end

let _ =
    Dispatch.run (function
        | ["info"] -> Ctrl.Info.run
        | [""] | ["login"] ->
            Ctrl.login
        | ["main"] ->
            Ctrl.ensure_logged Ctrl.main
        | ["Admin"; "logout"] ->
            Ctrl.logout
        | ["Traffic"; "bandwidth"] ->
            Ctrl.ensure_logged Ctrl.Traffic.bandwidth
        | ["Traffic"; "peers"] ->
            Ctrl.ensure_logged Ctrl.Traffic.peers
        | ["Traffic"; "graph"] ->
            Ctrl.ensure_logged Ctrl.Traffic.graph
        | ["Traffic"; "tops"] ->
            Ctrl.ensure_logged Ctrl.Traffic.tops
        | ["Web"; "resptime"] ->
            Ctrl.ensure_logged Ctrl.Web.resp_time
        | ["Web"; "top"] ->
            Ctrl.ensure_logged Ctrl.Web.top
        | ["DNS"; "resptime"] ->
            Ctrl.ensure_logged Ctrl.Dns.resp_time
        | ["DNS"; "top"] ->
            Ctrl.ensure_logged Ctrl.Dns.top
        | _ -> Ctrl.Invalid.run)

