open Batteries
module View = My_view
module Forms = My_forms

open Html
open Input.Ops
open Datatype

include Ctrl

let dbdir = Prefs.get_string "CPN_DB_BASE_DIR" "./test.db"

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
 * function in another axis... for instance imagine we have dates for both X and Y axis... *)
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
    with InputError str -> View.add_err [cdata str] ; None
       | exc -> View.add_exc exc ; None

module ChartDescr =
struct
    (* A chart unique id is "category/title" *)
    type t = { category : string ; (* for menus *)
               title : string ; (* submenu entry *)
               help : html ;
               to_html : string -> (string -> string list) -> html ; (* rendering the chart *)
               filter : string -> string -> (string -> string list) -> html_chunk (* FIXME: html! *) (* rendering the form *) }

let filtered_chart_show chart_descr getter =
    let namespace = "filter" in
    let target = chart_descr.category ^"/"^ chart_descr.title ^"/show" in
    let chart = chart_descr.to_html namespace getter in
    let form = chart_descr.filter target namespace getter in
    View.make_chart chart_descr.title form chart

let filtered_chart chart_descr getter =
    let target = chart_descr.category ^"/"^ chart_descr.title ^"/show" in
    let form = chart_descr.filter target "filter" getter in
    View.make_filter chart_descr.title chart_descr.help form

end

let top_help name =
    [ p [ cdata @@ "This is the 'free form' query into the "^ name ^" database. You can ask \
                 for any fields, grouped by any fields with whatever aggregate function \
                 you like, sorted by any field, filtered by any expression, and get back \
                 the top " ; em "N" ; cdata " tuples." ] ;
      p [ cdata "This is useful when you look for something in particular and none of the \
                 other views fit your need." ] ;
      p [ cdata "Notice the " ; em "single" ; cdata " or " ; em "double" ;
          cdata " pass radiobutton; single pass is approximately twice faster but will \
                 report an approximate result." ] ]

let queries_help =
    [ p [ cdata "This lists individual queries, sorted by response times, limited to the " ; em "N" ;
          cdata " fastest/slowest." ] ;
      p [ cdata "As usual, you can filter queries my any filter to limit the scope to what's important to you." ] ]

let resptime_help name =
    [ p [ cdata @@ "This chart displays the response time of "^ name ^" queries with regard to time." ] ;
      p [ cdata "The average response time is surrounded by the min, max and standard deviation." ] ;
      p [ cdata "The thin line above this graph represents the number of queries, which scale \
                 is drawn on the right Y axis." ] ]

let distrib_help name =
    [ p [ cdata @@ "This chart displays a distribution of the "^ name ^" request times. It gives the \
                 best available representation of the performance of name resolution for your \
                 users. Different servers are shown in different colors." ] ;
      p [ cdata "Basically, the X coordinate gives a response time and the height of the plot \
                 at that location represents the number of queries which were answered that fast. \
                 You will probably want to zoom in to have a closer view of the clusters." ] ]

let timestep_of_timestamps start stop =
    let dt = Timestamp.to_unixfloat stop -. Timestamp.to_unixfloat start in
    Interval.of_secs (dt /. Float.of_pref "CPN_GUI_CHART_PREFERED_RESOLUTION" 200.)

let default_chart_duration = Interval.of_pref "CPN_GUI_CHART_PREFERED_DURATION" Interval.({ zero with days = 1. })

let tblname_of_timestep dt metric tbls =
    let nb_noblur = ref 0 in (* it's ok to have one table with no bluring, but more than that and the user got a warning *)
    (* choose the table which round parameter is greater yet below dt *)
    let pname tbl = "CPN_DB_"^ metric ^"_"^ tbl ^"_ROUND" |> String.uppercase in
    match Array.fold_left (fun ma tbl ->
        match Interval.of_pref_option (pname tbl) with
        | None ->
            incr nb_noblur ;
            if !nb_noblur > 1 then Log.warning "No time bluring information for table %s (%s unset)" tbl (pname tbl) ;
            if ma = None then Some (Interval.({zero with msecs = 0.1}), tbl) else ma
        | Some round ->
            if Interval.compare round dt <= 0 then match ma with
                | Some (m,_) when Interval.compare round m <= 0 ->
                    ma
                | _ -> Some (round, tbl)
            else ma) None tbls with
    | Some (_, tbl) -> tbl
    | None -> tbls.(0)

(* DB search pages *)
module Traffic =
struct
    include Traffic
    let dbdir = dbdir^"/traffic"

    let bandwidth_chart = function
        | None ->
            [ p ~cls:"err" [ cdata "No selection" ] ]
        | Some (start, (stop, (vlan, (mac_src, (mac_dst, (eth_proto, (ip_src, (ip_dst, (ip, (ip_proto, (port, (usr_filter, (time_step, (tblname, (what, (group_by, (max_graphs, ()))))))))))))))))) ->
            let stop  = match stop with Some t -> My_time.to_timeval t | None -> Timestamp.now () in
            let start = match start with Some t -> My_time.to_timeval t | None -> Timestamp.sub_interval stop default_chart_duration in
            let time_step = match time_step with Some t -> t | None -> timestep_of_timestamps start stop in
            let tblname = match tblname with Some n -> Forms.Traffic.TblNames.options.(n) | None -> tblname_of_timestep time_step "traffic" Forms.Traffic.TblNames.options in
            let time_step = Interval.to_ms time_step in
            let what = if what = 0 then Volume else PacketCount in
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
                and svg_width  = Float.of_pref "CPN_GUI_SVG_WIDTH" 1000.
                and svg_height = Float.of_pref "CPN_GUI_SVG_HEIGHT" 600. in
                let fold f init =
                    List.fold_left (fun prev (l, a) ->
                        let get i = Array.get a i |> float_of_int in
                        f prev l true get)
                        init datasets in
                Chart.xy_plot ~svg_width ~svg_height ~stacked:Chart.Stacked
                              ~force_show_0:true ~string_of_x:string_of_date ~show_rate:true
                              "time" ~x_label_for_rate:"secs" what
                              (Timestamp.to_unixfloat start) time_step nb_vx
                              { Chart.fold = fold }

    let bw_chart_descr =
        { ChartDescr.category = "Traffic" ;
          ChartDescr.title = "Bandwidth Evolution" ;
          ChartDescr.help = [ p [ cdata "The first chart to check is usually the bandwidth chart. \
                                         It displays the volume of traffic (either bytes or packets count) \
                                         with regard to time." ] ;
                              p [ cdata "It is possible to group volume by addresses or ports." ] ] ;
          ChartDescr.to_html = (fun name getter ->
                      display_errs Forms.Traffic.Bandwidth.from name getter |> bandwidth_chart) ;
          ChartDescr.filter = (fun target name getter ->
                      form target (Forms.Traffic.Bandwidth.to_edit name getter)) }


    let peers_chart = function
        | Some (start, (stop, (vlan, (mac_src, (mac_dst, (eth_proto, (ip_src, (ip_dst, (ip, (ip_proto, (port, (usr_filter, (tblname, (what, (group_by, (max_graphs, ())))))))))))))))) ->
            let stop  = match stop with Some t -> My_time.to_timeval t | None -> Timestamp.now () in
            let start = match start with Some t -> My_time.to_timeval t | None -> Timestamp.sub_interval stop default_chart_duration in
            let tblname = match tblname with Some n -> Forms.Traffic.TblNames.options.(n) | None -> tblname_of_timestep (Timestamp.sub_to_interval stop start) "traffic" Forms.Traffic.TblNames.options in
            let what = if what = 0 then Volume else PacketCount in
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

    let peer_chart_descr =
        { ChartDescr.category = "Traffic" ;
          ChartDescr.title = "Peers" ;
          ChartDescr.help = [ p [ cdata "This chart displays which network peers exchanged the most traffic." ;
                                  cdata "This may be used to learn what hosts are the more verbose or to spot \
                                         possibly compromised hosts that connect to many others." ] ] ;
          ChartDescr.to_html = (fun name getter ->
                      display_errs Forms.Traffic.Peers.from name getter |> peers_chart) ;
          ChartDescr.filter = (fun target name getter ->
                      form target (Forms.Traffic.Peers.to_edit name getter)) }

    let graph_chart = function
        | Some (start, (stop, (vlan, (eth_proto, (ip_proto, (port, (min_volume, (usr_filter, (layout, (tblname, (group_by, ()))))))))))) ->
            let stop  = match stop with Some t -> My_time.to_timeval t | None -> Timestamp.now () in
            let start = match start with Some t -> My_time.to_timeval t | None -> Timestamp.sub_interval stop default_chart_duration in
            let tblname = match tblname with Some n -> Forms.Traffic.TblNames.options.(n) | None -> tblname_of_timestep (Timestamp.sub_to_interval stop start) "traffic" Forms.Traffic.TblNames.options in
            let show_ip = group_by <> 2 and show_mac = group_by <> 1 in
            let datasets = network_graph start stop ?min_volume ?vlan ?eth_proto ?ip_proto ?port ?usr_filter show_mac show_ip dbdir tblname in
            if Hashtbl.is_empty datasets then
                []
            else
                View.peers_graph datasets Forms.Traffic.LayoutType.options.(layout)
        | None -> []

    let graph_chart_descr =
        { ChartDescr.category = "Traffic" ;
          ChartDescr.title = "Network Graph" ;
          ChartDescr.help = [ p [ cdata "This chart displays a zoomable graph of " ; em "all" ; cdata " peers \
                                         (above a given threshold of volume) as layered by graphviz." ;
                                  cdata "This comes handy to learn what clusters your hosts are in, \
                                         and can even be used to spot subnets of bots (via the pattern these \
                                         bots use to synchronize)." ] ] ;
          ChartDescr.to_html = (fun name getter ->
                      display_errs Forms.Traffic.Graph.from name getter |> graph_chart) ;
          ChartDescr.filter = (fun target name getter ->
                      form target (Forms.Traffic.Graph.to_edit name getter)) }

    let top_chart = function
        | Some (start, (stop, (_vlan, (_mac_src, (_mac_dst, (_eth_proto, (ip_src, (_ip_dst, (_ip, (_ip_proto, (_port, (usr_filter, (tblname, (group_by, (aggr_fields, (sort_by, (max_graphs, (single_pass, ())))))))))))))))))) ->
            let stop  = match stop with Some t -> My_time.to_timeval t | None -> Timestamp.now () in
            let start = match start with Some t -> My_time.to_timeval t | None -> Timestamp.sub_interval stop default_chart_duration in
            let tblname = match tblname with Some n -> Forms.Traffic.TblNames.options.(n) | None -> tblname_of_timestep (Timestamp.sub_to_interval stop start) "traffic" Forms.Traffic.TblNames.options in
            let datasets = get_top ~start ~stop ?ip_src ?usr_filter ?max_graphs ?single_pass sort_by group_by aggr_fields dbdir tblname in
            View.table_of_datasets group_by aggr_fields sort_by datasets
        | None -> []

    let top_chart_descr =
        { ChartDescr.category = "Traffic" ;
          ChartDescr.title = "Free Search" ;
          ChartDescr.help = top_help "traffic" ;
          ChartDescr.to_html = (fun name getter ->
                      display_errs Forms.Traffic.Tops.from name getter |> top_chart) ;
          ChartDescr.filter = (fun target name getter ->
                      form target (Forms.Traffic.Tops.to_edit name getter)) }

    let map_chart = function
        | Some (start, (stop, (vlan, (eth_proto, (ip_proto, (port, (min_volume, (usr_filter, (tblname, ()))))))))) ->
            let stop  = match stop with Some t -> My_time.to_timeval t | None -> Timestamp.now () in
            let start = match start with Some t -> My_time.to_timeval t | None -> Timestamp.sub_interval stop default_chart_duration in
            let tblname = match tblname with Some n -> Forms.Traffic.TblNames.options.(n) | None -> tblname_of_timestep (Timestamp.sub_to_interval stop start) "traffic" Forms.Traffic.TblNames.options in
            let datasets = network_map start stop ?min_volume ?vlan ?eth_proto ?ip_proto ?port ?usr_filter dbdir tblname in
            if Hashtbl.is_empty datasets then []
            else
                View.peers_map datasets
        | None -> []

    let map_chart_descr =
        { ChartDescr.category = "Traffic" ;
          ChartDescr.title = "World Map" ;
          ChartDescr.help = [ p [ cdata "This chart maps your traffic on a world map, according to IP geolocalisation \
                                         provided by the (free version of) the geoip database." ] ;
                              p [ cdata "May be useful to check where your traffic comes from or what sites use the \
                                         most bandwidth, and always nice to look at." ] ] ;
          ChartDescr.to_html = (fun name getter ->
                      display_errs Forms.Traffic.Map.from name getter |> map_chart) ;
          ChartDescr.filter = (fun target name getter ->
                      form target (Forms.Traffic.Map.to_edit name getter)) }

end

module Flow =
struct
    include Flow
    let flow_dbdir = dbdir^"/flow"

    let callflow_chart = function
        | Some (start, (stop, (vlan, (ip_start, (ip_dst, (ip_proto, (port_src, (port_dst, ())))))))) ->
            let stop  = match stop with Some t -> My_time.to_timeval t | None -> Timestamp.now () in
            let start = match start with Some t -> My_time.to_timeval t | None -> Timestamp.sub_interval stop default_chart_duration in
            let datasets =
                get_callflow start stop ?vlan ip_start ?ip_dst ?ip_proto ?port_src ?port_dst
                             ~dns_dbdir:(dbdir^"/dns") ~web_dbdir:(dbdir^"/web")
                             ~tcp_dbdir:(dbdir^"/tcp") flow_dbdir in
            View.callflow_chart (InetAddr.to_string ip_start) datasets
        | None -> []

    let callflow_chart_descr =
        { ChartDescr.category = "Traffic" ;
          ChartDescr.title = "Call Flow" ;
          ChartDescr.help = [ p [ cdata "This chart is the microscope allowing to view graphically the complete \
                                         set of informations stored for a given range of time, from a given IP \
                                         address. All traffic from this IP sent during this time range will be \
                                         displayed as well as all other communicating hosts related to this \
                                         traffic, as a call flow with time increasing from top to bottom." ] ;
                              p [ cdata "As a busy host can send and receive many things per seconds, it \
                                         is advised to start with a very short time range." ] ] ;
          ChartDescr.to_html = (fun name getter ->
                      display_errs Forms.Flow.Callflow.from name getter |> callflow_chart) ;
          ChartDescr.filter = (fun target name getter ->
                      form target (Forms.Flow.Callflow.to_edit name getter)) }

end

module Web =
struct
    include Web
    let dbdir = dbdir^"/web"

    let url_name host port url =
        host ^
        (if port = 80 then "" else (":" ^ string_of_int port)) ^
        url

    let queries_chart = function
        | Some (start, (stop, (vlan, (mac_clt, (mac_srv, (ip_clt, (ip_srv, (methd, (status, (host, (url, (rt_min, (rt_max, (n, (sort_order, ()))))))))))))))) ->
            let n = BatOption.default 30 n
            and stop   = match stop with Some t -> My_time.to_timeval t | None -> Timestamp.now () in
            let start = match start with Some t -> My_time.to_timeval t | None -> Timestamp.sub_interval stop default_chart_duration in
            let rt_min = i2s ~min:0. rt_min in
            let rt_max = i2s ?min:rt_min rt_max in
            let sort_order = match sort_order with 0 -> Plot.Asc | _ -> Plot.Desc in
            let tops = top_requests start stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?methd ?status ?host ?url ?rt_min ?rt_max dbdir n sort_order in
            let field_display_names =
                [ "Origin"     ; "VLAN" ;
                  "Client MAC" ; "Client IP" ;
                  "Server MAC" ; "Server IP" ;
                  "Method"     ; "Error Code" ;
                  "Timestamp" ;
                  "Response Time (&#x00B5s)" ;
                  "URL" ] in
            View.tops_table tops field_display_names (fun (orig, vlan, eclt, clt, esrv, srv, port, meth, err, ts, (_, _, _, rt, _), host, url) ->
                [ Origin.to_string orig ;
                  string_of_vlan vlan ;
                  EthAddr.to_string eclt ;
                  Cidr.to_string clt ;
                  EthAddr.to_string esrv ;
                  InetAddr.to_string srv ;
                  string_of_method meth ;
                  string_of_int err ;
                  Timestamp.to_string ts ;
                  string_of_float rt ;
                  url_name host port url ])
        | None -> []

    let queries_chart_descr =
        { ChartDescr.category = "Web" ;
          ChartDescr.title = "Queries" ;
          ChartDescr.help = queries_help ;
          ChartDescr.to_html = (fun name getter ->
                      display_errs Forms.Web.Queries.from name getter |> queries_chart) ;
          ChartDescr.filter = (fun target name getter ->
                      form target (Forms.Web.Queries.to_edit name getter)) }

    let srt_chart = function
        | Some (start, (stop, (vlan, (mac_clt, (mac_srv, (ip_clt, (ip_srv, (methd, (status, (host, (url, (rt_min, (rt_max, (time_step, (tblname, ()))))))))))))))) ->
            let stop  = match stop with Some t -> My_time.to_timeval t | None -> Timestamp.now () in
            let start = match start with Some t -> My_time.to_timeval t | None -> Timestamp.sub_interval stop default_chart_duration in
            let time_step = match time_step with Some t -> t | None -> timestep_of_timestamps start stop in
            let tblname = match tblname with Some n -> Forms.Web.TblNames.options.(n) | None -> tblname_of_timestep time_step "web" Forms.Web.TblNames.options in
            let time_step = Interval.to_ms time_step in
            let rt_min = i2s ~min:0. rt_min in
            let rt_max = i2s ?min:rt_min rt_max in
            let datasets = plot_resp_time start stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?methd ?status ?host ?url ?rt_min ?rt_max time_step dbdir tblname in
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
                let i = f i "Transactions" false
                    (fun i -> Distribution.count datasets.(i) |> float_of_int) in
                i in
            let nb_vx = Array.length datasets in
            let time_step = (Int64.to_float time_step) *. 0.001
            and svg_width  = Float.of_pref "CPN_GUI_SVG_WIDTH" 1000.
            and svg_height = Float.of_pref "CPN_GUI_SVG_HEIGHT" 600. in
            Chart.xy_plot ~svg_width ~svg_height
                          ~string_of_x:string_of_date ~stacked:Chart.Stacked
                          "time" "secs"
                          (Timestamp.to_unixfloat start) time_step nb_vx
                          { Chart.fold = fold }
        | None -> []

    let srt_chart_descr =
        { ChartDescr.category = "Web" ;
          ChartDescr.title = "Response Time Evolution" ;
          ChartDescr.help = resptime_help "HTTP" ;
          ChartDescr.to_html = (fun name getter ->
                      display_errs Forms.Web.RespTime.from name getter |> srt_chart) ;
          ChartDescr.filter = (fun target name getter ->
                      form target (Forms.Web.RespTime.to_edit name getter)) }

    let distrib_chart = function
        | Some (start, (stop, (vlan, (mac_clt, (mac_srv, (ip_clt, (ip_srv, (methd, (status, (host, (url, (rt_min, (rt_max, (prec, (top_nth, (tblname, ())))))))))))))))) ->
            let stop   = match stop with Some t -> My_time.to_timeval t | None -> Timestamp.now () in
            let start = match start with Some t -> My_time.to_timeval t | None -> Timestamp.sub_interval stop default_chart_duration in
            let tblname = match tblname with Some n -> Forms.Web.TblNames.options.(n) | None -> tblname_of_timestep (Timestamp.sub_to_interval stop start) "web" Forms.Web.TblNames.options in
            let rt_min = i2s ~min:0. rt_min in
            let rt_max = i2s ?min:rt_min rt_max in
            let prec   = i2s ~min:0.00001 ~max:1. prec in
            let vx_step, bucket_min, bucket_max, datasets =
                plot_distrib start stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?methd ?status ?host ?url ?rt_min ?rt_max ?prec ?top_nth dbdir tblname in
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
            and svg_width  = Float.of_pref "CPN_GUI_SVG_WIDTH" 1000.
            and svg_height = Float.of_pref "CPN_GUI_SVG_HEIGHT" 600. in
            Chart.xy_plot ~svg_width ~svg_height ~stacked:Chart.StackedCentered
                          ~vxmin_filter:"filter/minrt" ~vxmax_filter:"filter/maxrt"
                          ~vxstep_filter:"filter/distr-prec"
                          "secs" "queries"
                          vx_min vx_step nb_vx
                          { Chart.fold = fold }
        | None -> []

    let distrib_chart_descr =
        { ChartDescr.category = "Web" ;
          ChartDescr.title = "Response Time Distribution" ;
          ChartDescr.help = distrib_help "HTTP" ;
          ChartDescr.to_html = (fun name getter ->
                      display_errs Forms.Web.Distrib.from name getter |> distrib_chart) ;
          ChartDescr.filter = (fun target name getter ->
                      form target (Forms.Web.Distrib.to_edit name getter)) }

    let top_chart = function
        | Some (start, (stop, (ip_srv, (usr_filter, (tblname, (group_by, (aggr_fields, (sort_by, (max_graphs, (single_pass, ())))))))))) ->
            let stop  = match stop with Some t -> My_time.to_timeval t | None -> Timestamp.now () in
            let start = match start with Some t -> My_time.to_timeval t | None -> Timestamp.sub_interval stop default_chart_duration in
            let tblname = match tblname with Some n -> Forms.Web.TblNames.options.(n) | None -> tblname_of_timestep (Timestamp.sub_to_interval stop start) "web" Forms.Web.TblNames.options in
            let datasets = get_top ~start ~stop ?ip_srv ?usr_filter ?max_graphs ?single_pass sort_by group_by aggr_fields dbdir tblname in
            View.table_of_datasets group_by aggr_fields sort_by datasets
        | None -> []

    let top_chart_descr =
        { ChartDescr.category = "Web" ;
          ChartDescr.title = "Free Search" ;
          ChartDescr.help = top_help "HTTP" ;
          ChartDescr.to_html = (fun name getter ->
                      display_errs Forms.Web.Tops.from name getter |> top_chart) ;
          ChartDescr.filter = (fun target name getter ->
                      form target (Forms.Web.Tops.to_edit name getter)) }


end

module Dns =
struct
    include Dns
    let dbdir = dbdir^"/dns"

    let queries_chart = function
        | Some (start, (stop, (vlan, (mac_clt, (mac_srv, (ip_clt, (ip_srv, (rt_min, (rt_max, (error, (qname, (n, (sort_order, ()))))))))))))) ->
            let stop   = match stop with Some t -> My_time.to_timeval t | None -> Timestamp.now () in
            let start = match start with Some t -> My_time.to_timeval t | None -> Timestamp.sub_interval stop default_chart_duration in
            let rt_min = i2s ~min:0. rt_min in
            let rt_max = i2s ?min:rt_min rt_max in
            let n = BatOption.default 30 n
            and sort_order = match sort_order with 0 -> Plot.Asc | _ -> Plot.Desc in
            let tops = top_requests start stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?rt_min ?rt_max ?error ?qname dbdir n sort_order in
            let field_display_names =
                [ "Origin"     ; "VLAN" ;
                  "Client MAC" ; "Client IP" ;
                  "Server MAC" ; "Server IP" ;
                  "Error Code" ; "Timestamp" ;
                  "Response Time (&#x00B5s)" ; "Query Name" ] in
            View.tops_table tops field_display_names (fun (orig, vlan, eclt, clt, esrv, srv, err, ts, (_, _, _, rt, _), name) ->
                [ Origin.to_string orig ;
                  string_of_vlan vlan ;
                  EthAddr.to_string eclt ;
                  Cidr.to_string clt ;
                  EthAddr.to_string esrv ;
                  InetAddr.to_string srv ;
                  string_of_int err ;
                  Timestamp.to_string ts ;
                  string_of_float rt ;
                  name ])
        | None -> []

    let queries_chart_descr =
        { ChartDescr.category = "DNS" ;
          ChartDescr.title = "Queries" ;
          ChartDescr.help = queries_help ;
          ChartDescr.to_html = (fun name getter ->
                      display_errs Forms.Dns.Queries.from name getter |> queries_chart) ;
          ChartDescr.filter = (fun target name getter ->
                      form target (Forms.Dns.Queries.to_edit name getter)) }

    let srt_chart = function
        | Some (start, (stop, (vlan, (mac_clt, (mac_srv, (ip_clt, (ip_srv, (tx_min, (rt_min, (rt_max, (time_step, (tblname, ())))))))))))) ->
            let stop  = match stop with Some t -> My_time.to_timeval t | None -> Timestamp.now () in
            let start = match start with Some t -> My_time.to_timeval t | None -> Timestamp.sub_interval stop default_chart_duration in
            let time_step = match time_step with Some t -> t | None -> timestep_of_timestamps start stop in
            let tblname = match tblname with Some n -> Forms.Dns.TblNames.options.(n) | None -> tblname_of_timestep time_step "dns" Forms.Dns.TblNames.options in
            let time_step = Interval.to_ms time_step in
            let rt_min = i2s ~min:0. rt_min in
            let rt_max = i2s ?min:rt_min rt_max in
            let datasets = plot_resp_time start stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?rt_min ?rt_max ?tx_min time_step dbdir tblname in
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
                let i = f i "Transactions" false
                    (fun i -> Distribution.count datasets.(i) |> float_of_int) in
                i in
            let nb_vx = Array.length datasets in
            let time_step = (Int64.to_float time_step) *. 0.001
            and svg_width  = Float.of_pref "CPN_GUI_SVG_WIDTH" 1000.
            and svg_height = Float.of_pref "CPN_GUI_SVG_HEIGHT" 600. in
            Chart.xy_plot ~svg_width ~svg_height
                          ~string_of_x:string_of_date ~stacked:Chart.Stacked
                          "time" "secs"
                          (Timestamp.to_unixfloat start) time_step nb_vx
                          { Chart.fold = fold }
        | None -> []

    let srt_chart_descr =
        { ChartDescr.category = "DNS" ;
          ChartDescr.title = "Response Time Evolution" ;
          ChartDescr.help = resptime_help "DNS" ;
          ChartDescr.to_html = (fun name getter ->
                      display_errs Forms.Dns.RespTime.from name getter |> srt_chart) ;
          ChartDescr.filter = (fun target name getter ->
                      form target (Forms.Dns.RespTime.to_edit name getter)) }


    let distrib_chart = function
        | Some (start, (stop, (vlan, (mac_clt, (mac_srv, (ip_clt, (ip_srv, (rt_min, (rt_max, (prec, (top_nth, (tblname, ())))))))))))) ->
            let stop   = match stop with Some t -> My_time.to_timeval t | None -> Timestamp.now () in
            let start = match start with Some t -> My_time.to_timeval t | None -> Timestamp.sub_interval stop default_chart_duration in
            let tblname = match tblname with Some n -> Forms.Dns.TblNames.options.(n) | None -> tblname_of_timestep (Timestamp.sub_to_interval stop start) "dns" Forms.Dns.TblNames.options in
            let rt_min = i2s ~min:0. rt_min in
            let rt_max = i2s ?min:rt_min rt_max in
            let prec   = i2s ~min:0.00001 ~max:1. prec in
            let vx_step, bucket_min, bucket_max, datasets =
                plot_distrib start stop ?vlan ?mac_clt ?ip_clt ?mac_srv ?ip_srv ?rt_min ?rt_max ?prec ?top_nth dbdir tblname in
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
            and svg_width  = Float.of_pref "CPN_GUI_SVG_WIDTH" 1000.
            and svg_height = Float.of_pref "CPN_GUI_SVG_HEIGHT" 600. in
            Chart.xy_plot ~svg_width ~svg_height ~stacked:Chart.StackedCentered
                          ~vxmin_filter:"filter/minrt" ~vxmax_filter:"filter/maxrt"
                          ~vxstep_filter:"filter/distr-prec"
                          "secs" "queries"
                          vx_min vx_step nb_vx
                          { Chart.fold = fold }
        | None -> []

    let distrib_chart_descr =
        { ChartDescr.category = "DNS" ;
          ChartDescr.title = "Response Time Distribution" ;
          ChartDescr.help = distrib_help "DNS" ;
          ChartDescr.to_html = (fun name getter ->
                      display_errs Forms.Dns.Distrib.from name getter |> distrib_chart) ;
          ChartDescr.filter = (fun target name getter ->
                      form target (Forms.Dns.Distrib.to_edit name getter)) }

    let top_chart = function
        | Some (start, (stop, (ip_srv, (usr_filter, (tblname, (group_by, (aggr_fields, (sort_by, (max_graphs, (single_pass, ())))))))))) ->
            let stop  = match stop with Some t -> My_time.to_timeval t | None -> Timestamp.now () in
            let start = match start with Some t -> My_time.to_timeval t | None -> Timestamp.sub_interval stop default_chart_duration in
            let tblname = match tblname with Some n -> Forms.Dns.TblNames.options.(n) | None -> tblname_of_timestep (Timestamp.sub_to_interval stop start) "dns" Forms.Dns.TblNames.options in
            let datasets = get_top ~start ~stop ?ip_srv ?usr_filter ?max_graphs ?single_pass sort_by group_by aggr_fields dbdir tblname in
            View.table_of_datasets group_by aggr_fields sort_by datasets
        | None -> []

    let top_chart_descr =
        { ChartDescr.category = "DNS" ;
          ChartDescr.title = "Free Search" ;
          ChartDescr.help = top_help "DNS" ;
          ChartDescr.to_html = (fun name getter ->
                      display_errs Forms.Dns.Tops.from name getter |> top_chart) ;
          ChartDescr.filter = (fun target name getter ->
                      form target (Forms.Dns.Tops.to_edit name getter)) }

end

module Config =
struct
    let preferences getter =
        (* fill getter from preferences *)
        let getter' name =
            if String.starts_with "prefs/" name then (
                match Prefs.get_option (String.split ~by:"/" name |> snd) with
                | Some v -> [v]
                | None   -> getter name
            ) else getter name in
        let prefs_form = form "Config/Preferences/save" (Forms.Config.Preferences.to_edit "prefs" getter') in
        [ h1 "DNS Response Times" ;
          div ~id:"preferences" [ prefs_form ] ]

    let save_preferences getter =
        (match display_errs Forms.Config.Preferences.from "prefs" getter with
        | Some (svg_width, (svg_height, (resolve_ip, (resolve_mac, (ncores, ()))))) ->
            (* Save as much as possible in cookies as base64 string (not marshalled) *)
            let save_param n v to_string =
                BatOption.may (fun v ->
                    Dispatch.add_cookie n (Base64.str_encode (to_string v))) v in
            save_param "CPN_RESOLVER_IP" resolve_ip string_of_bool ;
            save_param "CPN_RESOLVER_MAC" resolve_mac string_of_bool ;
            save_param "CPN_GUI_SVG_WIDTH" svg_width string_of_float ;
            save_param "CPN_GUI_SVG_HEIGHT" svg_height string_of_float ;
            save_param "CPN_DB_NB_CORES" ncores string_of_int ;
            View.add_msg [cdata "Saved"]
        | None -> ()) ;
        preferences getter
end

let chart_descrs = [ Traffic.bw_chart_descr ; Traffic.peer_chart_descr ;
                     Traffic.graph_chart_descr ; Traffic.top_chart_descr ;
                     Traffic.map_chart_descr ;
                     Flow.callflow_chart_descr ;
                     Web.queries_chart_descr ; Web.srt_chart_descr ; Web.distrib_chart_descr ; Web.top_chart_descr ;
                     Dns.queries_chart_descr ; Dns.srt_chart_descr ; Dns.distrib_chart_descr ; Dns.top_chart_descr ]

let menu_entries =
    let report_names =
        Prefs.enum () //@
        (fun (n, _v) ->
            match String.nsplit ~by:"_" n with
            | ["CPN" ; "REPORT" ; name ; _ ; "TITLE" ] -> Some name
            | _ -> None) |>
        List.of_enum |>
        List.sort_unique String.compare in
    [ "Traffic", ["Bandwidth Evolution"; "Peers"; "Free Search"; "Network Graph"; "World Map"; "Call Flow"] ;
      "DNS", ["Response Time Evolution"; "Queries"; "Free Search"; "Response Time Distribution"] ;
      "Web", ["Response Time Evolution"; "Queries"; "Free Search"; "Response Time Distribution"] ;
      "Reports", report_names ;
      "Config", ["Preferences"] ]

let find_chart cat title =
    List.find (fun d -> d.ChartDescr.category = cat && d.ChartDescr.title = title) chart_descrs

module Report =
struct
    type report_page = { page_no : int ;
                         title : string option ;
                         descr : string option ;
                         chart : string ;
                         getter : string -> string list }

    let report_page_prefix name page_no =
        "CPN_REPORT_"^ name ^"_"^ string_of_int page_no

    let get_report_page name page_no =
        let pn n = report_page_prefix name page_no ^"_"^ n in
        let envvar_of_field n =
            let n = String.nreplace n "-" "_" in
            let n = String.nreplace n " " "_" in
            let n = String.nreplace n "/" "_" in
            String.uppercase n in
        { page_no ;
          title = Prefs.get_option (pn "TITLE") ;
          descr = Prefs.get_option (pn "DESCR") ;
          chart = Prefs.get_option (pn "CHART") |> BatOption.get ;
          getter = (fun n ->
            match Prefs.get_option (envvar_of_field n) with
            | Some x -> [x]
            | None -> []) }

    let get_report name =
        Enum.unfold 0 (fun page_no ->
            Log.info "try to fetch page %d..." page_no ;
            try Some (get_report_page name page_no, succ page_no)
            with Invalid_argument _ -> None)

    let chart_descr_of_page page =
        List.find (fun d -> d.ChartDescr.category ^"/"^ d.ChartDescr.title = page.chart) chart_descrs

    let chart_of report_name page =
        let rn = report_page_prefix report_name page.page_no in
        try let chart_descr = chart_descr_of_page page in
            chart_descr.ChartDescr.to_html rn page.getter
        with Not_found ->
            [ p ~cls:"err" [ cdata "No such chart " ;
                             tag "i" [ cdata page.chart ] ] ]

    let build report_name _args =
        get_report report_name /@
        (fun page ->
            let chart = chart_of report_name page in
            View.make_report_page page.page_no page.title page.descr chart) |>
        List.of_enum |> List.concat |>
        List.cons (h1 report_name)

end

