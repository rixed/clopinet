(* CGI script for visualizing mlrrd datas *)
open Batteries
open Html
open Datatype
open Input.Ops

let dbdir = "../test.db"

module View =
struct
    include View

    (* notification system *)

    let msgs = ref []

    let add_msg_with_class c txt =
        msgs := (p ~attrs:["class",c] [cdata txt]) :: !msgs
    let add_msg = add_msg_with_class "ok"
    let add_err = add_msg_with_class "nok"

    let msgs () = div ~attrs:["id","notifs"] !msgs

    (* Google charts *)

    let chart_head = [
        tag "script" ~attrs:["type","text/javascript" ; "src","https://www.google.com/jsapi"] [] ;
        tag "script" ~attrs:["type","text/javascript" ; "src","static/js/graph.js"] [] ;
        tag "script" ~attrs:["type","text/javascript"]
            [ cdata "graph_init();" ]
    ]

    let chart_div = div ~attrs:["id","chart_div"] []

    (* rendering of pages *)

    let menu () =
        let html_of_entry e = tag "li" [ tag "a" ~attrs:["href","?action=main/"^e] [cdata e] ]
        and menu_entries = ["traffic/bandwidth"; "traffic/peers"; "traffic/tops"; "DNS/resptime"; "web/resptime"; "logout"] in
        tag ~attrs:["class","menu"] "ul" (List.map html_of_entry menu_entries)

    (* add the menu *)
    let make_app_page content =
        let body = menu () :: msgs () :: [ tag "div" ~attrs:["id","page"] content ]
        and head = [ title "MlRRD" ; link_css "static/css/style.css" ] @ chart_head in
        html head body

    let table_of_datasets datasets =
        let all_rows =
            Hashtbl.fold (fun label pts doc ->
                let rows =
                    List.map (fun (x, y) ->
                        tr [ td [ cdata (string_of_float x) ] ;
                             td [ cdata (string_of_float y) ] ])
                        pts in
                (tr [ th ~attrs:["colspan", "2"] [ cdata label ] ] ::
                rows) @ doc)
                datasets [] in
        [ table (tr [ th [ cdata "time" ] ;
                      th [ cdata "qtt" ] ] ::
                 all_rows) ]

    let js_of_timesets time_step tmin datasets =
        let datasets = List.rev datasets in (* for some reason the legend is reversed in the graph lib *)
        let nb_xs = List.hd datasets |> snd |> Array.length in
        let js =
            let os = IO.output_string () in
            Printf.fprintf os "{\ncols: %a,\nrows: [ "
                (* display the labels *)
                (List.print ~first:"[ { label: 'Time', type: 'datetime' },\n"
                            ~last:" ]" ~sep:",\n"
                               (fun oc (label, _ys) -> Printf.fprintf oc "{ label: '%s', type: 'number' }" label))
                datasets ;
            (* Iter on all rows *)
            let rec print_row r t =
                if r < nb_xs then (
                    Printf.fprintf os "{c: [{v: new Date(%Ld)}, " t ;
                    (* iter on all datasets *)
                    List.iter (fun (_label, ys) ->
                        Printf.fprintf os "{v:%f}," ys.(r))
                        datasets ;
                    Printf.fprintf os " ]},\n" ;
                    print_row (succ r) (Int64.add t time_step)
                ) in
            print_row 0 tmin ;
            Printf.fprintf os " ]\n}\n" ;
            IO.close_out os in
        raw js

    let js_of_timedistr time_step tmin datasets =
        let os = IO.output_string () in
        Printf.fprintf os "{\n\
    cols: [ {label:'Time',type:'datetime'},{label:'Min',type:'number'},{label:'Avg-\\u03C3',type:'number'},{label:'Avg',type:'number'},{label:'Avg+\\u03C3',type:'number'},{label:'Max',type:'number'},{label:'#tx',type:'number'} ],\n\
    rows: [ " ;
        (* Iter on all dates *)
        let rec print_row r t =
            if r < Array.length datasets then (
                (match datasets.(r) with
                    | None ->
                        Printf.fprintf os "{c: [{v: new Date(%Ld)},{v:0},{v:0},{v:0},{v:0},{v:0},{v:0}] },\n" t
                    | Some ((c, mi, ma, avg, _v) as d) ->
                        let s = Distribution.std_dev d in
                        Printf.fprintf os "{c: [{v: new Date(%Ld)},{v:%f},{v:%f},{v:%f},{v:%f},{v:%f},{v:%d}] },\n"
                            t mi (max 0. (avg -. s)) avg (avg +. s) ma c) ;
                print_row (succ r) (Int64.add t time_step)
            ) in
        print_row 0 tmin ;
        Printf.fprintf os " ]\n}\n" ;
        let js = IO.close_out os in
        raw js

    let js_of_single_keyed_data lab datasets what =
        let js =
            let os = IO.output_string () in
            Printf.fprintf os "{\ncols: [{label: '%s', type: 'string'},{label: '%s', type: 'number'}],\nrows: %a\n"
                lab what
                (* display the rows *)
                (Hashtbl.print ~first:"[" ~last:"]" ~sep:",\n" ~kvsep:","
                               (fun oc s -> Printf.fprintf oc "{c:[{v:'%s'}" s)
                               (fun oc y -> Printf.fprintf oc "{v:%f}]}" y))
                datasets ;
            Printf.fprintf os " \n}\n" ;
            IO.close_out os in
        raw js

    let js_of_double_keyed_data lab1 lab2 datasets what =
        let js =
            let os = IO.output_string () in
            Printf.fprintf os "{\ncols: [{label: '%s', type: 'string'},{label: '%s', type: 'string'},{label: '%s', type: 'number'}],\nrows: %a\n"
                lab1 lab2 what
                (* display the rows *)
                (Hashtbl.print ~first:"[" ~last:"]" ~sep:",\n" ~kvsep:","
                               (fun oc (s, d) -> Printf.fprintf oc "{c:[{v:'%s'},{v:'%s'}" s d)
                               (fun oc y -> Printf.fprintf oc "{v:%f}]}" y))
                datasets ;
            Printf.fprintf os " \n}\n" ;
            IO.close_out os in
        raw js

    let resp_times title time_step start datasets =
        [ chart_div ;
          tag "script" ~attrs:["type","text/javascript"]
            [ Raw "var data = new google.visualization.DataTable(" ;
              js_of_timedistr time_step start datasets ;
              Raw (");\n\
var options = {\n\
    title:'"^title^"',\n\
    width:'100%',\n\
    height:600,\n\
    hAxis: {format:'y-MM-dd HH:mm:ss.SSS', gridlines:{color:'#333'}, title:'Time'},\n\
    vAxes: [{title:'Response Time (sec)'},\n\
            {title:'#Transaction', textStyle:{color:'#78a'}}],\n\
    seriesType: 'line',\n\
    series: {0: {type:'area', color:'#aaa', areaOpacity:0, lineWidth:0},\n\
             1: {type:'area', color:'#777', lineWidth:0},\n\
             2: {type:'line', color:'#555'},\n\
             3: {type:'area', color:'#777', lineWidth:0},\n\
             4: {type:'area', color:'#aaa', lineWidth:0},\n\
             5: {type:'line', color:'#78a', targetAxisIndex:1}},\n\
    isStacked: true,\n\
    legend:{textStyle:{fontSize:9}}\n\
};\n\
var chart = new google.visualization.ComboChart(document.getElementById('chart_div'));\n\
chart.draw(data, options);\n") ] ]

end

module InputOfDatatype (D : DATATYPE) :
    TYPE with type t = D.t =
struct
    module String = Input.String (struct let min = Some 1 let max = None end)
    type t = D.t
    let name = D.name
    let to_html v = [ cdata (html_of_user_value D.to_string v) ]
    let edit name v =
        [ input [ "name", name ;
                  "value", input_of_user_value D.to_string v ] ] @
        err_msg_of v
    let from_args name args =
        match String.from_args name args with
        | Error _ as x -> x
        | Value s  ->
            (try Value (D.of_string s)
            with End_of_file -> Error ("Not enough data", s)
               | Overflow    -> Error ("Integer overflow", s)
               | exn         -> Error (Printexc.to_string exn, s))
end

module OptInputOfDatatype (D : DATATYPE) :
    TYPE with type t = D.t option =
struct
    module String = Input.OptString (struct let min = Some 0 let max = None end)
    type t = D.t option
    let name = D.name
    let to_html v = [ cdata (html_of_user_value (function None -> "<i>unset</i>"
                                                        | Some s -> D.to_string s) v) ]
    let edit name v =
        [ input [ "name", name ;
                  "value", input_of_user_value (function None -> ""
                                                       | Some s -> D.to_string s) v ] ] @
        err_msg_of v
    let from_args name args =
        match String.from_args name args with
        | Error _ | Value None as x -> x
        | Value (Some s) -> 
            (try Value (Some (D.of_string s))
            with End_of_file -> Error ("Not enough data", s)
               | Overflow    -> Error ("Integer overflow", s)
               | exn         -> Error (Printexc.to_string exn, s))
end

module Forms =
struct
    open Input

    module LoginField = struct
        module Type = Input.String (struct let min = Some 1 let max = Some 100 end)
        let name = "name"
    end
    module PasswdField = struct
        module Type = Input.Password (struct let min = Some 3 let max = Some 100 end)
        let name = "password"
    end
    
    module Login = RecordOf (ConsOf (FieldOf (LoginField))
                            (ConsOf (FieldOf (PasswdField))
                                    (NulType)))

    module StartField = struct
        module Type = InputOfDatatype(Timestamp)
        let name = "start"
    end
    module StopField = struct
        module Type = InputOfDatatype(Timestamp)
        let name = "stop"
    end
    module OptStartField = struct
        module Type = OptInputOfDatatype(Timestamp)
        let name = "start"
    end
    module OptStopField = struct
        module Type = OptInputOfDatatype(Timestamp)
        let name = "stop"
    end
    module VlanField = struct
        module Type = OptInputOfDatatype(Integer16)
        let name = "vlan"
    end
    module MacSrcField = struct
        module Type = OptInputOfDatatype(EthAddr)
        let name = "eth-src"
    end
    module MacDstField = struct
        module Type = OptInputOfDatatype(EthAddr)
        let name = "eth-dest"
    end
    module EthProtoField = struct
        module Type = OptInputOfDatatype(Integer16)
        let name = "eth-proto"
    end
    module IpSrcField = struct
        module Type = OptInputOfDatatype(Cidr)
        let name = "ip-src"
    end
    module IpDstField = struct
        module Type = OptInputOfDatatype(Cidr)
        let name = "ip-dst"
    end
    module IpProtoField = struct
        module Type = OptInputOfDatatype(Integer8)
        let name = "ip-proto"
    end
    module TimeStepField = struct
        module Type = Integer (struct let min = Some 1 let max = None end)
        let name = "time step (ms)"
    end
    module GroupByField = struct
        module Type = Enum (struct let name = "key"
                                   let options = [| "src-mac";"dst-mac";"src-ip";"dst-ip";"apps" |] end)
        let name = "group by"
    end
    module GroupByPeerField = struct
        module Type = Enum (struct let name = "key"
                                   let options = [| "mac";"ip" |] end)
        let name = "group by"
    end
    module GroupByTopField = struct
        module Type = Enum (struct let name = "key"
                                   let options = [| "src-mac";"dst-mac";"mac (both)";"src-ip";"dst-ip";"ip (both)";"app" |] end)
        let name = "group by"
    end
    module PlotWhat = struct
        module Type = Enum (struct let name = "y"
                                   let options = [| "packets";"volume" |] end)
        let name = "Y"
    end
    module MaxGraphsField = struct
        module Type = OptInteger (struct let min = Some 1 let max = Some 1000 end)
        let name = "#series"
    end
    module TxMin = struct
        module Type = OptInteger (struct let min = Some 1 let max = None end)
        let name = "#tx min"
    end
    module MinRespTime = struct
        module Type = OptFloat (struct let min = Some 0. let max = None end)
        let name = "min resp time (s)"
    end
    module MaxRespTime = struct
        module Type = OptFloat (struct let min = Some 0. let max = None end)
        let name = "max resp time (s)"
    end
    module Traffic = struct
        module TblNames = struct
            let name = "db-tables"
            let options = [| "30secs";"10mins";"1hour" |]
        end
        module TblNameField = struct
            module Type = Enum (TblNames)
            let name = "db-table"
        end
        module Bandwidth = RecordOf (ConsOf (FieldOf (StartField))
                                    (ConsOf (FieldOf (StopField))
                                    (ConsOf (FieldOf (VlanField))
                                    (ConsOf (FieldOf (MacSrcField))
                                    (ConsOf (FieldOf (MacDstField))
                                    (ConsOf (FieldOf (EthProtoField))
                                    (ConsOf (FieldOf (IpSrcField))
                                    (ConsOf (FieldOf (IpDstField))
                                    (ConsOf (FieldOf (IpProtoField))
                                    (ConsOf (FieldOf (TimeStepField))
                                    (ConsOf (FieldOf (TblNameField))
                                    (ConsOf (FieldOf (PlotWhat))
                                    (ConsOf (FieldOf (GroupByField))
                                    (ConsOf (FieldOf (MaxGraphsField))
                                            (NulType)))))))))))))))

        module Peers = RecordOf (ConsOf (FieldOf (OptStartField))
                                (ConsOf (FieldOf (OptStopField))
                                (ConsOf (FieldOf (VlanField))
                                (ConsOf (FieldOf (MacSrcField))
                                (ConsOf (FieldOf (MacDstField))
                                (ConsOf (FieldOf (EthProtoField))
                                (ConsOf (FieldOf (IpSrcField))
                                (ConsOf (FieldOf (IpDstField))
                                (ConsOf (FieldOf (IpProtoField))
                                (ConsOf (FieldOf (TblNameField))
                                (ConsOf (FieldOf (PlotWhat))
                                (ConsOf (FieldOf (GroupByPeerField))
                                (ConsOf (FieldOf (MaxGraphsField))
                                        (NulType))))))))))))))

        module Tops = RecordOf (ConsOf (FieldOf (OptStartField))
                               (ConsOf (FieldOf (OptStopField))
                               (ConsOf (FieldOf (VlanField))
                               (ConsOf (FieldOf (MacSrcField))
                               (ConsOf (FieldOf (MacDstField))
                               (ConsOf (FieldOf (EthProtoField))
                               (ConsOf (FieldOf (IpSrcField))
                               (ConsOf (FieldOf (IpDstField))
                               (ConsOf (FieldOf (IpProtoField))
                               (ConsOf (FieldOf (TblNameField))
                               (ConsOf (FieldOf (PlotWhat))
                               (ConsOf (FieldOf (GroupByTopField))
                               (ConsOf (FieldOf (MaxGraphsField))
                                       (NulType))))))))))))))
    end

    module Web = struct
        module TblNames = struct
            let name = "db-tables"
            let options = [| "queries";"1min";"10mins";"1hour" |]
        end
        module TblNameField = struct
            module Type = Enum (TblNames)
            let name = "db-table"
        end
        module HttpStatus = struct
            module Type = OptInteger (struct let min = Some 100 let max = Some 999 end)
            let name = "status"
        end
        module HttpHost = struct
            module Type = OptString (struct let min = None let max = None end)
            let name = "host"
        end
        module HttpURL = struct
            module Type = OptString (struct let min = None let max = None end)
            let name = "url"
        end
        module RespTime = RecordOf (ConsOf (FieldOf (StartField))
                                   (ConsOf (FieldOf (StopField))
                                   (ConsOf (FieldOf (VlanField))
                                   (ConsOf (FieldOf (MacSrcField))
                                   (ConsOf (FieldOf (MacDstField))
                                   (ConsOf (FieldOf (IpSrcField))
                                   (ConsOf (FieldOf (IpDstField))
                                   (ConsOf (FieldOf (HttpStatus))
                                   (ConsOf (FieldOf (HttpHost))
                                   (ConsOf (FieldOf (HttpURL))
                                   (ConsOf (FieldOf (MinRespTime))
                                   (ConsOf (FieldOf (MaxRespTime))
                                   (ConsOf (FieldOf (TimeStepField))
                                   (ConsOf (FieldOf (TblNameField))
                                           (NulType)))))))))))))))
    end

    module Dns = struct
        module TblNames = struct
            let name = "db-tables"
            let options = [| "queries";"1min";"10mins";"1hour" |]
        end
        module TblNameField = struct
            module Type = Enum (TblNames)
            let name = "db-table"
        end
        module RespTime = RecordOf (ConsOf (FieldOf (StartField))
                                   (ConsOf (FieldOf (StopField))
                                   (ConsOf (FieldOf (VlanField))
                                   (ConsOf (FieldOf (MacSrcField))
                                   (ConsOf (FieldOf (MacDstField))
                                   (ConsOf (FieldOf (IpSrcField))
                                   (ConsOf (FieldOf (IpDstField))
                                   (ConsOf (FieldOf (TxMin))
                                   (ConsOf (FieldOf (MinRespTime))
                                   (ConsOf (FieldOf (MaxRespTime))
                                   (ConsOf (FieldOf (TimeStepField))
                                   (ConsOf (FieldOf (TblNameField))
                                           (NulType)))))))))))))
    end

end

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
            View.make_app_page [ h1 "Authentification" ;
                                 form "login" (Forms.Login.edit "login" login) ] in
        let may_add_err = function
            | Error (err, inp) -> View.add_err (inp^":&nbsp;"^err)
            | Value _ -> () in
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
        | x, (y, ()) ->
            may_add_err x ;
            may_add_err y ;
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
            let filters_form = form "main/traffic/bandwidth" (Forms.Traffic.Bandwidth.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_src, (Value mac_dst, (Value eth_proto, (Value ip_src, (Value ip_dst, (Value ip_proto, (Value time_step, (Value tblname, (Value what, (Value group_by, (Value max_graphs, ()))))))))))))) ->
                    let time_step = Int64.of_int time_step
                    and tblname = Forms.Traffic.TblNames.options.(tblname)
                    and what = if what = 0 then PacketCount else Volume in
                    let datasets = match group_by with
                        | 0 (* src-mac *) | 1 (* dst-mac *) as sd ->
                            eth_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?max_graphs (sd = 0) what time_step dbdir tblname
                        | 2 (* src-ip *) | 3 (* dst-ip *) as sd ->
                            ip_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?max_graphs (sd = 2) what time_step dbdir tblname
                        | _ (* default, apps *) ->
                            app_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?max_graphs what time_step dbdir tblname in
                    if datasets = [] then
                        [ cdata "No data" ]
                    else
                        let what = if what = PacketCount then "Packets" else "Bytes" in
                        [ View.chart_div ;
                          tag "script" ~attrs:["type","text/javascript"]
                            [ Raw "var data = new google.visualization.DataTable(" ;
                              View.js_of_timesets time_step start datasets ;
                              Raw (");\n\
var options = {\n\
    title:'Traffic - "^what^"/sec',\n\
    width:'100%',\n\
    height:600,\n\
    isStacked:true,\n\
    hAxis:{format:'MMM d, y HH:mm', gridlines:{color:'#333'}, title:'Time'},\n\
    legend:{textStyle:{fontSize:9}}\n\
};\n\
var chart = new google.visualization.AreaChart(document.getElementById('chart_div'));\n\
chart.draw(data, options);\n") ] ]
                | _ ->
                    [ cdata "Fill in the form above" ] in
            View.make_app_page
                (h1 "Bandwidth" :: filters_form :: disp_graph)

        let peers args =
            let filters = Forms.Traffic.Peers.from_args "filter" args in
            let filters_form = form "main/traffic/peers" (Forms.Traffic.Peers.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_src, (Value mac_dst, (Value eth_proto, (Value ip_src, (Value ip_dst, (Value ip_proto, (Value tblname, (Value what, (Value group_by, (Value max_graphs, ())))))))))))) ->
                    let tblname = Forms.Traffic.TblNames.options.(tblname)
                    and what = if what = 0 then PacketCount else Volume in
                    let datasets = match group_by with
                        | 0 (* mac *) ->
                            eth_plot_vol_tot2 ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?max_graphs what dbdir tblname
                        | _ (* ip *) ->
                            ip_plot_vol_tot2 ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?max_graphs what dbdir tblname in
                    if Hashtbl.is_empty datasets then
                        [ cdata "No data" ]
                    else
                        let units = if what = PacketCount then "Packets" else "Bytes" in
                        [ View.chart_div ;
                          tag "script" ~attrs:["type","text/javascript"]
                            [ Raw "var data = new google.visualization.DataTable(" ;
                              View.js_of_double_keyed_data "src" "dst" datasets units ;
                              Raw (");\n\
console.log(data);\n\
var options = {\n\
    width:'100%',\n\
    height:600,\n\
    showRowNumber:true,\n\
    sortColumn:2,\n\
    sortAscending:false\n\
};\n\
var chart = new google.visualization.Table(document.getElementById('chart_div'));\n\
chart.draw(data, options);\n") ] ]
                | _ ->
                    [ cdata "Fill in the form above" ] in
            View.make_app_page
                (h1 "Peers" :: filters_form :: disp_graph)

        let tops args =
            let filters = Forms.Traffic.Tops.from_args "filter" args in
            let filters_form = form "main/traffic/tops" (Forms.Traffic.Tops.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_src, (Value mac_dst, (Value eth_proto, (Value ip_src, (Value ip_dst, (Value ip_proto, (Value tblname, (Value what, (Value group_by, (Value max_graphs, ())))))))))))) ->
                    let tblname = Forms.Traffic.TblNames.options.(tblname)
                    and what = if what = 0 then PacketCount else Volume in
                    let key, datasets = match group_by with
                        | 0 (* src-mac *) ->
                            "src mac", eth_plot_vol_top ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?max_graphs true what dbdir tblname
                        | 1 (* dst-mac *) ->
                            "dst mac", eth_plot_vol_top ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?max_graphs false what dbdir tblname
                        | 2 (* mac (both) *) ->
                            "mac (both)", eth_plot_vol_top_both ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?max_graphs what dbdir tblname
                        | 3 (* src-ip *) ->
                            "src IP", ip_plot_vol_top ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?max_graphs true what dbdir tblname
                        | 4 (* dst-ip *) ->
                            "dst IP", ip_plot_vol_top ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?max_graphs false what dbdir tblname
                        | 5 (* ip (both) *) ->
                            "IP (both)", ip_plot_vol_top_both ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?max_graphs what dbdir tblname
                        | _ (* app *) ->
                            "Port", app_plot_vol_top ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip_proto ?max_graphs what dbdir tblname in

                    if Hashtbl.is_empty datasets then
                        [ cdata "No data" ]
                    else
                        let units = if what = PacketCount then "Packets" else "Bytes" in
                        [ View.chart_div ;
                          tag "script" ~attrs:["type","text/javascript"]
                            [ Raw "var data = new google.visualization.DataTable(" ;
                              View.js_of_single_keyed_data key datasets units ;
                              Raw (");\n\
console.log(data);\n\
var options = {\n\
    showRowNumber:true,\n\
    sortColumn:1,\n\
    sortAscending:false\n\
};\n\
var chart = new google.visualization.Table(document.getElementById('chart_div'));\n\
chart.draw(data, options);\n") ] ]
                | _ ->
                    [ cdata "Fill in the form above" ] in
            View.make_app_page
                (h1 "Tops" :: filters_form :: disp_graph)

    end

    module Web =
    struct
        include Web
        let dbdir = dbdir^"/web"
        let s2m x = x *. 1_000_000.
        let resp_time args =
            let filters = Forms.Web.RespTime.from_args "filter" args in
            let filters_form = form "main/web/resptime" (Forms.Web.RespTime.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_clt, (Value mac_srv, (Value client, (Value server, (Value status, (Value host, (Value url, (Value rt_min, (Value rt_max, (Value time_step, (Value tblname, ()))))))))))))) ->
                    let time_step = Int64.of_int time_step
                    and tblname = Forms.Web.TblNames.options.(tblname) in
                    let rt_min = BatOption.map s2m rt_min
                    and rt_max = BatOption.map s2m rt_max in
                    let datasets = plot_resp_time start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?status ?host ?url ?rt_min ?rt_max time_step dbdir tblname in
                    View.resp_times "Web - Average Response Time (sec)" time_step start datasets
                | _ ->
                    [ cdata "Fill in the form above" ] in
            View.make_app_page
                (h1 "Web Response Time" :: filters_form :: disp_graph)

    end

    module Dns =
    struct
        include Dns
        let dbdir = dbdir^"/dns"
        let s2m x = x *. 1_000_000.
        let resp_time args =
            let filters = Forms.Dns.RespTime.from_args "filter" args in
            let filters_form = form "main/DNS/resptime" (Forms.Dns.RespTime.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_clt, (Value mac_srv, (Value client, (Value server, (Value tx_min, (Value rt_min, (Value rt_max, (Value time_step, (Value tblname, ()))))))))))) ->
                    let time_step = Int64.of_int time_step
                    and tblname = Forms.Dns.TblNames.options.(tblname) in
                    let rt_min = BatOption.map s2m rt_min
                    and rt_max = BatOption.map s2m rt_max in
                    let datasets = plot_resp_time start stop ?vlan ?mac_clt ?client ?mac_srv ?server ?rt_min ?rt_max ?tx_min time_step dbdir tblname in
                    View.resp_times "DNS - Average Response Time (sec)" time_step start datasets
                | _ ->
                    [ cdata "Fill in the form above" ] in
            View.make_app_page
                (h1 "DNS Response Time" :: filters_form :: disp_graph)

    end

end

let _ =
    Dispatch.run (function
        | ["info"] -> Ctrl.Info.run
        | [""] | ["login"] ->
            Ctrl.login
        | ["main"] ->
            Ctrl.ensure_logged Ctrl.main
        | ["main"; "logout"] ->
            Ctrl.logout
        | ["main";"traffic";"bandwidth"] ->
            Ctrl.ensure_logged Ctrl.Traffic.bandwidth
        | ["main";"traffic";"peers"] ->
            Ctrl.ensure_logged Ctrl.Traffic.peers
        | ["main";"traffic";"tops"] ->
            Ctrl.ensure_logged Ctrl.Traffic.tops
        | ["main";"web";"resptime"] ->
            Ctrl.ensure_logged Ctrl.Web.resp_time
        | ["main";"DNS";"resptime"] ->
            Ctrl.ensure_logged Ctrl.Dns.resp_time
        | _ -> Ctrl.Invalid.run)

