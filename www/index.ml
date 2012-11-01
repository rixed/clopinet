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
        and menu_entries = ["traffic/bandwidth"; "traffic/peers"; "traffic/tops"; "DNS/resptime"; "DNS/top"; "web/resptime"; "web/top"; "logout"] in
        tag ~attrs:["id","menu"] "ul" (List.map html_of_entry menu_entries)

    (* add the menu *)
    let make_app_page content =
        let body = menu () :: msgs () :: [ tag "div" ~attrs:["id","page"] content ]
        and head = [ title "MlRRD" ; link_css "static/css/style.css" ] @ chart_head in
        html head body

    let make_graph_page title form graph =
        let content =
            [ h1 title ;
              table [ tr [ td [form] ; td ~attrs:["width","100%"] graph ] ] ] in
        make_app_page content

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

    let tops_table tops heads vals_of_top =
        let all_rows =
            Array.fold_left (fun rows top ->
                match top with None -> rows
                             | Some top ->
                                  let vals = vals_of_top top in
                                  tr (List.map (fun v -> td [ raw v ]) vals) :: rows)
                [] tops |>
            List.rev in
        let heads = thead [ tr (List.map (fun v -> th [ raw v ]) heads) ] in
        [ table ~attrs:["class","tops"] (heads :: all_rows) ]

    let bandwidth_chart title time_step start datasets =
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
            print_row 0 start ;
            Printf.fprintf os " ]\n}\n" ;
            IO.close_out os in
        [ chart_div ;
          tag "script" ~attrs:["type","text/javascript"]
            [ raw ("var data = new google.visualization.DataTable(" ^ js ^ ");\n\
var options = {\n\
    title:'"^title^"',\n\
    width:'100%',\n\
    height:600,\n\
    lineWidth:1,\n\
    isStacked:true,\n\
    hAxis:{format:'MMM d, y HH:mm', gridlines:{color:'#333'}, title:'Time'},\n\
    legend:{textStyle:{fontSize:9}}\n\
};\n\
var chart = new google.visualization.AreaChart(document.getElementById('chart_div'));\n\
chart.draw(data, options);\n") ] ]

    let top_chart lab datasets units =
        let js =
            let os = IO.output_string () in
            Printf.fprintf os "{\ncols: [{label: '%s', type: 'string'},{label: '%s', type: 'number'}],\nrows: %a\n"
                lab units 
                (* display the rows *)
                (Hashtbl.print ~first:"[" ~last:"]" ~sep:",\n" ~kvsep:","
                               (fun oc s -> Printf.fprintf oc "{c:[{v:'%s'}" s)
                               (fun oc y -> Printf.fprintf oc "{v:%f}]}" y))
                datasets ;
            Printf.fprintf os " \n}\n" ;
            IO.close_out os in
        [ chart_div ;
          tag "script" ~attrs:["type","text/javascript"]
            [ raw ("var data = new google.visualization.DataTable(" ^ js ^ ");\n\
var options = {\n\
    showRowNumber:true,\n\
    sortColumn:1,\n\
    sortAscending:false\n\
};\n\
var chart = new google.visualization.Table(document.getElementById('chart_div'));\n\
chart.draw(data, options);\n") ] ]

    let color_scale =
        [ 0.0, [| 0.0; 0.5; 1.0 |] ;
          0.3, [| 0.2; 0.8; 1.0 |] ;
          0.6, [| 0.0; 1.0; 0.7 |] ;
          0.9, [| 0.9; 0.9; 0.4 |] ;
          1.0, [| 1.0; 0.4; 0.3 |] ]
    let peers_chart _lab1 _lab2 datasets _what =
        let pi = BatFloat.pi in
        let to_deg rad = 180. *. rad /. pi in
        let svg_width = 800. and svg_height = 600. in
        let inner_rad = 3.5 *. svg_height /. 10. in
        let inner_x = svg_height/.2. and inner_y = svg_height/.2. in
        let inner r x y = (x -. inner_x) *. r +. inner_x,
                          (y -. inner_y) *. r +. inner_y in
        let x_of_ang ang = inner_rad *. cos ang +. inner_x
        and y_of_ang ang = inner_rad *. sin ang +. inner_y in
        let legend_x = svg_height +. 50.
        and legend_y = 50. in
        let legend_height = svg_height -. 2.*. legend_y in
        (* Build a list of all peers *)
        let other_volume = Hashtbl.find datasets ("other", "") in
        let tot_volume = ref 0. and max_volume = ref 0. in
        let peers = (Hashtbl.fold (fun (a, b) v l ->
                                     if a = "other" then l else (
                                         tot_volume := !tot_volume +. v ;
                                         max_volume := max !max_volume v ;
                                         a::b::l
                                     )) datasets [] |>
                     List.sort String.compare) |>
                    List.unique_cmp ~cmp:String.compare in
        let opacity w = 0.2 +. 0.8 *. w in
        let color w = Color.get color_scale w in
        let nb_peers = List.length peers in
        let peers =
            let a = 2.*.pi /. (float_of_int nb_peers) in
            List.mapi (fun i p ->
                let a = float_of_int i *. a -. pi/.2. in
                let a = mod_float a (2.*.pi) in
                p, "peer"^string_of_int i, a) peers in
        let get_by_name p =
            List.find_map (fun ((p', _, _) as r) -> if p = p' then Some r else None) peers in
        [ svg ~attrs:["width",  string_of_float svg_width ;
                      "height", string_of_float svg_height ]
              [ (* legend *)
                g (rect ~fill:"none" ~stroke_width:1. ~stroke:"#ef0" legend_x legend_y 4. legend_height ::
                   texts ~fill:"#bbb" ~stroke:"#aaa" (legend_x +. 20.) (legend_y +. 20.)
                       [ Printf.sprintf "%.0f bytes" !tot_volume, 20. ;
                         Printf.sprintf "%05.2f%% of total"
                           (100.*. !tot_volume /. (other_volume +. !tot_volume)), 16. ]) ;
                (* Traffic *)
                g (Hashtbl.fold (fun (p1, p2) v l ->
                     try (
                         let _, c1, a1 = get_by_name p1
                         and _, c2, a2 = get_by_name p2 in
                         let x1, y1 = x_of_ang a1, y_of_ang a1
                         and x2, y2 = x_of_ang a2, y_of_ang a2 in
                         let close r a = (* return x,y of a point at radius r and at ang a2+da *)
                            inner r (x_of_ang a) (y_of_ang a) in
                         let c1x, c1y = if p1 <> p2 then inner 0.5 x1 y1
                                        else inner 0.8 (x_of_ang (a1+.0.6)) (y_of_ang (a1+.0.6))
                         and c2x, c2y = inner 0.5 x2 y2 in
                         let w = v /. !max_volume in
                         let w'= 0.3 +. 0.7 *. v /. !max_volume in
                         let d = moveto (close 0.97 (a1+.0.05*.w')) ^
                                 curveto (c1x, c1y) (c2x, c2y) (close 0.8 (a2-.0.09*.w')) ^
                                 lineto (close 0.8 (a2-.0.15*.w')) ^
                                 lineto (close 0.97 a2) ^
                                 lineto (close 0.8 (a2+.0.15*.w')) ^
                                 lineto (close 0.8 (a2+.0.09*.w')) ^
                                 curveto (c2x, c2y) (c1x, c1y) (close 0.97 (a1-.0.05*.w')) ^
                                 closepath in
                         let col = color w in
                         (g [ path ~attrs:["class",c1^" "^c2] ~fill:col ~stroke:col ~stroke_opacity:0. ~fill_opacity:(opacity w) d ]) :: l
                     ) with Not_found -> l)
                     datasets []) ;
                (* The peers *)
                g (List.map (fun (p, pclass, a) ->
                    let x, y = x_of_ang a, y_of_ang a in
                    let a' = (mod_float (a +. pi/.2.) pi) -. pi/.2. in
                    let anchor = if a < 0.5*.pi || a > 1.5*.pi then "start" else "end" in
                       g ~attrs:["transform","translate("^string_of_float x^","^string_of_float y^") "^
                                             "rotate("^string_of_float (to_deg a') ^")" ;
                                 "onmouseover","peer_select(evt, '"^pclass^"')" ;
                                 "onmouseout", "peer_unselect(evt, '"^pclass^"')" ]
                         [ text ~attrs:["class",pclass] ~style:("text-anchor:"^anchor^"; dominant-baseline:central")
                                ~font_size:15. ~fill:"#444" ~stroke:"#444" ~stroke_opacity:0. p ]
                     ) peers) ] ]

    let peers_table lab1 lab2 datasets what =
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
        [ chart_div ;
          tag "script" ~attrs:["type","text/javascript"]
            [ raw ("var data = new google.visualization.DataTable(" ^ js ^ ");\n\
var options = {\n\
    width:'100%',\n\
    height:600,\n\
    showRowNumber:true,\n\
    sortColumn:2,\n\
    sortAscending:false\n\
};\n\
var chart = new google.visualization.Table(document.getElementById('chart_div'));\n\
chart.draw(data, options);\n") ] ]

    let resp_times_chart title time_step start datasets =
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
        print_row 0 start ;
        Printf.fprintf os " ]\n}\n" ;
        let js = IO.close_out os in
        [ chart_div ;
          tag "script" ~attrs:["type","text/javascript"]
            [ raw ("var data = new google.visualization.DataTable(" ^ js ^ ");\n\
var options = {\n\
    title:'"^title^"',\n\
    /*width:'100%',*/\n\
    height:600,\n\
    lineWidth:1,\n\
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

    (* Misc *)

    let string_of_vlan = function
        | None -> ""
        | Some v -> string_of_int v
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
        let display_name = "name"
        let uniq_name = "name"
        let persistant = false
    end
    module PasswdField = struct
        module Type = Input.Password (struct let min = Some 3 let max = Some 100 end)
        let display_name = "password"
        let uniq_name = "password"
        let persistant = false
    end
 
    module Login = RecordOf (ConsOf (FieldOf (LoginField))
                            (ConsOf (FieldOf (PasswdField))
                                    (NulType)))

    module StartField = struct
        module Type = InputOfDatatype(Timestamp)
        let display_name = "start"
        let uniq_name = "start"
        let persistant = true
    end
    module StopField = struct
        module Type = InputOfDatatype(Timestamp)
        let display_name = "stop"
        let uniq_name = "stop"
        let persistant = true
    end
    module VlanField = struct
        module Type = OptInputOfDatatype(Integer16)
        let display_name = "vlan"
        let uniq_name = "vlan"
        let persistant = false
    end
    module MacSrcField = struct
        module Type = OptInputOfDatatype(EthAddr)
        let display_name = "Eth src"
        let uniq_name = "eth-src"
        let persistant = false
    end
    module MacDstField = struct
        module Type = OptInputOfDatatype(EthAddr)
        let display_name = "Eth dest"
        let uniq_name = "eth-dest"
        let persistant = false
    end
    module EthProtoField = struct
        module Type = OptInputOfDatatype(Integer16)
        let display_name = "Eth proto"
        let uniq_name = "eth-proto"
        let persistant = false
    end
    module IpSrcField = struct
        module Type = OptInputOfDatatype(Cidr)
        let display_name = "IP src"
        let uniq_name = "ip-src"
        let persistant = false
    end
    module IpDstField = struct
        module Type = OptInputOfDatatype(Cidr)
        let display_name = "IP dst"
        let uniq_name = "ip-dst"
        let persistant = false
    end
    module IpField = struct
        module Type = OptInputOfDatatype(Cidr)
        let display_name = "IP src/dst"
        let uniq_name = "ip"
        let persistant = false
    end
    module IpProtoField = struct
        module Type = OptInputOfDatatype(Integer8)
        let display_name = "IP proto"
        let uniq_name = "ip-proto"
        let persistant = false
    end
    module L4PortField = struct
        module Type = OptInputOfDatatype(Integer16)
        let display_name = "port"
        let uniq_name = "port"
        let persistant = true
    end
    module TimeStepField = struct
        module Type = Integer (struct let min = Some 1 let max = None end)
        let display_name = "time step (ms)"
        let uniq_name = "tstep"
        let persistant = true
    end
    module GroupByField = struct
        module Type = Enum (struct
            let name = "key"
            let options = [| "port";"src-mac";"dst-mac";"src-ip";"dst-ip" |]
        end)
        let display_name = "group by"
        let uniq_name = "groupby"
        let persistant = false
    end
    module GroupByPeerField = struct
        module Type = Enum (struct let name = "key"
                                   let options = [| "mac";"ip" |] end)
        let display_name = "group by"
        let uniq_name = "groupby"
        let persistant = false
    end
    module GroupByTopField = struct
        module Type = Enum (struct
            let name = "key"
            let options = [| "port";"src-mac";"dst-mac";"mac (both)";"src-ip";"dst-ip";"ip (both)" |]
        end)
        let display_name = "group by"
        let uniq_name = "groupby"
        let persistant = false
    end
    module PlotWhat = struct
        module Type = Enum (struct let name = "y"
                                   let options = [| "volume";"packets" |] end)
        let display_name = "value"
        let uniq_name = "Y"
        let persistant = false
    end
    module MaxGraphsField = struct
        module Type = OptInteger (struct let min = Some 1 let max = Some 1000 end)
        let display_name = "#series"
        let uniq_name = "series"
        let persistant = false
    end
    module TxMin = struct
        module Type = OptInteger (struct let min = Some 1 let max = None end)
        let display_name = "#tx min"
        let uniq_name = "txmin"
        let persistant = false
    end
    module MinRespTime = struct
        module Type = OptFloat (struct let min = Some 0. let max = None end)
        let display_name = "min resp time (s)"
        let uniq_name = "minrt"
        let persistant = false
    end
    module MaxRespTime = struct
        module Type = OptFloat (struct let min = Some 0. let max = None end)
        let display_name = "max resp time (s)"
        let uniq_name = "maxrt"
        let persistant = false
    end
    module SortOrders = struct
        let name = "selection"
        let options = [| "min";"max" |]
    end
    module SortOrder = struct
        module Type = Enum (SortOrders)
        let display_name = "Selection"
        let uniq_name = "sort-order"
        let persistant = false
    end

    module Traffic = struct
        module TblNames = struct
            let name = "db-tables"
            let options = [| "1min";"10mins";"1hour" |]
        end
        module TblNameField = struct
            module Type = Enum (TblNames)
            let display_name = "DB table"
            let uniq_name = "db-table"
            let persistant = false
        end
        module Bandwidth = RecordOf (ConsOf (FieldOf (StartField))
                                    (ConsOf (FieldOf (StopField))
                                    (ConsOf (FieldOf (VlanField))
                                    (ConsOf (FieldOf (MacSrcField))
                                    (ConsOf (FieldOf (MacDstField))
                                    (ConsOf (FieldOf (EthProtoField))
                                    (ConsOf (FieldOf (IpSrcField))
                                    (ConsOf (FieldOf (IpDstField))
                                    (ConsOf (FieldOf (IpField))
                                    (ConsOf (FieldOf (IpProtoField))
                                    (ConsOf (FieldOf (L4PortField))
                                    (ConsOf (FieldOf (TimeStepField))
                                    (ConsOf (FieldOf (TblNameField))
                                    (ConsOf (FieldOf (PlotWhat))
                                    (ConsOf (FieldOf (GroupByField))
                                    (ConsOf (FieldOf (MaxGraphsField))
                                            (NulType)))))))))))))))))

        module Peers = RecordOf (ConsOf (FieldOf (StartField))
                                (ConsOf (FieldOf (StopField))
                                (ConsOf (FieldOf (VlanField))
                                (ConsOf (FieldOf (MacSrcField))
                                (ConsOf (FieldOf (MacDstField))
                                (ConsOf (FieldOf (EthProtoField))
                                (ConsOf (FieldOf (IpSrcField))
                                (ConsOf (FieldOf (IpDstField))
                                (ConsOf (FieldOf (IpField))
                                (ConsOf (FieldOf (IpProtoField))
                                (ConsOf (FieldOf (L4PortField))
                                (ConsOf (FieldOf (TblNameField))
                                (ConsOf (FieldOf (PlotWhat))
                                (ConsOf (FieldOf (GroupByPeerField))
                                (ConsOf (FieldOf (MaxGraphsField))
                                        (NulType))))))))))))))))

        module Tops = RecordOf (ConsOf (FieldOf (StartField))
                               (ConsOf (FieldOf (StopField))
                               (ConsOf (FieldOf (VlanField))
                               (ConsOf (FieldOf (MacSrcField))
                               (ConsOf (FieldOf (MacDstField))
                               (ConsOf (FieldOf (EthProtoField))
                               (ConsOf (FieldOf (IpSrcField))
                               (ConsOf (FieldOf (IpDstField))
                               (ConsOf (FieldOf (IpField))
                               (ConsOf (FieldOf (IpProtoField))
                               (ConsOf (FieldOf (L4PortField))
                               (ConsOf (FieldOf (TblNameField))
                               (ConsOf (FieldOf (PlotWhat))
                               (ConsOf (FieldOf (GroupByTopField))
                               (ConsOf (FieldOf (MaxGraphsField))
                                       (NulType))))))))))))))))

    end

    module Web = struct
        module TblNames = struct
            let name = "db-tables"
            let options = [| "queries";"1min";"10mins";"1hour" |]
        end
        module TblNameField = struct
            module Type = Enum (TblNames)
            let display_name = "DB table"
            let uniq_name = "db-table"
            let persistant = false
        end
        module HttpStatus = struct
            module Type = OptInteger (struct let min = Some 100 let max = Some 999 end)
            let display_name = "status"
            let uniq_name = "status"
            let persistant = false
        end
        module HttpHost = struct
            module Type = OptString (struct let min = None let max = None end)
            let display_name = "host"
            let uniq_name = "host"
            let persistant = false
        end
        module HttpURL = struct
            module Type = OptString (struct let min = None let max = None end)
            let display_name = "URL"
            let uniq_name = "url"
            let persistant = false
        end
        module HttpMethod = struct
            module Type = OptEnum (struct let name = "method" let options = Web.http_methods end)
            let display_name = "method"
            let uniq_name = "method"
            let persistant = false
        end

        (* TODO: Add HttpMethod *)
        module RespTime = RecordOf (ConsOf (FieldOf (StartField))
                                   (ConsOf (FieldOf (StopField))
                                   (ConsOf (FieldOf (VlanField))
                                   (ConsOf (FieldOf (MacSrcField))
                                   (ConsOf (FieldOf (MacDstField))
                                   (ConsOf (FieldOf (IpSrcField))
                                   (ConsOf (FieldOf (IpDstField))
                                   (ConsOf (FieldOf (HttpMethod))
                                   (ConsOf (FieldOf (HttpStatus))
                                   (ConsOf (FieldOf (HttpHost))
                                   (ConsOf (FieldOf (HttpURL))
                                   (ConsOf (FieldOf (MinRespTime))
                                   (ConsOf (FieldOf (MaxRespTime))
                                   (ConsOf (FieldOf (TimeStepField))
                                   (ConsOf (FieldOf (TblNameField))
                                           (NulType))))))))))))))))

        (* TODO: Add HttpMethod *)
        module Top = RecordOf (ConsOf (FieldOf (StartField))
                              (ConsOf (FieldOf (StopField))
                              (ConsOf (FieldOf (VlanField))
                              (ConsOf (FieldOf (MacSrcField))
                              (ConsOf (FieldOf (MacDstField))
                              (ConsOf (FieldOf (IpSrcField))
                              (ConsOf (FieldOf (IpDstField))
                              (ConsOf (FieldOf (HttpMethod))
                              (ConsOf (FieldOf (HttpStatus))
                              (ConsOf (FieldOf (HttpHost))
                              (ConsOf (FieldOf (HttpURL))
                              (ConsOf (FieldOf (MinRespTime))
                              (ConsOf (FieldOf (MaxRespTime))
                              (ConsOf (FieldOf (MaxGraphsField))
                              (ConsOf (FieldOf (SortOrder))
                                      (NulType))))))))))))))))

    end

    module Dns = struct
        module TblNames = struct
            let name = "db-tables"
            let options = [| "queries";"1min";"10mins";"1hour" |]
        end
        module TblNameField = struct
            module Type = Enum (TblNames)
            let display_name = "DB table"
            let uniq_name = "db-table"
            let persistant = false
        end
        module Error = struct
            module Type = OptInteger (struct let min = Some 0 let max = Some 255 end)
            let display_name = "Error code"
            let uniq_name = "err-code"
            let persistant = false
        end
        module QueryName = struct
            module Type = OptString (struct let min = None let max = None end)
            let display_name = "Query Name"
            let uniq_name = "qname"
            let persistant = false
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

        module Top = RecordOf (ConsOf (FieldOf (StartField))
                              (ConsOf (FieldOf (StopField))
                              (ConsOf (FieldOf (VlanField))
                              (ConsOf (FieldOf (MacSrcField))
                              (ConsOf (FieldOf (MacDstField))
                              (ConsOf (FieldOf (IpSrcField))
                              (ConsOf (FieldOf (IpDstField))
                              (ConsOf (FieldOf (MinRespTime))
                              (ConsOf (FieldOf (MaxRespTime))
                              (ConsOf (FieldOf (Error))
                              (ConsOf (FieldOf (QueryName))
                              (ConsOf (FieldOf (MaxGraphsField))
                              (ConsOf (FieldOf (SortOrder))
                                      (NulType))))))))))))))
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
                | Value start, (Value stop, (Value vlan, (Value mac_src, (Value mac_dst, (Value eth_proto, (Value ip_src, (Value ip_dst, (Value ip, (Value ip_proto, (Value port, (Value time_step, (Value tblname, (Value what, (Value group_by, (Value max_graphs, ()))))))))))))))) ->
                    let time_step = Int64.of_int time_step
                    and tblname = Forms.Traffic.TblNames.options.(tblname)
                    and what = if what = 0 then Volume else PacketCount in
                    let datasets = match group_by with
                        | 1 (* src-mac *) | 2 (* dst-mac *) as sd ->
                            eth_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs (sd = 0) what time_step dbdir tblname
                        | 3 (* src-ip *) | 4 (* dst-ip *) as sd ->
                            ip_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs (sd = 2) what time_step dbdir tblname
                        | _ (* default, apps *) ->
                            app_plot_vol_time start stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs what time_step dbdir tblname in
                    if datasets = [] then
                        [ cdata "No data" ]
                    else
                        let what = if what = PacketCount then "Packets" else "Bytes" in
                        View.bandwidth_chart ("Traffic - "^what^"/sec") time_step start datasets
                | _ -> [] in
            View.make_graph_page "Bandwidth" filters_form disp_graph

        let peers args =
            let filters = Forms.Traffic.Peers.from_args "filter" args in
            let filters_form = form "main/traffic/peers" (Forms.Traffic.Peers.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_src, (Value mac_dst, (Value eth_proto, (Value ip_src, (Value ip_dst, (Value ip, (Value ip_proto, (Value port, (Value tblname, (Value what, (Value group_by, (Value max_graphs, ())))))))))))))) ->
                    let tblname = Forms.Traffic.TblNames.options.(tblname)
                    and what = if what = 0 then Volume else PacketCount in
                    let datasets = match group_by with
                        | 0 (* mac *) ->
                            eth_plot_vol_tot ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs what dbdir tblname
                        | _ (* ip *) ->
                            ip_plot_vol_tot ~start ~stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_src ?ip_dst ?ip ?ip_proto ?port ?max_graphs what dbdir tblname in
                    if Hashtbl.is_empty datasets then
                        [ cdata "No data" ]
                    else
                        let units = if what = PacketCount then "Packets" else "Bytes" in
                        View.peers_chart "src" "dst" datasets units @
                        [ tag "hr" [] ] @
                        View.peers_table "src" "dst" datasets units
                | _ -> [] in
            View.make_graph_page "Peers" filters_form disp_graph

        let tops args =
            let filters = Forms.Traffic.Tops.from_args "filter" args in
            let filters_form = form "main/traffic/tops" (Forms.Traffic.Tops.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_src, (Value mac_dst, (Value eth_proto, (Value ip_src, (Value ip_dst, (Value ip, (Value ip_proto, (Value port, (Value tblname, (Value what, (Value group_by, (Value max_graphs, ())))))))))))))) ->
                    let tblname = Forms.Traffic.TblNames.options.(tblname)
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
                        [ cdata "No data" ]
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
            let filters_form = form "main/web/top" (Forms.Web.Top.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_clt, (Value mac_srv, (Value client, (Value server, (Value methd, (Value status, (Value host, (Value url, (Value rt_min, (Value rt_max, (Value n, (Value sort_order, ())))))))))))))) ->
                    let rt_min = BatOption.map s2m rt_min
                    and rt_max = BatOption.map s2m rt_max
                    and n = BatOption.default 30 n
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
            let filters_form = form "main/web/resptime" (Forms.Web.RespTime.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_clt, (Value mac_srv, (Value client, (Value server, (Value methd, (Value status, (Value host, (Value url, (Value rt_min, (Value rt_max, (Value time_step, (Value tblname, ())))))))))))))) ->
                    let time_step = Int64.of_int time_step
                    and tblname = Forms.Web.TblNames.options.(tblname) in
                    let rt_min = BatOption.map s2m rt_min
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
            let filters_form = form "main/DNS/top" (Forms.Dns.Top.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_clt, (Value mac_srv, (Value client, (Value server, (Value rt_min, (Value rt_max, (Value error, (Value qname, (Value n, (Value sort_order, ())))))))))))) ->
                    let rt_min = BatOption.map s2m rt_min
                    and rt_max = BatOption.map s2m rt_max
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
            let filters_form = form "main/DNS/resptime" (Forms.Dns.RespTime.edit "filter" filters) in
            let disp_graph = match filters with
                | Value start, (Value stop, (Value vlan, (Value mac_clt, (Value mac_srv, (Value client, (Value server, (Value tx_min, (Value rt_min, (Value rt_max, (Value time_step, (Value tblname, ()))))))))))) ->
                    let time_step = Int64.of_int time_step
                    and tblname = Forms.Dns.TblNames.options.(tblname) in
                    let rt_min = BatOption.map s2m rt_min
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
        | ["main";"web";"top"] ->
            Ctrl.ensure_logged Ctrl.Web.top
        | ["main";"DNS";"resptime"] ->
            Ctrl.ensure_logged Ctrl.Dns.resp_time
        | ["main";"DNS";"top"] ->
            Ctrl.ensure_logged Ctrl.Dns.top
        | _ -> Ctrl.Invalid.run)

