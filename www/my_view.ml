module StdScanf = Scanf (* opening Batteries will somewhat overwrite the stdlib exception... *)
open Batteries
module Timestamp = Datatype.Timestamp
module UInteger64 = Datatype.UInteger64

open Html
include View

(* notification system *)

let msgs = ref []

let add_msg_with_class c txt =
    msgs := (p ~attrs:["class",c] [cdata txt]) :: !msgs
let add_msg = add_msg_with_class "ok"
let add_err = add_msg_with_class "nok"

let msgs () = div ~id:"notifs" !msgs

(* Google charts *)

let chart_head = [
    tag "script" ~attrs:["type","text/javascript" ; "src","https://www.google.com/jsapi"] [] ;
    tag "script" ~attrs:["type","text/javascript" ; "src","static/js/graph.js"] [] ;
    tag "script" ~attrs:["type","text/javascript"]
        [ cdata "graph_init();" ]
]

let chart_div = div ~attrs:["id","chart_div"] []

(* rendering of pages *)

let header () =
    div ~id:"header" []

let menu () =
    let html_of_entry e1 e2 = tag "li" [ tag "a" ~attrs:["href","?action="^e1^"/"^e2] [cdata e2] ]
    and menu_entries = [ "Traffic", ["bandwidth"; "peers"; "tops"; "graph"; "callflow"] ;
                         "DNS", ["resptime"; "top"] ;
                         "Web", ["resptime"; "top"] ;
                         "Admin", ["preferences"] ] in
    span ~id:"menu" [
        tag "ul" (List.map (fun (section, links) ->
            tag "li" [ p [ raw section ] ;
                       tag "ul" (List.map (html_of_entry section) links) ])
            menu_entries) ]

(* add the menu *)
let make_app_page content =
    let body = [ header () ;
                 menu () ;
                 msgs () ;
                 tag "div" ~attrs:["id","page"] content ]
    and head = [ title "MlRRD" ;
                 link_css "static/css/style.css" ;
                 link_css "http://fonts.googleapis.com/css?family=BenchNine:300|Anaheim" ] @
               chart_head in
    html head body

let make_graph_page title form graph =
    let content =
        [ h1 title ;
          div ~id:"filter"
            [ div ~id:"filter-form" [ form ] ;
              div ~id:"filter-handle" [] ] ;
          div ~id:"data" graph ;
          if graph = [] then raw "No data" else
          script "\n\
              var filter = document.getElementById('filter');\n\
              var handle = document.getElementById('filter-handle');\n\
              var form = document.getElementById('filter-form');\n\
              var data = document.getElementById('data');\n\
              function show() {\n\
                  form.style.width = null;\n\
                  form.style.visibility = 'visible';\n\
              }\n\
              function hide() {\n\
                  form.style.width = '0';\n\
                  form.style.visibility = 'hidden';\n\
              }\n\
              hide();\n\
              filter.addEventListener('mouseover', show, false);\n\
              data.addEventListener('mouseover', hide, false);\n\
          " ] in
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
      script ("var data = new google.visualization.DataTable(" ^ js ^ ");\n\
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
chart.draw(data, options);\n") ]

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
      script ("var data = new google.visualization.DataTable(" ^ js ^ ");\n\
var options = {\n\
showRowNumber:true,\n\
sortColumn:1,\n\
sortAscending:false\n\
};\n\
var chart = new google.visualization.Table(document.getElementById('chart_div'));\n\
chart.draw(data, options);\n") ]

let color_scale =
    [ 0.0, [| 0.0; 0.5; 1.0 |] ;
      0.2, [| 0.2; 0.8; 1.0 |] ;
      0.5, [| 0.0; 1.0; 0.7 |] ;
      0.7, [| 0.9; 0.8; 0.4 |] ;
      1.0, [| 1.0; 0.4; 0.3 |] ]
let pi = BatFloat.pi
let to_deg rad = 180. *. rad /. pi
let svg_width = 1000. and svg_height = 800. (* Should be taken from user prefs *)

let peers_chart ?(is_bytes=false) datasets =
    let string_of_val = if is_bytes then Datatype.string_of_volume else Datatype.string_of_number in
    let inner_rad = 0.3 *. svg_height in
    let inner_x = svg_height/.2. and inner_y = svg_height/.2. in
    let inner r x y = (x -. inner_x) *. r +. inner_x,
                      (y -. inner_y) *. r +. inner_y in
    let x_of_ang ang = inner_rad *. cos ang +. inner_x
    and y_of_ang ang = inner_rad *. sin ang +. inner_y in
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
    let opacity w = 0.05 +. 0.95 *. w in
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
    [ table ~attrs:["class","svg"] [ tr [ td
        [ svg
            [ (* Traffic *)
              g (Hashtbl.fold (fun (p1, p2) v l ->
                 try (
                     let _, c1, a1 = get_by_name p1
                     and _, c2, a2 = get_by_name p2 in
                     let x1, y1 = x_of_ang a1, y_of_ang a1
                     and x2, y2 = x_of_ang a2, y_of_ang a2 in
                     let close r a = (* return x,y of a point at radius r and at ang a2+da *)
                        inner r (x_of_ang a) (y_of_ang a) in
                     let c1x, c1y = if p1 <> p2 then inner 0.1 x1 y1
                                    else close 0.6 (a1+.0.6)
                     and c2x, c2y = if p1 <> p2 then inner 0.8 x2 y2
                                    else close 0.6 (a1-.0.6) in
                     let w = v /. !max_volume in
                     let w'= 0.3 +. 0.7 *. v /. !max_volume in
                     let d = moveto (close 0.97 (a1+.0.08*.w')) ^
                             curveto (c1x, c1y) (c2x, c2y) (close 0.97 a2) ^
                             curveto (c2x, c2y) (c1x, c1y) (close 0.97 (a1-.0.08*.w')) ^
                             closepath in
                     let col = color w and opac = opacity w in
                     (g [ path ~attrs:["class",c1^" "^c2] ~fill:col ~stroke:col ~stroke_opacity:opac ~stroke_width:0. ~fill_opacity:opac d ]) :: l
                 ) with Not_found -> l)
                 datasets []) ;
              (* The peers *)
              g (List.map (fun (p, pclass, a) ->
                let x, y = x_of_ang a, y_of_ang a in
                let class_list, tot_up, tot_down = Hashtbl.fold (fun (p1, p2) v ((tot_class, up, down) as l) ->
                    if p = p1 then
                        let _, c, _ = get_by_name p2 in tot_class^" "^c, up +. v, down
                    else if p = p2 then
                        let _, c, _ = get_by_name p1 in tot_class^" "^c, up, down +. v
                    else l) datasets (pclass, 0., 0.) in
                let a' = (mod_float (a +. pi/.2.) pi) -. pi/.2. in
                let anchor = if a < 0.5*.pi || a > 1.5*.pi then "start" else "end" in
                   g ~attrs:["transform","translate("^string_of_float x^","^string_of_float y^") "^
                                         "rotate("^string_of_float (to_deg a') ^")" ;
                             "onmouseover","peer_select(evt, '"^p^"', '"^Datatype.string_of_volume tot_up^"', '"^Datatype.string_of_volume tot_down^"')" ;
                             "onmouseout", "peer_unselect(evt)" ]
                     [ text ~attrs:["class",class_list ; "id",pclass] ~style:("text-anchor:"^anchor^"; dominant-baseline:central")
                            ~font_size:15. ~fill:"#444" ~stroke:"#444" ~stroke_width:0. p ]
                 ) peers) ] ] ;
        td
            [ div ~attrs:["class","svg-info"]
                [ h2 (string_of_val !tot_volume) ;
                  h3 (Printf.sprintf "%.1f%% of total"
                       (100.*. !tot_volume /. (other_volume +. !tot_volume))) ;
                  h4 ~id:"selected-peer-name" "" ;
                  p ~id:"selected-peer-info" [] ] ] ] ] ]

let peers_graph datasets layout =
    (* Get max volume *)
    let max_volume = Hashtbl.fold (fun _k1 n m ->
        Hashtbl.fold (fun _k2 y m ->
            max y m) n m) datasets 0. in
    let weight w = 1. +. 2. *. w in
    let color w = Color.get color_scale w in
    let out, inp = Unix.open_process "dot -Tplain" in (* dot outputs HTML header :-< *)
    let font_size_pt = 11 in
    Printf.fprintf inp "graph network {\n\
node [shape=box,height=0.2];\n\
graph [fontsize=%d];\n\
overlap=scale;\n\
layout=\"%s\";\n"
    font_size_pt layout ;
    Hashtbl.iter (fun k1 n ->
        Hashtbl.iter (fun k2 y ->
            let w = y /. max_volume in
            Printf.fprintf inp "\t\"%s\" -- \"%s\" [weight=%f];\n"
                k1 k2 (weight w))
            n)
        datasets ;
    Printf.fprintf inp "}\n" ;
    IO.flush inp ;
    let ic = IO.to_input_channel out in
    try let scale, width, height =
            Scanf.fscanf ic "graph %f %f %f\n" (fun scale width height ->
                scale, width, height) in
        (* FIXME: svg_width and svg_height should be set according to width and height to preserve aspect ratio *)
        let dot_2_svg x y =
            svg_width *. x *. scale /. width,
            svg_height *. y *. scale /. height in
        (* if 72points is an inch, then our fontsize is .11111 inch *)
        let font_size, _ = dot_2_svg (float_of_int font_size_pt /. 72.) 0.0 in
        let node_pos = Hashtbl.create 71 in
        (try while true do
            Scanf.fscanf ic "node \"%s@\" %f %f %f %f %_s %_s %_s %_s %_s\n" (fun name x y w h ->
                let x, y = dot_2_svg x y
                and w, h = dot_2_svg w h in
                Hashtbl.add node_pos name (x, svg_height -. y, w, h))
            done
        with StdScanf.Scan_failure _ -> ()) ;
        let pos_of n =
            let x,y,_w,_h = Hashtbl.find node_pos n in
            x,y in
        IO.close_out inp ; (* cleans everything *)
        let svg_nodes = Hashtbl.fold (fun n (x,y,w,h) p ->
            let is_mac = try Scanf.sscanf n "%[0-9a-f]:" ignore ; true
                         with StdScanf.Scan_failure _ -> false in
            let col = if is_mac then "#888" else "#5c8" in
            (g ~attrs:[ "onmouseover","node_select(evt, '"^n^"')" ;
                        "onmouseout","node_unselect(evt)" ]
               [ rect ~fill:col (x-.w/.2.) (y-.h/.2.) w h ;
                 text ~x ~y n ]) ::p)
            node_pos [] in
        let svg_edges = Hashtbl.fold (fun k1 n p ->
            Hashtbl.fold (fun k2 y p ->
                let w = y /. max_volume in
                let col = color w and sw = 0.5 *. font_size in
                (g [line ~stroke:col ~stroke_width:sw (pos_of k1) (pos_of k2)
                    (*path ~stroke:col ~stroke_width:sw
                         (moveto (pos_of k1) ^ lineto (pos_of k2))*)])::p)
                n p) datasets [] in
        (* FF seams confused by svg when implementing getBoundingClientRect.
           That's why we introduce this "netgraph" div here (actualy now a td),
           so that we can reliably get SVG's bounding box position (this works
           iif the div box actually stick to svg's boundary). *)
        [ table ~attrs:["class","svg"] [ tr
            [ td ~id:"netgraph"
                [ svg 
                    [ g ~id:"scaler" ~attrs:[ "transform","scale(1)" ]
                        [ g svg_edges ;
                          g ~attrs:["style","text-anchor:middle; dominant-baseline:central" ;
                                    "font-size", Html.string_of_float font_size]
                            svg_nodes ] ] ] ;
              td [ div ~attrs:["class","svg-info"]
                    [ h2 (Printf.sprintf "%d nodes" (Hashtbl.length node_pos)) ;
                      h4 ~id:"selected-peer-name" "" ;
                      p ~id:"selected-peer-info" [] ] ] ] ] ;
          script "svg_explorer('netgraph', 'scaler');" ]
    with End_of_file ->
        [ raw "dot crashed" ]

let peers_table ?(is_bytes=false) lab1 lab2 datasets =
    let units = if is_bytes then "Bytes" else "Packets" in
    let js =
        let os = IO.output_string () in
        Printf.fprintf os "{\ncols: [{label: '%s', type: 'string'},{label: '%s', type: 'string'},{label: '%s', type: 'number'}],\nrows: %a\n"
            lab1 lab2 units
            (* display the rows *)
            (Hashtbl.print ~first:"[" ~last:"]" ~sep:",\n" ~kvsep:","
                           (fun oc (s, d) -> Printf.fprintf oc "{c:[{v:'%s'},{v:'%s'}" s d)
                           (fun oc y -> Printf.fprintf oc "{v:%f}]}" y))
            datasets ;
        Printf.fprintf os " \n}\n" ;
        IO.close_out os in
    [ chart_div ;
      script ("var data = new google.visualization.DataTable(" ^ js ^ ");\n\
var options = {\n\
width:'100%',\n\
height:600,\n\
showRowNumber:true,\n\
sortColumn:2,\n\
sortAscending:false\n\
};\n\
var chart = new google.visualization.Table(document.getElementById('chart_div'));\n\
chart.draw(data, options);\n") ]

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
      script ("var data = new google.visualization.DataTable(" ^ js ^ ");\n\
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
chart.draw(data, options);\n") ]

(* Dataset is a list of (ts1, ts2, peer1, peer2, descr, group), where group is used for coloring *)
let callflow_chart (datasets : Flow.callflow_item list) =
    let left_margin = 50. and peer_width = 100.
    and first_ts = ref Int64.max_int and last_ts = ref Int64.zero
    and bw_max = ref 0. and tot_svg_height = ref svg_height
    and min_dt = ref Int64.max_int in
    (* Build the hash of all peers and set their X location and time range *)
    let peers = Hashtbl.create 71 (* ip -> (ref x, ts_min, ts_max) *) in
    let y_of_ts ts =
        let dt = Timestamp.sub !last_ts !first_ts |> Int64.to_float
        and y = Timestamp.sub ts !first_ts |> Int64.to_float in
        (* scale from 0 to tot_svg_height *)
        (y *. !tot_svg_height) /. dt in
    let x_ref_of_peer ip =
        let x, _, _ = Hashtbl.find peers ip in x in
    let x_of_peer ip =
        try !(x_ref_of_peer ip)
        with Not_found -> (* An IP mentionned in a Tx but not in a Dt, clip it *) -1. in
    let update_peer ip ts1 ts2 =
        if Timestamp.compare ts1 !first_ts < 0 then first_ts := ts1 ;
        if Timestamp.compare ts2 !last_ts > 0 then last_ts := ts2 ;
        let dt = Timestamp.sub ts2 ts1 in
        if dt > 0L && dt < !min_dt then min_dt := dt ;
        match Hashtbl.find_option peers ip with
        | None ->
            Hashtbl.add peers ip (ref 0., ts1, ts2) ;
        | Some (x, ts1',ts2') ->
            Hashtbl.replace peers ip (x, Timestamp.min ts1 ts1', Timestamp.max ts2 ts2') in
    let tot_volume = ref 0. in
    let update_bw ts1 ts2 vol =
        tot_volume := !tot_volume +. vol ;
        if ts2 > ts1 then
            let bw = vol /. (Timestamp.sub ts2 ts1 |> Int64.to_float) in
            if bw > !bw_max then bw_max := bw in
    let nb_flows = ref 0 in
    let senders_to = Hashtbl.create 31 in (* while we are at it, we build this hash of peer -> senders (as string, for CSS class) *)
    let update_senders ip1 ip2 =
        match Hashtbl.find_option senders_to ip2 with
        | None -> Hashtbl.add senders_to ip2 (ip1^" ")
        | Some str -> Hashtbl.replace senders_to ip2 (str^ip1^" ") in
    List.iter (function
        | ts1,ts2,ip1,ip2,_,_,Flow.Dt vol ->
            update_peer ip1 ts1 ts2 ;
            update_peer ip2 ts1 ts2 ;
            update_bw ts1 ts2 vol ;
            incr nb_flows ;
            update_senders ip1 ip2
        (* We want to scale graph according to flows, mostly *)
        | _ -> ())
        datasets ;
    (* Now take into account Tx if they are within range *)
    List.iter (function
        | ts1,ts2,ip1,ip2,_,_,Flow.Tx _ ->
            if Timestamp.compare ts1 !first_ts >= 0 &&
               Timestamp.compare ts2 !last_ts <= 0 then (
                update_peer ip1 ts1 ts2 ;
                update_peer ip2 ts1 ts2 ;
                incr nb_flows
            )
        | _ -> ())
        datasets ;
    (* Scale in Y for a reasonable event density *)
    let min_dt_pix = 30. in
    let h = ((Int64.to_float (Timestamp.sub !last_ts !first_ts)) *. min_dt_pix) /.
            Int64.to_float !min_dt in
    tot_svg_height := max svg_height h ;
    (* Find out x of peers, ordering them from taller to smaller *)
    let peer_durations : (float ref * Timestamp.t) array =
        Hashtbl.enum peers /@
        (fun (_ip, (x, ts_min, ts_max)) -> x, Timestamp.sub ts_max ts_min) |>
        Array.of_enum in
    Array.sort (fun (_x1, dt1) (_x2, dt2) ->
        ~- (Timestamp.compare dt1 dt2))
        peer_durations ;
    Array.iteri (fun i (x, _dt) ->
        x := left_margin +. float_of_int i *. peer_width) peer_durations ;
    (* Show timeline for each peer (TODO: better allocation of X space) *)
    let peer_y_padding = 20. in
    let svg_grid =
        let mark_margin = 999999. in
        let nb_marks = (int_of_float !tot_svg_height) / 200 in (* approx one mark every 200 pixs *)
        Plot.grid nb_marks (Int64.to_float !first_ts) (Int64.to_float !last_ts) |>
        Enum.map (fun y ->
            let t = Int64.of_float y in
            let y = y_of_ts t in
            g [ line ~stroke:"#bbb" ~stroke_width:2. (-.mark_margin, y) (peer_width *. (float_of_int (Hashtbl.length peers)) +. mark_margin, y) ;
                text ~x:0. ~y:(y-.9.) ~style:("text-anchor:end") ~font_size:10. (Timestamp.to_string t) ]) |>
        List.of_enum
    and svg_peers =
        (* A set of columns, one for each IP *)
        Hashtbl.fold (fun ip (x, ts1, ts2) p ->
            let y1 = y_of_ts ts1 -. peer_y_padding and y2 = y_of_ts ts2 +. peer_y_padding in
            (
                let senders = Hashtbl.find_option senders_to ip |? "" in
                g ~attrs:[ "onmouseover","timeline_select(evt, '"^ip^"')" ;
                           "onmouseout", "timeline_unselect(evt, '"^ip^"')" ]
                    [ line ~attrs:["stroke-dasharray","5,5"; "class","fitem "^senders^ip ]
                           ~stroke:"#000" ~stroke_width:2. (!x, y1) (!x, y2) ;
                      text ~attrs:["class","fitem "^senders^ip ] 
                           ~x:!x ~y:(y1-.10.)
                           ~style:("text-anchor:middle; dominant-baseline:central")
                           ~font_size:13.
                           ip ;
                      (* this one to attract mouse events *)
                      let w = peer_width *. 0.05 in
                      rect ~attrs:["opacity","0"] (!x -. w) y1 (2.*.w) (y2-.y1) ]
            ) :: p)
            peers []
    and svg_flows =
        (* flows from source to dest *)
        let dw = 1. (* otherwise arrow of 0 width would looks ugly *) in
        List.map (fun (ts1, ts2, ip_s, ip_d, descr, group, spec) ->
            let y1 = y_of_ts ts1 and y2 = y_of_ts ts2
            and x1 = x_of_peer ip_s and x2 = x_of_peer ip_d in
            let dx = abs_float (x1 -. x2) and dt = y2 -. y1 in
            let hx = min (peer_width *. 0.2) (peer_width *. 0.05 +. dt *. 0.1) in
            let hy = hx *. 0.5 in
            let hx = if x2 >= x1 then hx else -.hx in
            let inclin = min (0.8 *. peer_y_padding) (hy *. (0.001 *. dx))
            and fill = Color.(random_of_string group |> to_html) in
            match spec with
            Flow.Dt vol ->
                let fill_opacity = (* proportionnal to bandwidth *)
                    if dt > 0. then
                        let bw = vol /. dt in 0.1 +. 0.5 *. (bw /. !bw_max)
                    else
                        0.5 in
                g [ path ~attrs:["class","fitem "^ip_s^" "^ip_d] ~stroke:fill ~stroke_width:0.5 ~stroke_opacity:0.7 ~fill_opacity ~fill (
                       moveto (x1, y1 -. dw -. inclin) ^
                       lineto (x2 -. hx, y1 -. dw +. inclin) ^
                       lineto (x2 -. hx, y1 -. dw -. hy +. inclin) ^
                       lineto (x2, y1 +. inclin) ^
                       lineto (x2, y2 +. inclin) ^
                       lineto (x2 -. hx, y2 +. dw +. hy +. inclin) ^
                       lineto (x2 -. hx, y2 +. dw +. inclin) ^
                       lineto (x1, y2 +. dw -. inclin) ^
                       closepath
                    ) ;
                    text ~attrs:["class","fitem "^ip_s^" "^ip_d] ~x:((3.*.x1+.x2)*.0.25) ~y:(y1-.5.) ~style:("text-anchor:middle; dominant-baseline:central") ~font_size:6.
                         descr ]
            | Flow.Tx resp ->
                let is_visible x y = x >= 0. && y >= 0. && y <= !tot_svg_height in
                if is_visible x1 y1 && is_visible x2 y2 then (
                    let hx = hx *. 0.5 in
                    let mid_x = (x1 +. x2)*.0.5 in
                    (* Draw query + resp *)
                    g [ path ~attrs:["class","fitem "^ip_s] ~stroke:fill ~stroke_width:1.5 ~stroke_opacity:0.7 ~fill_opacity:0. (
                            moveto (x1, y1 -. dw -. inclin) ^
                            lineto (mid_x, y1 -. dw) ^
                            curveto (x2, y1 -. dw +. inclin) (x2, y1 -. dw +. inclin) (x2, (y1 +. y2)*.0.5) ^
                            curveto (x2, y2 +. dw +. inclin) (x2, y2 +. dw +. inclin) (mid_x, y2 +. dw) ^
                            lineto (x1, y2 +. dw -. inclin) ^
                            lineto (x1 +. hx, y2 +. dw -. inclin -. hx) ^
                            moveto (x1, y2 +. dw -. inclin) ^
                            lineto (x1 +. hx, y2 +. dw -. inclin +. hx)
                        ) ;
                        text ~attrs:["class","fitem "^ip_s]  ~x:(x1+.hx) ~y:(y1-.dw-.inclin+.5.) ~style:("text-anchor:left") ~font_size:8. descr ;
                        text ~attrs:["class","fitem "^ip_s] ~x:(x1+.hx) ~y:(y2+.dw-.inclin-.3.) ~style:("text-anchor:left") ~font_size:8. resp
                    ]
                ) else if is_visible x1 y1 then (
                    g []
                ) else if is_visible x2 y2 then (
                    g []
                ) else g []
        ) datasets in
    [ table ~attrs:["class","svg"] [ tr
        [ td ~id:"callflow"
            [ svg
              [ g ~id:"scaler" ~attrs:[ "transform","scale(1)" ]
                  [ g svg_grid ;
                    g svg_flows ;
                    g svg_peers ] ] ] ;
          td [ div ~attrs:["class","svg-info"]
                [ h2 (Datatype.string_of_volume !tot_volume) ;
                  h3 (Printf.sprintf "%d hosts" (Hashtbl.length peers)) ;
                  h4 ~id:"selected-peer-name" "" ;
                  p ~id:"selected-peer-info" [] ] ] ] ] ;
      script "svg_explorer('callflow', 'scaler');" ]

