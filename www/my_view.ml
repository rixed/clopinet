module StdScanf = Scanf (* opening Batteries will somewhat overwrite the stdlib exception... *)
open Batteries

open Html
include View

(* Misc *)

let string_of_vlan = function
    | None -> ""
    | Some v -> string_of_int v

let multiples_bytes = [| ""; "KiB"; "MiB"; "GiB"; "TiB"; "PiB" |]
let multiples = [| ""; "k"; "M"; "G"; "T"; "P" |]
let unpower v =
    let rec aux e v =
        if e >= Array.length multiples -1 || v < 1024. then
            e, v
        else
            aux (succ e) (v/.1024.) in
    aux 0 v
let string_of_volume v =
    let e, v = unpower v in
    Printf.sprintf "%.2f %s" v multiples_bytes.(e)
let string_of_number v =
    let e, v = unpower v in
    Printf.sprintf "%.2f %s" v multiples.(e)

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
    and menu_entries = [ "Traffic", ["bandwidth"; "peers"; "tops"; "graph"] ;
                         "DNS", ["resptime"; "top"] ;
                         "Web", ["resptime"; "top"] ;
                         "Admin", ["logout"] ] in
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
let svg_width = 800. and svg_height = 600.

let peers_chart ?(is_bytes=false) datasets =
    let string_of_val = if is_bytes then string_of_volume else string_of_number in
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
    [ table ~attrs:["class","peers"] [ tr [ td
        [ svg ~width:svg_height ~height:svg_height
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
                             "onmouseover","peer_select(evt, '"^p^"', '"^string_of_volume tot_up^"', '"^string_of_volume tot_down^"')" ;
                             "onmouseout", "peer_unselect(evt)" ]
                     [ text ~attrs:["class",class_list ; "id",pclass] ~style:("text-anchor:"^anchor^"; dominant-baseline:central")
                            ~font_size:15. ~fill:"#444" ~stroke:"#444" ~stroke_width:0. p ]
                 ) peers) ] ] ;
        td
            [ div ~attrs:["class","peers-info"]
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
            (g [ rect ~fill:col (x-.w/.2.) (y-.h/.2.) w h ;
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
           That's why we introduce this "netgraph" div here, so that we can
           reliably get SVG's bounding box position (this works iif the div
           box actually stick to svg's boundary). *)
        [ div ~id:"netgraph"
          [ svg
              [ g ~id:"scaler" ~attrs:[ "transform","scale(1)" ]
                  [ g svg_edges ;
                    g ~attrs:["style","text-anchor:middle; dominant-baseline:central" ;
                              "font-size", Html.string_of_float font_size]
                      svg_nodes ] ] ] ;
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


