open Batteries
open Metric
module Timestamp = Datatype.Timestamp

open Html
include View

(* notification system *)

let msgs = ref []

let add_msg_with_class c txt =
    msgs := (div ~attrs:["class",c] txt) :: !msgs
let add_msg = add_msg_with_class "ok"
let add_err = add_msg_with_class "nok"
let add_exc exc = add_err [ cdata (Printexc.to_string exc) ; tag "br" [] ; pre [raw (Printexc.get_backtrace ())] ]

let chart_head = [
    tag "script" ~attrs:["type","text/javascript" ; "src","static/js/graph.js"] []
]

let msgs () = div ~id:"notifs" !msgs

(* rendering of pages *)

let header () =
    div ~id:"header" []

let menu menu_entries =
    let html_of_entry e1 e2 = tag "li" [ tag "a" ~attrs:["href","?action="^e1^"/"^e2] [cdata e2] ] in
    span ~id:"menu" [
        tag "ul" (List.map (fun (section, links) ->
            tag "li" [ p [ tag "a" ~attrs:["href","?action="^section^"/help"] [ cdata section ] ] ;
                       tag "ul" (List.map (html_of_entry section) links) ])
            menu_entries) ]

(* add the menu *)
let make_app_page menu_entries content =
    let body = [ header () ;
                 menu menu_entries ;
                 msgs () ;
                 div ~cls:"page" content ]
    and head = [ title "ClopiNet" ;
                 Block (match Prefs.get_option "CPN_GUI_BASEHREF" with
                        | Some base_path ->
                            [ tag "base" ~attrs:[ "href", base_path ] [] ]
                        | None -> []) ;
                 tag "link" ~attrs:[ "rel", "shortcut icon" ;
                                     "href", "static/img/favicon.svg" ;
                                     "type", "image/svg+xml" ] [] ;
                 link_css "static/css/style.css" ;
                 link_css "http://fonts.googleapis.com/css?family=BenchNine:300|Anaheim" ;
                 link_css "http://cdn.leafletjs.com/leaflet-0.4/leaflet.css" ;
                 raw "<!--[if lte IE 8]>\n\
                      <link rel=\"stylesheet\" href=\"http://cdn.leafletjs.com/leaflet-0.4/leaflet.ie.css\" />\n\
                      <![endif]-->\n" ] @
                 chart_head
    in
    [ html head body ]

(* remove all html elements which class match the given one *)
let remove_class name doc =
    let rec aux = function
        | Tag (t, a, s) :: rest ->
            if AList.(mem a "class" ~value_match:(has_word name)) then
                rest
            else
                Tag (t, a, aux s) :: aux rest
        | Block h :: rest ->
            Block (aux h) :: aux rest
        | x :: rest -> x :: aux rest
        | [] -> [] in
    aux doc

let make_app_page_for_email url content =
    let wwwdir = Prefs.get_string "CPN_GUI_INLINER_WWWDIR" "www" in
    make_app_page [] (content @ [ p [ tag "a" ~attrs:["href",url] [ cdata "View in browser" ] ] ]) |>
    Inliner.filter wwwdir |>
    remove_class "interactive" (* elements in this class must not be present in emails. *)

let make_filter title help form =
    [ h1 title ;
      div ~cls:"filter" ~attrs:["style", "position: static; float: left; margin-right: 2em;"]
        [ div ~cls:"filter-form" [ form ] ;
          div ~cls:"filter-handle" [] ] ;
      div ~cls:"help" help ]

let random_id () =
    let chars = "abcdefghijklmnopqrstuvwxyz_" in
    String.init 7 (fun _ -> chars.[Random.int (String.length chars)])

let make_chart title form graph =
    let id = random_id () in
    [ h1 title ;
      div ~attrs:["style","position:relative"] [
          div ~cls:"filter" ~id
            [ div ~cls:"filter-form" ~id:(id^"-form") [ form ] ;
              div ~cls:"filter-handle" [] ] ;
          div ~cls:"data" ~id:(id^"-data") graph ] ;
      if graph = [] then p ~cls:"no-data" [ cdata "No data" ] else
      script ("\n\
          var filter_"^id^" = document.getElementById('"^id^"');\n\
          var form_"^id^" = document.getElementById('"^id^"-form');\n\
          var data_"^id^" = document.getElementById('"^id^"-data');\n\
          function show_"^id^"() {\n\
              form_"^id^".style.width = null;\n\
              form_"^id^".style.visibility = 'visible';\n\
          }\n\
          function hide_"^id^"() {\n\
              form_"^id^".style.width = '0';\n\
              form_"^id^".style.visibility = 'hidden';\n\
          }\n\
          hide_"^id^"();\n\
          filter_"^id^".addEventListener('mouseover', show_"^id^", false);\n\
          data_"^id^".addEventListener('mouseover', hide_"^id^", false);\n\
      ") ]

let make_report_page page_no title descr chart =
    [ h2 (string_of_int (succ page_no) ^": "^
          BatOption.default "notitle" title) ;
      (match descr with None -> span [] | Some txt -> h3 txt) ;
      div ~attrs:["class","report_page"] chart ]

let disp_name_of_field fields k =
    let descr = List.assoc k fields in
    let n = descr.Datatype.disp_name in
    match descr.Datatype.valunit with
    | None -> n
    | Some u -> n ^" ("^ u ^")"

let table_of_datasets fields key_fields aggr_fields sort_field (tbl, sum_v, sum_tv) =
    let tds_of_arr a =
        Array.enum a /@
        (fun k -> td [ raw k ]) |>
        List.of_enum in
    let tds_of_key = function
        | None -> [ td ~attrs:[ "colspan", List.length key_fields |> string_of_int ;
                                "class","rest" ]
                       [ cdata "others" ] ]
        | Some ks -> tds_of_arr ks in
    let all_rows =
        let lineno = ref 0 in
        List.map (fun (key, aggrs, sort_v_min, sort_v_max) ->
            let style = if !lineno land 1 = 0 then "even" else "odd" in
            incr lineno ;
            tr ~attrs:["class",style]
               (tds_of_key key @
                tds_of_arr aggrs @
                [ td ~attrs:["class","sort_by"]
                     [ raw (Datatype.string_of_min_max (float_of_int sort_v_min) (float_of_int sort_v_max)) ] ]))
            tbl @
        [ tr ~attrs:["class","total"]
            ([ th ~attrs:[ "colspan", List.length key_fields |> string_of_int ;
                           "class","total" ]
                  [ cdata "total" ] ] @
             tds_of_arr sum_tv @
             [ td ~attrs:["class","sort_by"]
                  [ raw (Datatype.string_of_number (float_of_int sum_v)) ] ]) ]
    and headers =
        let ths_of l =
            List.map (fun s -> th [ cdata s ]) l in
        let disp_name_of_aggr f =
            let field, aggr = String.split f "." in
            aggr ^" of "^ disp_name_of_field fields field in
        ths_of (List.map (disp_name_of_field fields) key_fields) @
        ths_of (List.map disp_name_of_aggr aggr_fields) @
        [ th ~attrs:["class","sort_by"]
             [ cdata (disp_name_of_field fields sort_field) ] ]
    in
    [ table ~attrs:["class","tops"]
            (headers @ all_rows) ]

let tops_table tops heads vals_of_top =
    let all_rows =
        let lineno = ref 0 in
        Array.fold_left (fun rows top ->
            match top with None -> rows
                         | Some top ->
                              let vals = vals_of_top top in
                              let style = if !lineno land 1 = 0 then "even" else "odd" in
                              incr lineno ;
                              tr ~attrs:["class",style]
                                 (List.map (fun v -> td [ raw v ]) vals) ::
                              rows)
            [] tops |>
        List.rev in
    let heads = thead [ tr (List.map (fun v -> th [ raw v ]) heads) ] in
    [ table ~attrs:["class","tops"] (heads :: all_rows) ]

let color_scale =
    [ 0.0, [| 0.0; 0.5; 1.0 |] ;
      0.2, [| 0.2; 0.8; 1.0 |] ;
      0.5, [| 0.0; 1.0; 0.7 |] ;
      0.7, [| 0.9; 0.8; 0.4 |] ;
      1.0, [| 1.0; 0.4; 0.3 |] ]
let pi = BatFloat.pi
let to_deg rad = 180. *. rad /. pi

let peers_chart ?(is_bytes=false) datasets =
    let svg_height = Datatype.Float.of_pref "CPN_GUI_SVG_HEIGHT" 800. in
    let string_of_val = if is_bytes then Datatype.string_of_volume else Datatype.string_of_number in
    let inner_rad = 0.3 *. svg_height in
    let inner_x = svg_height/.2. and inner_y = svg_height/.2. in
    let inner r x y = (x -. inner_x) *. r +. inner_x,
                      (y -. inner_y) *. r +. inner_y in
    let x_of_ang ang = inner_rad *. cos ang +. inner_x
    and y_of_ang ang = inner_rad *. sin ang +. inner_y in
    (* Build a list of all peers *)
    let other_volume = Hashtbl.find datasets (Other "other", Other "") in
    let tot_volume = ref 0. and max_volume = ref 0. in
    (* peers : a hash of label to (vol_up, vol_down, angle) *)
    let peers = Hashtbl.create (Hashtbl.length datasets) in
    (* First insert all the keys *)
    Hashtbl.iter (fun (peer1,peer2) v ->
        tot_volume := !tot_volume +. v ;
        max_volume := max !max_volume v ;
        Hashtbl.modify_opt peer1 (function
           | None -> Some (0., v, 0.)
           | Some (up, down, ang) -> Some (up, down+.v, ang)) peers ;
        Hashtbl.modify_opt peer2 (function
           | None -> Some (v, 0., 0.)
           | Some (up, down, ang) -> Some (up+.v, down, ang)) peers
    ) datasets ;
    (* Then assign angles *)
    let nb_peers = Hashtbl.length peers in
    let a = 2.*.pi /. (float_of_int nb_peers) in
    let ang = ref (-. pi/.2.) in
    Hashtbl.map_inplace (fun _peer (up,down,_) ->
        let ang_ = mod_float !ang (2.*.pi) in
        ang := !ang +. a ;
        up, down, ang_) peers ;
    let opacity w = 0.05 +. 0.95 *. w in
    let color w = Color.get color_scale w in
    [ table ~attrs:["class","svg"] [ tr [ td
        [ svg ~cls:"clopinet"
            [ (* Traffic *)
              g (Hashtbl.fold (fun (p1, p2) v l ->
                 try (
                     let _, _, a1 = Hashtbl.find peers p1
                     and _, _, a2 = Hashtbl.find peers p2 in
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
                     let label1 = string_of_label p1
                     and label2 = string_of_label p2 in
                     (g [ path ~attrs:["class",label1^" "^label2] ~fill:col ~stroke:col ~stroke_opacity:opac ~stroke_width:0. ~fill_opacity:opac d ]) :: l
                 ) with Not_found -> l)
                 datasets []) ;
              (* The peers *)
              g (Hashtbl.fold (fun p (up, down, a) l ->
                let x, y = x_of_ang a, y_of_ang a in
                let label_js = js_of_label p and label_str = string_of_label p in
                let a' = (mod_float (a +. pi/.2.) pi) -. pi/.2. in
                let anchor = if a < 0.5*.pi || a > 1.5*.pi then "start" else "end" in
                (g ~attrs:["transform","translate("^string_of_float x^","^string_of_float y^") "^
                                         "rotate("^string_of_float (to_deg a') ^")" ;
                             "onmouseover","label_select("^ label_js ^", 'up:&nbsp;"^Datatype.string_of_volume up^"</br>down:&nbsp"^Datatype.string_of_volume down^"')" ;
                             "onmouseout", "label_unselect("^ label_js ^")" ]
                     [ text ~attrs:["class",label_str] ~style:("text-anchor:"^anchor^"; dominant-baseline:central")
                            ~font_size:15. ~fill:"#444" ~stroke:"#444" ~stroke_width:0. label_str ])::l
              ) peers []) ] ] ;
        td
            [ div ~attrs:["class","svg-info"]
                [ h2 (string_of_val !tot_volume) ;
                  h3 (Printf.sprintf "%.1f%% of total"
                       (100.*. !tot_volume /. (other_volume +. !tot_volume))) ;
                  h4 ~id:"selected-peer-name" "" ;
                  p ~id:"selected-peer-info" [] ;
                  p ~id:"selected-peer-links" [] ] ] ] ] ]

let peers_graph datasets layout =
    let svg_width  = Datatype.Float.of_pref "CPN_GUI_SVG_WIDTH" 1000.
    and svg_height = Datatype.Float.of_pref "CPN_GUI_SVG_HEIGHT" 800. in
    (* Get max volume *)
    let max_volume = Hashtbl.fold (fun _k1 n m ->
        Hashtbl.fold (fun _k2 y m ->
            max y m) n m) datasets 0. in
    let weight w = 1. +. 2. *. w in
    let color w = Color.get color_scale w in
    let out, inp = Unix.open_process "tee /tmp/dot.log | dot -Tplain" in (* dot outputs HTML header :-< *)
    let font_size_pt = 11 in
    Printf.fprintf inp "graph network {\n\
node [shape=box,height=0.2];\n\
graph [fontsize=%d];\n\
overlap=scale;\n\
layout=\"%s\";\n"
    font_size_pt layout ;
    let iname_of_label n = Marshal.(to_string n [No_sharing]) |> Base64.str_encode
    and label_of_iname n = let s = Base64.str_decode n in Marshal.from_string s 0 in
    Hashtbl.iter (fun k1 n ->
        Hashtbl.iter (fun k2 y ->
            let w = y /. max_volume in
            Printf.fprintf inp "\t\"%s-\" -- \"%s-\" [weight=%f];\n" (* the added - is to force dot to quote *)
                (iname_of_label k1) (iname_of_label k2) (weight w))
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
            Scanf.fscanf ic "node \"%s@-\" %f %f %f %f %_s %_s %_s %_s %_s\n" (fun iname x y w h ->
                let x, y = dot_2_svg x y
                and w, h = dot_2_svg w h in
                Hashtbl.add node_pos (label_of_iname iname) (x, svg_height -. y, w, h))
            done
        with Scanf.Scan_failure _ -> ()) ;
        let pos_of n =
            let x,y,_w,_h = Hashtbl.find node_pos n in
            x,y in
        IO.close_out inp ; (* cleans everything *)
        let svg_nodes = Hashtbl.fold (fun n (x,y,w,h) p ->
            let label_js = js_of_label n in
            let is_mac = match n with Mac _ -> true | _ -> false in
            let col = if is_mac then "#888" else "#5c8" in
            (g ~attrs:[ "onmouseover","label_select("^ label_js ^", '')" ;
                        "onmouseout","label_unselect("^ label_js ^")" ]
               [ rect ~fill:col (x-.w/.2.) (y-.h/.2.) w h ;
                 text ~x ~y (string_of_label n) ]) ::p)
            node_pos [] in
        let svg_edges = Hashtbl.fold (fun k1 n p ->
            Hashtbl.fold (fun k2 y p ->
                let w = y /. max_volume in
                let col = color w and sw = 0.5 *. font_size in
                (g ~attrs:[ "onmouseover", "link_select(evt, "^js_of_label k1^", "^js_of_label k2^")" ]
                    [ line ~stroke:col ~stroke_width:sw (pos_of k1) (pos_of k2) ])::p)
                n p) datasets [] in
        (* FF seams confused by svg when implementing getBoundingClientRect.
           That's why we introduce this "netgraph" div here (actualy now a td),
           so that we can reliably get SVG's bounding box position (this works
           iif the div box actually stick to svg's boundary). *)
        [ table ~attrs:["class","svg"] [ tr
            [ td ~id:"netgraph"
                [ svg ~attrs:["class","clopinet"]
                    [ g ~id:"scaler" ~attrs:[ "transform","scale(1)" ]
                        [ g svg_edges ;
                          g ~attrs:["style","text-anchor:middle; dominant-baseline:central" ;
                                    "font-size", Html.string_of_float font_size]
                            svg_nodes ] ] ] ;
              td [ div ~attrs:["class","svg-info"]
                    [ h2 (Printf.sprintf "%d nodes" (Hashtbl.length node_pos)) ;
                      h4 ~id:"selected-peer-name" "" ;
                      p ~id:"selected-peer-info" [] ;
                      p ~id:"selected-peer-links" [] ] ] ] ] ;
          script ~attrs:["class","interactive"] "svg_explorer('netgraph', 'scaler');" ]
    with End_of_file ->
        [ raw "dot crashed" ]

(* Dataset is a list of (ts1, ts2, peer1, peer2, descr, group), where group is used for coloring *)
let callflow_chart start (datasets : Flow.callflow_item list) =
    let svg_height = Datatype.Float.of_pref "CPN_GUI_SVG_HEIGHT" 800. in
    let left_margin = 50. and peer_width = 150.
    and first_ts = ref Int64.max_int and last_ts = ref Int64.zero
    and bw_max = ref 0. and tot_svg_height = ref svg_height
    and min_dt = ref Int64.max_int in
    (* Build the hash of all peers and set their X location and time range *)
    let peers = Hashtbl.create 71 (* ip -> (ref x, ts_min, ts_max) *) in
    let more_peers = ref false in
    (* Helpers to access peers hash content *)
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
            if Hashtbl.length peers < Datatype.Integer.of_pref "CPN_GUI_MAX_CALLFLOW_HOSTS" 500 then
                Hashtbl.add peers ip (ref 0., ts1, ts2)
            else
                more_peers := true
        | Some (x, ts1',ts2') ->
            Hashtbl.replace peers ip (x, Timestamp.min ts1 ts1', Timestamp.max ts2 ts2') in
    let tot_volume = ref 0. in
    let update_bw ts1 ts2 vol =
        tot_volume := !tot_volume +. vol ;
        if ts2 > ts1 then
            let bw = vol /. (Timestamp.sub ts2 ts1 |> Int64.to_float) in
            if bw > !bw_max then bw_max := bw in
    let nb_flows = ref 0 in
    let connected_to = Hashtbl.create 31 in (* while we are at it, we build this hash of peer -> senders (as string, for CSS class) *)
    let update_connected ip1 ip2 =
        match Hashtbl.find_option connected_to ip2 with
        | None -> Hashtbl.add connected_to ip2 (ip1^" ")
        | Some str -> Hashtbl.replace connected_to ip2 (str^ip1^" ") in
    List.iter (function
        | ts1,ts2,ip1,ip2,_,_,Flow.Dt vol ->
            update_peer ip1 ts1 ts2 ;
            update_peer ip2 ts1 ts2 ;
            update_bw ts1 ts2 vol ;
            incr nb_flows ;
            update_connected ip1 ip2 ;
            update_connected ip2 ip1
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
    let min_dt_pix = 16. in
    let h = ((Int64.to_float (Timestamp.sub !last_ts !first_ts)) *. min_dt_pix) /.
            Int64.to_float !min_dt in
    tot_svg_height := max svg_height h ;
    (* Find out x of peers, ordering them from taller to smaller, with the given source as first *)
    let peer_durations =
        Hashtbl.enum peers /@
        (fun ((_ip, (_x, ts_min, ts_max)) as peer) -> peer, Timestamp.sub ts_max ts_min) |>
        Array.of_enum in
    Array.sort (fun (p1, dt1) (p2, dt2) ->
        if fst p1 = start then -1 else
        if fst p2 = start then 1 else
        ~- (Timestamp.compare dt1 dt2))
        peer_durations ;
    let peer_y_padding = 20. in
    Array.iteri (fun i ((_ip, (x, ts_min, ts_max)), _dt) ->
        let xx =
            if i < 2 then (
                left_margin +. float_of_int i *. peer_width
            ) else (
                (* Make peers as close to left edge as possible *)
                let rec blocker_x ii =
                    if ii = 0 then left_margin else
                    let (_ip, (x', ts_min', ts_max')), _dt = peer_durations.(ii) in
                    if y_of_ts ts_min' -. 2. *. peer_y_padding <= y_of_ts ts_max +. peer_y_padding &&
                       y_of_ts ts_max' +. peer_y_padding >= y_of_ts ts_min -. 2. *. peer_y_padding then (* padding + title of the timeline *)
                        !x'
                    else
                        blocker_x (pred ii) in
                blocker_x (pred i) +. peer_width
            ) in
        x := xx) peer_durations ;
    (* Show timeline for each peer (TODO: better allocation of X space) *)
    let svg_grid =
        let mark_margin = 999999. in
        let nb_marks = (int_of_float !tot_svg_height) / 200 in (* approx one mark every 200 pixs *)
        Plot.grid nb_marks (Int64.to_float !first_ts) (Int64.to_float !last_ts) |>
        Enum.map (fun y ->
            let t = Int64.of_float y in
            let y = y_of_ts t in
            g [ line ~stroke:"#bbb" ~stroke_width:2. (-.mark_margin, y) (peer_width *. (float_of_int (Hashtbl.length peers)) +. mark_margin, y) ;
                text ~x:0. ~y:(y-.5.) ~style:("text-anchor:end") ~font_size:10. (Timestamp.to_string t) ]) |>
        List.of_enum
    and svg_peers =
        (* A set of columns, one for each IP *)
        Hashtbl.fold (fun ip (x, ts1, ts2) p ->
            let y1 = y_of_ts ts1 -. peer_y_padding and y2 = y_of_ts ts2 +. peer_y_padding in
            (
                let senders = Hashtbl.find_option connected_to ip |? "" in
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
                g ~attrs:[ "onmouseover","dt_select(evt, '"^ip_s^"', '"^ip_d^"')" ]
                    [ path ~attrs:["class","fitem "^ip_s^" "^ip_d] ~stroke:fill ~stroke_width:0.5 ~stroke_opacity:0.7 ~fill_opacity ~fill (
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
                    text ~attrs:["class","fitem "^ip_s^" "^ip_d] ~x:(x1+.2.) ~y:(y1-.5.) ~style:("text-anchor:"^(if x2>x1 then "start" else "end")) ~font_size:6.
                         descr ]
            | Flow.Tx resp ->
                let is_visible x y = x >= 0. && y >= 0. && y <= !tot_svg_height in
                if is_visible x1 y1 && is_visible x2 y2 then (
                    let hx = hx *. 0.5 in
                    let mid_x = (x1 +. x2)*.0.5 in
                    (* Draw query + resp *)
                    g ~attrs:[ "onmouseover","tx_select(evt, '"^ip_s^"', '"^ip_d^"')" ]
                        [ path ~attrs:["class","fitem "^ip_s^" "^ip_d] ~stroke:fill ~stroke_width:2.5 ~stroke_opacity:0.7 ~fill_opacity:0. (
                            moveto (x1, y1 -. dw -. inclin) ^
                            lineto (mid_x, y1 -. dw) ^
                            curveto (x2, y1 -. dw +. inclin) (x2, y1 -. dw +. inclin) (x2, (y1 +. y2)*.0.5) ^
                            curveto (x2, y2 +. dw -. inclin) (x2, y2 +. dw -. inclin) (mid_x, y2 +. dw) ^
                            lineto (x1, y2 +. dw +. inclin) ^
                            lineto (x1 +. hx, y2 +. dw +. inclin -. hx) ^
                            moveto (x1, y2 +. dw +. inclin) ^
                            lineto (x1 +. hx, y2 +. dw +. inclin +. hx)
                        ) ;
                        text ~attrs:["class","fitem "^ip_s^" "^ip_d]  ~x:(x1+.hx) ~y:(y1-.dw-.inclin+.5.) ~style:("text-anchor:left; dominant-baseline:hanging") ~font_size:7. descr ;
                        text ~attrs:["class","fitem "^ip_s^" "^ip_d] ~x:(x1+.hx) ~y:(y2+.dw-.inclin-.3.) ~style:("text-anchor:left") ~font_size:7. resp
                    ]
                ) else if is_visible x1 y1 then (
                    (* TODO: display a small tick in x1 timeline? *)
                    g []
                ) else if is_visible x2 y2 then (
                    (* TODO: display a small tick in x1 timeline? *)
                    g []
                ) else g []
        ) datasets in
    [ table ~attrs:["class","svg"] [ tr
        [ td ~id:"callflow"
            [ svg ~attrs:["class","clopinet"]
              [ g ~id:"scaler" ~attrs:[ "transform","scale(1)" ]
                  [ g svg_grid ;
                    g svg_flows ;
                    g svg_peers ] ] ] ;
          td [ div ~attrs:["class","svg-info"]
                [ h2 (Datatype.string_of_volume !tot_volume) ;
                  h3 (Printf.sprintf "%d hosts" (Hashtbl.length peers)) ;
                  h4 ~id:"selected-peer-name" "" ;
                  p ~id:"selected-peer-info" [] ;
                  p ~id:"selected-peer-links" [] ] ] ] ] ;
      Html.Block (if !more_peers then [ p [ cdata "some peers did not fit in this graph. Consider reducing time range." ] ] else []) ;
      script ~attrs:["class","interactive"] "svg_explorer('callflow', 'scaler');" ]

(* Helpers for axis and grids *)

let axis ?(extend_ticks=0.) ?(stroke="#000") ?(stroke_width=1.) ?(arrow_size=0.) ?(tick_spacing=100.) ?(tick_length=5.) ?(label="") ?(font_size=16.) ?opacity ?(string_of_v=Datatype.string_of_number) ?(invert=false) (x1, y1) (x2, y2) v_min v_max =
    let sq x = x *. x in
    let axis_len = sqrt (sq (x2-.x1) +. sq (y2-.y1)) in
    (* u, v are unit vectors along axis and perpendicular to it *)
    let ux, uy = (x2 -. x1) /. axis_len, (y2 -. y1) /. axis_len in
    let vx, vy = ~-.uy, ux in
    let mostly_horiz = abs_float ux >= abs_float uy in
    let mostly_horiz = (if invert then not else identity) mostly_horiz in
    let add (ax, ay) (bx, by) = ax +. bx, ay +. by in
    let goto x y = x *. ux +. y *. vx, x *. uy +. y *. vy in
    g (
        (path ~stroke ~stroke_width ~fill:"none" ?stroke_opacity:opacity
            (moveto (x1, y1) ^
            lineto (x2, y2) ^
            lineto (add (x2, y2) (goto ~-.arrow_size ~-.arrow_size)) ^
            moveto (x2, y2) ^
            lineto (add (x2, y2) (goto ~-.arrow_size arrow_size)))) ::
        (let x, y =
            add (x2, y2) (goto (-1.5 *. font_size) (if mostly_horiz then (-1.5 *. font_size) else (1.5 *. font_size))) in
        (* TODO: rotate this text? *)
        let style =
            if mostly_horiz then "text-anchor:end; dominant-baseline:alphabetic"
                            else "text-anchor:start; dominant-baseline:central" in
        text ~font_size:(1.2 *. font_size) ?stroke_opacity:opacity ?fill_opacity:opacity ~x ~y ~style label) ::
        (
            Plot.grid (axis_len /. tick_spacing |> Int.of_float) v_min v_max /@
            (fun v ->
                let t = ((v -. v_min) /. (v_max -. v_min)) *. axis_len in
                let tick_start = add (x1, y1) (goto t ~-.tick_length) in
                let tick_stop  = add (x1, y1) (goto t tick_length) in
                g [ line ?stroke_opacity:opacity ~stroke ~stroke_width tick_start tick_stop ;
                    line ~stroke ~stroke_width:(stroke_width *. 0.6) ~stroke_opacity:0.1
                         tick_stop (add tick_stop (goto 0. extend_ticks)) ;
                    let x, y =
                        if mostly_horiz then
                            add tick_stop (goto 0. font_size)
                        else
                            add tick_start (goto 0. ~-.font_size)
                        in
                    let style =
                        if mostly_horiz then "text-anchor:middle; dominant-baseline:hanging"
                                        else "text-anchor:end; dominant-baseline:central" in
                    g (
                        string_of_v v |>
                        String.nsplit ~by:"\n" |>
                        List.map (fun s -> s, font_size) |>
                        texts ?stroke_opacity:opacity ?fill_opacity:opacity ~style x y
                    )
                  ]) |>
            List.of_enum
        )
    )

let get_ratio x_min x_max v_min v_max v =
    let r = (v -. v_min) /. (v_max -. v_min) in
    x_min +. r *. (x_max -. x_min)

let xy_grid ?(show_vgrid=true) ?stroke ?stroke_width ?font_size ?arrow_size ?x_tick_spacing ?y_tick_spacing ?tick_length ?x_label ?y_label ?string_of_y ?y2 ?string_of_x (x_min, x_max) (y_min, y_max) (vx_min, vx_max) (vy_min, vy_max) =
    let get_x = get_ratio x_min x_max vx_min vx_max
    and get_y = get_ratio y_min y_max vy_min vy_max in
    let bound_by mi ma v =
        if v < mi then mi else
        if v > ma then ma else
        v in
    let x_orig = bound_by x_min x_max (get_x 0.)
    and y_orig = bound_by y_max y_min (get_y 0.) in (* note that y_min, the Y of the origin, is actually greater the y_max, due to the fact that SVG Y starts at top of img *)
    let x_axis =
        axis ?stroke ?stroke_width ?arrow_size ?tick_spacing:x_tick_spacing ?font_size ?tick_length
             ?label:x_label ?string_of_v:string_of_x (x_min, y_orig) (x_max, y_orig) vx_min vx_max
    and y_axis =
        axis ?stroke ?stroke_width ?arrow_size ?tick_spacing:y_tick_spacing ?font_size ?tick_length ~extend_ticks:(if show_vgrid then x_max -. x_min else 0.)
             ?label:y_label ?string_of_v:string_of_y (x_orig, y_min) (x_orig, y_max) vy_min vy_max
    and y2_axis = match y2 with
        | None -> g []
        | Some (label, string_of_v, vy2_min, vy2_max) ->
            axis ?stroke ?stroke_width ?arrow_size ?tick_spacing:y_tick_spacing ?font_size
                 ?tick_length ~invert:true ~label ~string_of_v ~opacity:0.5
                 (x_max, y_min) (x_max, y_max) vy2_min vy2_max in
    g [ x_axis ; y_axis ; y2_axis ]


let peers_map datasets =
    let max_ips = Datatype.Integer.of_pref "CPN_GEOIP_MAX_IPS" 10 in
    let max_volume = Hashtbl.fold (fun _loc1 h m ->
        Hashtbl.fold (fun _loc2 (_,_,up,down) m' ->
            max (up+down) m') h m) datasets 0 |> float_of_int in
    let opacity w = 0.3 +. 0.7 *. w
    and color w = Color.get color_scale w in
    let mark_loc (cc,lat,long) ips =
        if cc = "" then "" else
        let os = IO.output_string () in
        let len = List.length ips in
        let ips, last =
            let m = max_ips - 1 in
            if len > m then list_merge_lim m ips [], "<br/>..."
                       else ips, "" in
        Printf.fprintf os "L.marker([%f, %f]).bindPopup('%a').addTo(map);\n"
            lat long
            (List.print ~first:"" ~last ~sep:"<br/>"
                        (fun fmt ip -> Printf.fprintf fmt "%s" (Datatype.InetAddr.to_string ip)))
                ips ;
        IO.close_out os in
    let mark_link (cc1,lat1,long1) (cc2,lat2,long2) vol =
        if cc1 = "" || cc2 = "" then "" else
        let os = IO.output_string () in
        let w = float_of_int vol /. max_volume in
        Printf.fprintf os "L.polygon([[%f, %f],[%f, %f]], {noClip: false, fill: false, color: '%s', opacity: %f}).addTo(map);\n"
            lat1 long1 lat2 long2
            (color w) (opacity w) ;
        IO.close_out os in
    let all_marks =
        Hashtbl.fold (fun loc1 h p ->
            let str, srcs = Hashtbl.fold (fun loc2 (srcs, dsts, up, down) (prev_str,prev_srcs) ->
                prev_str ^ mark_loc loc2 dsts ^ mark_link loc1 loc2 (up+down),
                list_merge_lim max_ips srcs prev_srcs)
                h (p, []) in
            str ^ (mark_loc loc1 srcs))
            datasets "" in
    [ div ~id:"map" [] ;
      tag "script" ~attrs:["type","text/javascript" ; "src","http://cdn.leafletjs.com/leaflet-0.4/leaflet.js"] [] ;
      script ("\
var map = L.map('map').setView([50, 0], 2);\n\
L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png', {\n\
    attribution: '&copy; <a href=\"http://osm.org/copyright\">OpenStreetMap</a> contributors',\n\
    minZoom: 2\n\
    }).addTo(map);\n\
" ^ all_marks)
    ]
