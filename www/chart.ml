open Metric
open Batteries
open Html

(** {1 Plot Chart}

  Take functions as much as possible as parameters instead of
  some given choice amongst possible data structures.

  The whole style is supposed to be in the CSS.

*)

(** Draw an axis-arrow with graduations, ticks and label. *)
let axis ?(extend_ticks=0.) ?(stroke="#000") ?(stroke_width=1.)
         ?(arrow_size=0.) ?(tick_spacing=100.) ?(tick_length=5.)
         ?(label="") ?(font_size=16.) ?opacity
         ?(string_of_v=Datatype.string_of_number) ?(invert=false)
         (x1, y1) (x2, y2) v_min v_max =
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
        (* TODO: rotate this text *)
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

(** if x_min corresponds to v_min and x_max to v_max, find the x which
 * corresponds to v *)
let get_ratio x_min x_max v_min v_max v =
    let r = (v -. v_min) /. (v_max -. v_min) in
    x_min +. r *. (x_max -. x_min)

(** Draws a grid ready for any XY chart *)
let xy_grid ?(show_vgrid=true) ?stroke ?stroke_width ?font_size
            ?arrow_size ?x_tick_spacing ?y_tick_spacing ?tick_length
            ?x_label ?y_label ?string_of_y ?y2 ?string_of_x
            (x_min, x_max) (y_min, y_max) (vx_min, vx_max) (vy_min, vy_max) =
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


(** Draws a XY plot.

  Apart from the various parameters to customize the look of the plot,
  the interresting parameter is the fold function.
  It calls back with: the previous value, the label of the dataset,
  a boolean telling if the dataset is mean for the primary (true) or
  secondary (false) axis, and a getter (index -> vy).

  Notice the fold_t array trick to force polymorphism of fold.

*)

type fold_t = {
    (* The bool in there is true for all plots in the "primary" chart, and
     * false once at most for the "secondary" plot. Note: the secondary plot
     * is displayed with a distinct Y axis. *)
    fold : 'a. ( 'a -> label -> bool -> (int -> float) -> 'a) -> 'a -> 'a }
            (* I wonder what's the world record in argument list length? *)
type stacked = NotStacked | Stacked | StackedCentered
let xy_plot ?(string_of_y=Datatype.string_of_number) ?(string_of_y2=Datatype.string_of_number) ?string_of_x
            ?(svg_width=800.) ?(svg_height=600.) ?(font_size=14.)
            ?(margin_bottom=30.) ?(margin_left=10.) ?(margin_top=30.) ?(margin_right=10.)
            ?(y_tick_spacing=100.) ?(x_tick_spacing=200.) ?(tick_length=5.5)
            ?(axis_arrow_h=11.)
            ?(vxmin_filter="filter/start") ?(vxmax_filter="filter/stop") ?(vxstep_filter="filter/tstep")
            ?(stacked=NotStacked) ?(force_show_0=false) ?(show_rate=false) ?x_label_for_rate
            ?(scale_vx=1.)
            x_label y_label
            vx_min_unscaled vx_step_unscaled nb_vx
            fold =
    let vx_min = vx_min_unscaled *. scale_vx
    and vx_step = vx_step_unscaled *. scale_vx
    and vx_max_unscaled = vx_min_unscaled +. (float_of_int nb_vx -. 0.5) *. vx_step_unscaled in
    let force_show_0 = if stacked = StackedCentered then true else force_show_0 in
    let y_label_grid = if show_rate then y_label ^"/"^ (x_label_for_rate |? x_label) else y_label in
    (* build iter and map from fold *)
    let iter_datasets f = fold.fold (fun _prev label prim get -> f label prim get) ()
    and map_datasets f = List.rev @@ fold.fold (fun prev label prim get -> (f label prim get) :: prev) []
    and rate_of_vy vy = if show_rate then vy /. vx_step else vy in
    (* Graph geometry in pixels *)
    let max_label_length = y_tick_spacing *. 0.9 in
    let y_axis_x = margin_left +. max_label_length in
    let x_axis_y = svg_height -. margin_bottom -. font_size *. 1.2 in
    let y_axis_ymin = x_axis_y and y_axis_ymax = margin_top
    and x_axis_xmin = y_axis_x and x_axis_xmax = svg_width -. margin_right in
    (* Data bounds *)
    let vx_of_bucket i = vx_min +. (float_of_int i +. 0.5) *. vx_step in
    (* TODO: if vx_min is close to 0 (compared to vx_max) then clamp it to 0 *)
    let vx_max = vx_of_bucket (nb_vx-1) in
    (* Compute max Y for a given bucket (for primary and secondary Ys) *)
    let max_vy = Array.init 2 (fun _ -> Array.create nb_vx 0.) in
    let label2 = ref None in
    let set_max pi =
        if stacked = NotStacked then
            (* keep the max of the Ys *)
            (fun i c -> max_vy.(pi).(i) <- max max_vy.(pi).(i) c)
        else
            (* sum the Ys *)
            (fun i c -> max_vy.(pi).(i) <- max_vy.(pi).(i) +. c) in
    iter_datasets
        (fun label prim get ->
            if not prim then label2 := Some label ;
            let pi = if prim then 0 else 1 in
            for i = 0 to nb_vx-1 do set_max pi i (get i |> rate_of_vy) done) ;
    (* TODO: if vy_min is close to 0 (compared to vy_max) then clamp it to 0 *)
    let vy_min = Array.create 2 max_float
    and vy_max = Array.create 2 0. in
    for pi = 0 to 1 do
        let ma, mi =
            Array.fold_left (fun (ma, mi) y ->
                max ma y, min mi y)
                (0., max_float)
                max_vy.(pi) in
        vy_max.(pi) <- if force_show_0 then max ma 0. else ma ;
        vy_min.(pi) <- if force_show_0 then min mi 0. else mi
    done ;
    if stacked = StackedCentered then (
        vy_max.(0) <- vy_max.(0) *. 0.5 ;
        vy_min.(0) <- -. vy_max.(0)
    ) ;
    let get_x    = get_ratio x_axis_xmin x_axis_xmax vx_min vx_max
    and get_y pi = get_ratio y_axis_ymin y_axis_ymax vy_min.(pi) vy_max.(pi) in
    (* In case we stack the values *)
    let prev_vy =
        if stacked = StackedCentered then
            (* Start from -0.5 * tot_y for this bucket *)
            Array.init nb_vx (fun i -> ~-.0.5 *. max_vy.(0).(i))
        else
            Array.create nb_vx 0. in
    (* per chart infos *)
    let tot_vy = Hashtbl.create 11
    and tot_vys = ref 0. in
    iter_datasets (fun lbl prim get ->
        if prim then for i = 0 to nb_vx-1 do
            let vy = get i in
            tot_vys := !tot_vys +. vy ;
            Hashtbl.modify_def 0. lbl ((+.) vy) tot_vy
        done) ;
    let dvx = vx_max -. vx_min in
    let avg_vy =
        if dvx > 0. then
            (string_of_y (!tot_vys /. dvx)) ^ y_label_grid
        else "none" in
    let info prim label =
        if prim then (
            let tot = Hashtbl.find tot_vy label in
            "Tot:&nbsp;"^ (string_of_y tot) ^ y_label ^
            (if dvx > 0. then "<br/>Avg:&nbsp;"^ (string_of_y (tot /. dvx)) ^ y_label_grid else "")
        ) else "" in
    (* The SVG *)
    let path_of_dataset label prim get =
        let is_stacked = stacked <> NotStacked && prim in
        let pi = if prim then 0 else 1 in
        let label_str = string_of_label label in
        let label_js = js_of_label label in
        let color = Color.random_of_string label_str in
        let stroke = Color.to_html color in
            path ~stroke:(if is_stacked then "none" else stroke)
             ~stroke_width:(if is_stacked then 0.7 else 1.)
             ~fill:(if is_stacked then stroke else "none")
             ?fill_opacity:(if is_stacked then Some 0.5 else None)
             ~attrs:["class","fitem "^ label_str ;
                     "onmouseover","label_select("^ label_js ^", '"^info prim label^"')" ;
                     "onmouseout", "label_unselect("^label_js ^")" ]
            (
                let buf = Buffer.create 100 in (* to write path commands in *)
                (* Top line *)
                for i = 0 to nb_vx-1 do
                    let vy' = (get i |> rate_of_vy) +. (if is_stacked then prev_vy.(i) else 0.) in
                    Buffer.add_string buf
                        ((if i = 0 then moveto else lineto)
                            (get_x (vx_of_bucket i), get_y pi vy'))
                done ;
                if is_stacked then (
                    (* Bottom line (to close the area) (note: we loop here from last to first) *)
                    for i = nb_vx-1 downto 0 do
                        let vy' = prev_vy.(i) in
                        prev_vy.(i) <- vy' +. (get i |> rate_of_vy) ;
                        Buffer.add_string buf
                            (lineto (get_x (vx_of_bucket i), get_y pi vy'))
                    done ;
                    Buffer.add_string buf closepath
                ) ;
                Buffer.contents buf
            )

    and legend_of_dataset label prim _get =
        let label_str = string_of_label label in
        let label_js = js_of_label label in
        let color = Color.random_of_string label_str in
        p ~attrs:["class","hitem "^ label_str ;
                  "onmouseover","label_select("^ label_js ^", '"^info prim label^"')" ;
                  "onmouseout", "label_unselect("^ label_js ^")" ] [
            span ~attrs:[ "class","color-box" ;
                          "style","background-color: " ^ Color.to_html color ]
                          [] ;
            raw label_str
        ] in

    let y2 =
        Option.bind !label2 (fun label ->
            Some (string_of_label label, string_of_y2, vy_min.(1), vy_max.(1))) in
    let grid = xy_grid ~stroke:"#000" ~stroke_width:2. ~font_size ~arrow_size:axis_arrow_h ~x_tick_spacing ~y_tick_spacing ~tick_length ~x_label ~y_label:y_label_grid ?string_of_x ~string_of_y ?y2 (x_axis_xmin, x_axis_xmax) (y_axis_ymin, y_axis_ymax) (vx_min, vx_max) (vy_min.(0), vy_max.(0))
    and distrs = g (map_datasets path_of_dataset) in
    let cursor = rect ~attrs:["id","cursor"] ~stroke:"none" ~fill:"#d8a" ~fill_opacity:0.3 x_axis_xmin y_axis_ymax 0. (y_axis_ymin -. y_axis_ymax) in
    [ table ~attrs:["class","svg"] [ tr
        [ td ~id:"plot"
            [ svg ~cls:"clopinet" [ cursor ; grid ; distrs ] ] ;
          td [ div ~attrs:["class","svg-info"]
                ([ h3 "Global" ;
                   p [ raw ("Tot: "^ (string_of_y !tot_vys) ^ y_label) ] ;
                   p [ raw ("Avg: "^ avg_vy) ] ;
                   h3 ~id:"selected-peer-name" "" ;
                   p ~id:"selected-peer-info" [] ;
                   p ~id:"selected-peer-links" [] ;
                   h3 "Legend" ] @
                 (map_datasets legend_of_dataset)) ] ] ] ;
        (* for this we really do want stdlib's string_of_float not our stripped down version *)
        script ~attrs:["class","interactive"]
            ("svg_explore_plot('plot', "^ string_of_float vx_min_unscaled ^", "^
                string_of_float vx_max_unscaled ^", "^
                string_of_float x_axis_xmin ^", "^
                string_of_float x_axis_xmax ^", "^
                string_of_float vx_step_unscaled ^", "^
                "'"^ vxmin_filter ^"', "^
                "'"^ vxmax_filter ^"', "^
                "'"^ vxstep_filter ^"');") ]

