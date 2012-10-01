open Bricabrac
open Datatype

(* Can be ploted, although no as stacked area, with:
 * plot for [i=0:50] 'traf' index i using 1:2 title columnheader(1) with lines smooth unique *)
let top_datasets max_graphs datasets =
    (* Reduce number of datasets to max_graphs *)
    if Hashtbl.length datasets <= max_graphs then (
        datasets
    ) else (
        let max_peak = Array.create (max_graphs-1) None (* ordered by max peak (bigger first) *)
        and others = Hashtbl.create 57
        and max_pts pts =
            let rec aux m = function
                | [] -> m
                | (_x, y)::res -> aux (max m y) res in
            aux 0. pts in
        let add_to_others label =
            Hashtbl.find datasets label |>
            List.iter (fun (x, y) ->
                (* add it to others then *)
                let prev_y = try Hashtbl.find others x with Not_found -> 0. in
                Hashtbl.replace others x (prev_y +. y)) in
        let insert_max max label =
            let rec aux i =
                if i < Array.length max_peak - 1 then (
                    if (match max_peak.(i) with
                        | None -> true
                        | Some (m, _) -> m < max) then
                    (
                        (* make one place *)
                        if i < Array.length max_peak - 1 then (
                            (* move last entry into others *)
                            (match max_peak.(Array.length max_peak - 1) with
                                | None -> ()
                                | Some (_m, label) -> add_to_others label) ;
                            (* make some room *)
                            Array.blit max_peak i max_peak (i+1) (Array.length max_peak - i - 1)
                        ) ;
                        max_peak.(i) <- Some (max, label) ;
                        true
                    ) else (
                        aux (i+1)
                    )
                ) else false in
            aux 0 in
        Hashtbl.iter (fun label pts ->
            let max = max_pts pts in
            if not (insert_max max label) then (
                add_to_others label
            )) datasets ;
        (* recompose datasets from max_peak and others *)
        let others_pts = Hashtbl.fold (fun x y pts -> (x,y)::pts) others []
        and new_datasets = Hashtbl.create 11 in
        Array.iter (function None -> () | Some (_max, label) ->
            Hashtbl.add new_datasets label (Hashtbl.find datasets label)) max_peak ;
        Hashtbl.add new_datasets "others" others_pts ;
        new_datasets
    )

let stacked_area datasets =
    Hashtbl.iter (fun label pts ->
        Printf.printf "\"%s\"\n" label ;
        List.sort (fun (x1, _) (x2, _) -> compare x1 x2) pts |>
        List.iter (fun (x, y) ->
            Printf.printf "%f %f\n" x y) ;
        Printf.printf "\n\n")
        datasets


module TimeGraph (Record : DATATYPE) (Key : DATATYPE) =
struct
    module Maplot = Finite_map_impl.Finite_map (struct
        type t = Int64.t * Key.t
        let compare (t1,k1) (t2,k2) =
            let c = Integer64.compare t1 t2 in
            if c = 0 then Key.compare k1 k2
            else c
    end)

    let plot ?(max_graphs=10) step fold extract label_of_key =
        assert (step > 0) ;
        let step = Int64.of_int step in
        let cumul_y m k y =
            Maplot.update_with_default y m k (fun prev -> prev +. y) in
        let m =
            fold (fun r m ->
                let k, (t1s, _t1us), (t2s, _t2us), y = extract r in
                (* step is a number of seconds. *)
                let t1' = Int64.div t1s step
                and t2' = Int64.div t2s step in
                if t1' = t2' then (
                    cumul_y m (Int64.mul t1' step, k) y
                ) else (
                    let dt = Int64.sub t2' t1' in
                    let dy = y /. Int64.to_float dt in
                    let rec aux ts prev =
                        if ts >= t2' then prev
                        else aux (Int64.succ ts)
                                 (cumul_y prev (Int64.mul ts step, k) dy) in
                    aux t1' m
                ))
                Maplot.empty
                (fun m1 m2 -> (* merge two maps, m1 being the big one, so merge m2 into m1 *)
                    Maplot.fold_left cumul_y m1 m2) in
        let datasets = Hashtbl.create 71
        and step = Int64.to_float step in
        Maplot.iter m (fun (t, k) y ->
            let label = label_of_key k in
            let prev = try Hashtbl.find datasets label with Not_found -> [] in
            (* Notice we want y/secs, so we divide each sample by time step *)
            Hashtbl.replace datasets label ((Int64.to_float t, y /. step)::prev)) ;

        top_datasets max_graphs datasets
end
