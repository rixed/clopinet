open Bricabrac

let max_graphs = 10 (* lesser plots are accumulated into a "other" plot *)

(* Can be ploted, although no as stacked area, with:
 * plot for [i=0:50] 'traf' index i using 1:2 title columnheader(1) with lines smooth unique *)
let stacked_area datasets =
    let datasets =
        if Hashtbl.length datasets > max_graphs then (
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
        ) else (
            datasets
        ) in
    Hashtbl.iter (fun label pts ->
        Printf.printf "\"%s\"\n" label ;
        List.sort (fun (x1, _) (x2, _) -> compare x1 x2) pts |>
        List.iter (fun (x, y) ->
            Printf.printf "%f %f\n" x y) ;
        Printf.printf "\n\n") datasets
