open Batteries
open Datatype

let sort_pt (x1, _) (x2, _) = compare x1 x2

(* Reduce number of datasets to max_graphs.
 * Returns an array of (label, pts), bigger y max first (apart from
 * others, at the end) *)
let top_datasets max_graphs datasets nb_steps =
    let max_peak = Array.make (max_graphs-1) None (* ordered by max peak (bigger first) *)
    and others_pts = Array.make nb_steps 0.
    and max_pts pts = Array.fold_left max min_float pts
    and need_others = ref false in
    let add_to_others label =
        need_others := true ;
        Hashtbl.find datasets label |>
        Array.iteri (fun i y ->
            (* add it to others then *)
            others_pts.(i) <- others_pts.(i) +. y) in
    let insert_max max label =
        let rec aux i =
            if i < Array.length max_peak then (
                if (match max_peak.(i) with
                    | None -> true
                    | Some (m, _) -> m < max) then
                (
                    (* make one place by moving last entry into others *)
                    (match max_peak.(Array.length max_peak - 1) with
                        | None -> ()
                        | Some (_m, label) -> add_to_others label) ;
                    (* and scrolling entries *)
                    if i < Array.length max_peak - 1 then
                        Array.blit max_peak i max_peak (i+1) (Array.length max_peak - i - 1) ;
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
    let rec add_next_dataset prev i =
        if i < 0 then prev else
        add_next_dataset (match max_peak.(i) with
            | None -> prev
            | Some (_max, label) -> (label, Hashtbl.find datasets label)::prev)
            (pred i) in
    add_next_dataset (if !need_others then ["others", others_pts] else [])
                     (Array.length max_peak - 1)

(* Can be ploted, although no as stacked area, with:
 * plot for [i=0:50] 'traf' index i using 1:2 title columnheader(1) with lines smooth unique *)
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
        type t = Key.t
        let compare = Key.compare
    end)

    (* fold iterate over the database, while extract extract from a row the key, X start, X stop and Y value. *)
    let plot_continuous ?(max_graphs=10) tmin tmax step fold extract label_of_key =
        assert (step > 0L) ;
        (* Fetch min and max available time *)
        let row_of_time t = Int64.div (Int64.sub t tmin) step |> Int64.to_int in
        let nb_steps = row_of_time tmax |> succ in
        (* Now prepare the datasets as a map of Key.t to array of Y *)
        let flat_dataset () = Array.make nb_steps 0. in
        let cumul_y m k r1 r2 y =
            Maplot.update_with_default_delayed
                (fun () ->
                    let a = flat_dataset () in
                    for x = r1 to r2 do a.(x) <- y done ;
                    a)
                m k
                (fun a ->
                    for x = r1 to r2 do a.(x) <- a.(x) +. y done ;
                    a) in
        let m =
            fold (fun r m ->
                let k, t1, t2, y = extract r in
                let r1 = row_of_time t1
                and r2 = row_of_time t2 in
                if r1 = r2 then (
                    cumul_y m k r1 r1 y
                ) else (
                    (* We should split value more accurately here *)
                    let dt = r2-r1 |> succ |> float_of_int in
                    let y' = y /. dt in
                    cumul_y m k r1 r2 y'
                ))
                Maplot.empty
                (fun m1 m2 -> (* merge two maps, m1 being the big one, so merge m2 into m1 *)
                    Maplot.fold_left (fun m k a ->
                        (* add k->a into m *)
                        Maplot.update_with_default a m k (fun a' ->
                            Array.iteri (fun i y -> a'.(i) <- y +. a.(i)) a' ;
                            a'))
                        m1 m2) in
        let datasets = Hashtbl.create 71
        and step_s = Int64.to_float step /. 1000. in
        (* Build hashtables indexed by label (instead of map indexed by some key), and convert Y into Y per second. *)
        Maplot.iter m (fun k a ->
            let label = label_of_key k in
            (* Note that several keys may map to the same label, thus these precautions *)
            try let a' = Hashtbl.find datasets label in
                Array.iteri (fun i y -> a'.(i) <- (a'.(i) +. y) /. step_s) a
            with Not_found ->
                Array.iteri (fun i y -> a.(i) <- y /. step_s) a ;
                Hashtbl.add datasets label a) ;

        (* reduce number of datasets to max_graphs *)
        top_datasets max_graphs datasets nb_steps

end
