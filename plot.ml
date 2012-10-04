open Batteries
open Datatype

let sort_pt (x1, _) (x2, _) = compare x1 x2

let top_datasets max_graphs datasets nb_steps =
    (* FIXME: return an ordered list? *)
    (* Reduce number of datasets to max_graphs *)
    if Hashtbl.length datasets <= max_graphs then (
        datasets
    ) else (
        let max_peak = Array.make (max_graphs-1) None (* ordered by max peak (bigger first) *)
        and others = Array.make nb_steps 0.
        and max_pts pts = Array.fold_left max min_float pts in
        let add_to_others label =
            Hashtbl.find datasets label |>
            Array.iteri (fun i y ->
                (* add it to others then *)
                others.(i) <- others.(i) +. y) in
        let insert_max max label =
            let rec aux i =
                if i < Array.length max_peak then (
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
        let new_datasets = Hashtbl.create max_graphs in
        Array.iter (function None -> () | Some (_max, label) ->
            Hashtbl.add new_datasets label (Hashtbl.find datasets label)) max_peak ;
        Hashtbl.add new_datasets "others" others ;
        new_datasets
    )

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

(*    let test (type a) (f : a -> a) : int * bool =
        f 1, f true*)

    let get_min_max fold extract =
        fold (fun r (t_min, t_max) ->
            let _k, (t1s, _t1us), (t2s, _t2us), _y = extract r in
            min t_min t1s, max t_max t2s)
            (Int64.max_int, Int64.min_int)
            (fun (t_min1, t_max1) (t_min2, t_max2) ->
                (min t_min1 t_min2, max t_max1 t_max2))

    (* fold iterate over the database, while extract extract from a row the key, X start, X stop and Y value. *)
    let plot_continuous ?(max_graphs=10) (tmin, _) (tmax, _) step fold extract label_of_key =
        assert (step > 0) ;
        let step = Int64.of_int step in
        (* Fetch min and max available time *)
        let row_of_time t = Int64.div (Int64.sub t tmin) step |> Int64.to_int in
        let nb_steps = row_of_time tmax |> succ in
        (* Now prepare the datasets as a map of Key.t to array of Y *)
        let flat_dataset () = Array.make nb_steps 0. in
        let cumul_y m k x y =
            Maplot.update_with_default_delayed
                (fun () ->
                    let a = flat_dataset () in
                    a.(x) <- y ;
                    a)
                m k
                (fun a ->
                    a.(x) <- a.(x) +. y ;
                    a) in
        let m =
            fold (fun r m ->
                let k, (t1s, _t1us), (t2s, _t2us), y = extract r in (* FIXME: use millisecs when splitting values accross several time steps *)
                let x1' = row_of_time t1s
                and x2' = row_of_time t2s in
                if x1' = x2' then (
                    cumul_y m k x1' y
                ) else (
                    let dt = x2'-x1' |> succ |> float_of_int in
                    let dy = y /. dt in
                    let rec aux x prev =
                        if x > x2' then prev
                        else aux (succ x)
                                 (cumul_y prev k x dy) in
                    aux x1' m
                ))
                Maplot.empty
                (fun m1 m2 -> (* merge two maps, m1 being the big one, so merge m2 into m1 *)
                    Maplot.fold_left (fun m k a ->
                        (* add k->a into m *)
                        Maplot.update_with_default a m k (fun a ->
                            Array.iteri (fun i y -> a.(i) <- y +. a.(i)) a ;
                            a))
                        m1 m2) in
        let datasets = Hashtbl.create 71
        and step = Int64.to_float step in
        (* Build hashtables indexed by label (instead of map indexed by some key), and convert Y into Y per second. *)
        Maplot.iter m (fun k a ->
            let label = label_of_key k in
            (* Note that several keys may map to the same label, thus these precautions *)
            try let a' = Hashtbl.find datasets label in
                Array.iteri (fun i y -> a'.(i) <- (a'.(i) +. y) /. step) a
            with Not_found ->
                Array.iteri (fun i y -> a.(i) <- (a.(i) +. y) /. step) a ;
                Hashtbl.add datasets label a) ;

        (* reduce number of datasets to max_graphs *)
        top_datasets max_graphs datasets nb_steps

end
