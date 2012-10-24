open Batteries
open Datatype

let sort_pt (x1, _) (x2, _) = compare x1 x2

let hashtbl_update_with_default d h k f =
    match Hashtbl.find_option h k with
    | None -> Hashtbl.add h k d
    | Some v -> Hashtbl.replace h k (f v)

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

(* clip the given y (between t1 and t2) value according to specified time intervals *)
let clip_y ?start ?stop t1 t2 y =
    let t1, y = match start with
        | Some start ->
            if t1 >= start then t1, y
            else start, y -. (Int64.to_float (Int64.div (Int64.sub start t1) (Int64.sub t2 t1)))
        | None -> t1, y in
    let t2, y = match stop with
        | Some stop ->
            if t2 <= stop then t2, y
            else stop, y -. (Int64.to_float (Int64.div (Int64.sub t2 stop) (Int64.sub t2 t1)))
        | None -> t2, y in
    t1, t2, y

module DataSet (Record : DATATYPE) (Key : DATATYPE) =
struct
    module Maplot = Finite_map_impl.Finite_map (struct
        type t = Key.t
        let compare = Key.compare
    end)

    (* fold iterate over the database, while extract extract from a row the key, X start, X stop and Y value. *)
    let per_time ?(max_graphs=10) start stop step fold extract label_of_key =
        assert (step > 0L) ;
        (* Fetch min and max available time *)
        let row_of_time t = Int64.div (Int64.sub t start) step |> Int64.to_int in
        let nb_steps = row_of_time stop |> succ in
        (* Now prepare the datasets as a map of Key.t to array of Y *)
        let flat_dataset () = Array.make nb_steps 0. in
        let cumul_y m k r1 r2 y =
            Maplot.update_with_default_delayed
                (fun () ->
                    let a = flat_dataset () in
                    for x = r1 to r2 do a.(x) <- y done ; (* FIXME: unsafe array set by compilation option? *)
                    a)
                m k
                (fun a ->
                    for x = r1 to r2 do a.(x) <- a.(x) +. y done ;
                    a) in
        let m =
            fold (fun r m ->
                let k, t1, t2, y = extract r in
                (* clip t1 and t2. beware that [t1;t2] is closed while [start;stop[ is semi-closed *)
                (* FIXME: use timestamps comparison function, sub, etc..? *)
                assert (t1 < stop && t2 >= start) ;
                let t1, t2, y = clip_y ~start ~stop t1 t2 y in
                let r1 = row_of_time t1
                and r2 = row_of_time t2 in
                (*
                let check_r r =
                    if r < 0 || r >= nb_steps then Printf.printf "XXX: r=%d while nb_steps=%d\n%!" r nb_steps in
                check_r r1 ; check_r r2 ;*)

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

    (* Fold iterate over the database, while extract from a row the key and Y value.
       All Y values with same key are summed. *)
    (* FIXME: extract should be bundled with fold *)
    let sum ?(max_graphs=10) ?start ?stop fold extract label_of_key =
        ignore (max_graphs) ;
        let m =
            fold (fun r m ->
                let k, t1, t2, y = extract r in
                (* clip t1 and t2. beware that [t1;t2] is closed while [start;stop[ is semi-closed *)
                (* FIXME: use timestamps comparison function, sub, etc..? *)
                let _, _, y = clip_y ?start ?stop t1 t2 y in
                Maplot.update_with_default y m k ((+.) y))
                Maplot.empty
                (fun m1 m2 -> (* merge two maps, m1 being the big one, so merge m2 into m1 *)
                    Maplot.fold_left (fun m k a ->
                        (* add k->a into m *)
                        Maplot.update_with_default a m k ((+.) a))
                        m1 m2) in
        let datasets = Hashtbl.create 71 in
        (* Build hashtables indexed by label (instead of map indexed by some key), and convert Y into Y per second. *)
        Maplot.iter m (fun k a ->
            let label = label_of_key k in
            (* Note that several keys may map to the same label, thus these precautions *)
            hashtbl_update_with_default a datasets label ((+.) a)) ;
        datasets

    module FindSignificant =
    struct
        (* over a collection of (key, value), return
         * return N with the property that, if a key worth more than 1/Nth of the
         * total value then it will be returned.
         * A second pass is required to get the list
         * of the at most N (key, tot value) that are significant enough (according to
         * the definition above), and the tot value of all other keys. *)
        (* We must split the various pass in various functions since ML does not
         * allow to use fold twice with different type for 'a in a single function. *)
        let pass1 fold n =
            assert (n > 0) ;
            (* Helper: given a map m of size s, another key k and value v, add this
             * value to the map (which size must not exceed n), either by updating the
             * previously bound value (if k is already bound in m) or by reducing all
             * values of m until eventualy one reach 0 and we can remove it and insert
             * what's left from k. Return the new map and it's new size. *)
            let update1 m s k v =
                try Maplot.update_exn m k ((+.) v), s
                with Not_found ->
                    if s < n then (
                        Maplot.bind m k v, s+1
                    ) else (
                        (* the map cannot grow: reduce all entries by either v (if it fits) or the min *)
                        (* TODO: a faster way to know the min value stored in m *)
                        let min, _k_min = Maplot.fold_left (fun (min, kmin) k v ->
                            if v < min then v, k else min, kmin)
                            (max_float, k) m in
                        if min >= v then (
                            (* m swallow v entirely *)
                            Maplot.map (fun _k v' -> v' -. v) m, s
                        ) else (
                            (* reduce by min and remove all entries reaching 0 *)
                            let s' = ref s in
                            let m' = Maplot.filter_map (fun _k v' ->
                                if v' > min then Some (v' -. min)
                                            else (decr s' ; None)) m in
                            (* add k with what's left from v *)
                            Maplot.bind m' k (v -. min), !s'+1
                        )
                    ) in
            (* First pass: find all keys that have more than n/Nth of the value (and others) *)
            let result, _s = fold
                (fun (k, v) (m, s) ->
                    update1 m s k v)
                (Maplot.empty, 0)
                (fun (m1, s1) (m2, _s2) ->
                    (* Merge the small m2 into the big m1 *)
                    Maplot.fold_left (fun (m1, s1) k2 v2 ->
                        (* FIXME: we should add k2 to v2 even if m1 grows larger than s1 *)
                        update1 m1 s1 k2 v2)
                        (m1, s1) m2) in
            result

        let pass2 result fold n =
            (* Second pass: Rescan all values, computing total value and total value of selected keys *)
            let zeroed = Maplot.map (fun _k _v -> 0.) result in
            let result, rest = fold
                (fun (k, v) (m, rest) ->
                    try Maplot.update_exn m k ((+.) v), rest
                    with Not_found -> m, rest +. v)
                (zeroed, 0.)
                (fun (m1, rest1) (m2, rest2) ->
                    (* Merge the small m2 into the big m1 *)
                    Maplot.fold_left (fun m1 k2 v2 ->
                        Maplot.update_exn m1 k2 ((+.) v2) (* we *must* have k already in m1 ! *))
                        m1 m2,
                    rest1 +. rest2) in
            (* Final touch: move insignificant keys from result to rest *)
            let tot_v = Maplot.fold_left (fun p _k v -> v +. p) rest result in
            let new_rest = ref rest
            and min_v = tot_v /. (float_of_int n) in (* min value to stay out of "others" *)
            let new_result = Maplot.filter (fun _k v ->
                if v >= min_v then true
                else (
                    new_rest := !new_rest +. v ;
                    false
                )) result in
            new_result, !new_rest
    end
end
