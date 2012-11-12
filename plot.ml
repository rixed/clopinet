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

(* [fold] iterate over some portion of the database, and call back with
 * timestamp, a distribution, and the previous value.  We are going to
 * accumulate the distribution for each time step, and return an array of
 * distributions. *)
let per_date start stop step fold =
    assert (step > 0L) ;
    (* Fetch min and max available time *)
    let row_of_time t = Int64.div (Int64.sub t start) step |> Int64.to_int in
    let nb_steps = row_of_time stop |> succ in
    assert (nb_steps > 0) ;
    let flat_dataset () = Array.make nb_steps None in
    (* accumulation of a distribution into a.(r) *)
    let accum_distr a r d =
        a.(r) <- Some (Distribution.combine d a.(r)) in
    let result = fold (fun ts d a ->
        if ts < start || ts >= stop then a else
        let r = row_of_time ts in
        accum_distr a r d ;
        a)
        flat_dataset
        (fun a1 a2 -> (* merge array a2 into a1 *)
            Array.iteri (fun r d -> match d with
                | Some d -> accum_distr a1 r d
                | None   -> ()) a2 ;
            a1) in
    (* Convert microseconds into seconds *)
    let m2s m = m /. 1_000_000. in
    let microseconds_to_seconds = function
        | None -> None
        | Some (c, mi,ma,a,v) ->
            Some (c, m2s mi, m2s ma, m2s a, m2s (m2s v)) in
    Array.map microseconds_to_seconds result


type sort_order = Asc | Desc
let top_table n sort_order cmp fold =
    let cmp = match sort_order with
        | Asc -> cmp
        | Desc -> fun a b -> cmp b a in
    let init_value () = Array.create n None in
    let is_smaller v = function
        | None -> true
        | Some v' -> cmp v v' < 0 in
    let merge_tops t1 t2 =
        let ret = init_value () in
        let rec m s1 s2 d =
            if d < n then (
                match t1.(s1) with
                | None ->
                    ret.(d) <- t2.(s2) ;
                    m s1 (succ s2) (succ d)
                | Some v1 as x ->
                    if is_smaller v1 t2.(s2) then (
                        ret.(d) <- x ;
                        m (succ s1) s2 (succ d)
                    ) else (
                        ret.(d) <- t2.(s2) ;
                        m s1 (succ s2) (succ d)
                    )
            ) in
        m 0 0 0 ;
        ret in
    let add_value v tops =
        if is_smaller v tops.(n-1) then (
            (* look for first top not smaller than v *)
            let rec look_bigger i =
                if is_smaller v tops.(i) then i else look_bigger (succ i) in
            let insert_pos = look_bigger 0 in
            (* make room in tops array *)
            Array.blit tops insert_pos tops (succ insert_pos) (n-1 - insert_pos) ;
            (* insert v *)
            tops.(insert_pos) <- Some v
        ) ;
        tops in
    fold add_value init_value merge_tops

(* TODO: update functions such as this should go into batteries *)
let hash_find_or_insert h k f =
    try Hashtbl.find h k
    with Not_found -> (
        let v = f () in
        Hashtbl.add h k v ;
        v
    )
let hash_update_with_default def h k f =
    try let prev = Hashtbl.find h k in
        Hashtbl.replace h k (f prev)
    with Not_found ->
        Hashtbl.add h k def

(* returns a hash of node *)
let netgraph ?min_volume fold =
    let update_h h k1 k2 v =
        let peers = hash_find_or_insert h k1 (fun () ->
            Hashtbl.create 3) in
        hash_update_with_default v
            peers k2
            ((+.) v) in
    let update_h_node h k1 peers =
        try let prev_peers = Hashtbl.find h k1 in
            (* merge peers into prev_peers *)
            Hashtbl.iter (fun k2 v ->
                hash_update_with_default v
                    prev_peers k2
                    ((+.) v)) peers
        with Not_found ->
            Hashtbl.add h k1 peers in
    let graph = fold (fun k1 k2 v h ->
        update_h h k1 k2 v ;
        h)
        (fun () -> Hashtbl.create 31)
        (fun h1 h2 -> (* merge two hashs, h1 being the big one, so merge h2 into h1 *)
            Hashtbl.iter (fun k1 n ->
                (* add k1->n into h1 *)
                update_h_node h1 k1 n) h2 ;
            h1) in
    match min_volume with
    | None ->
        graph
    | Some min_volume ->
        let min_volume = float_of_int min_volume in
        Hashtbl.filter_map (fun _k1 n ->
            let n' = Hashtbl.filter (fun y -> y >= min_volume) n in
            if Hashtbl.is_empty n' then None
            else Some n') graph

module DataSet (Key : DATATYPE) =
struct
    module Maplot = Finite_map_impl.Finite_map (struct
        type t = Key.t
        let compare = Key.compare
    end)

    (* fold iterate over the database, calling back with the key, X start, X stop and Y value. *)
    let per_time ?(max_graphs=10) start stop step fold label_of_key =
        assert (step > 0L) ;
        (* Fetch min and max available time *)
        let row_of_time t = Int64.div (Int64.sub t start) step |> Int64.to_int in
        let nb_steps = row_of_time stop |> succ in
        assert (nb_steps > 0) ;
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
            fold (fun k t1 t2 y m ->
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
                    let dt = r2-r1 |> float_of_int in
                    let y' = y /. dt in
                    cumul_y m k r1 (r2 - 1) y'
                ))
                (fun () -> Maplot.empty)
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

    (* Fold iterate over the database, calling back with the key and Y value.
       All Y values with same key are summed. *)
    let sum ?(max_graphs=10) ?start ?stop fold label_of_key =
        ignore (max_graphs) ;
        let m =
            fold (fun k t1 t2 y m ->
                (* clip t1 and t2. beware that [t1;t2] is closed while [start;stop[ is semi-closed *)
                (* FIXME: use timestamps comparison function, sub, etc..? *)
                let _, _, y = clip_y ?start ?stop t1 t2 y in
                Maplot.update_with_default y m k ((+.) y))
                (fun () -> Maplot.empty)
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
        (* amongst a collection of (key, value),
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
                (fun () -> Maplot.empty, 0)
                (fun (m1, s1) (m2, _s2) ->
                    (* Merge the small m2 into the big m1 *)
                    Maplot.fold_left (fun (m1, s1) k2 v2 ->
                        (* FIXME: we should add k2 to v2 even if m1 grows larger than s1 *)
                        update1 m1 s1 k2 v2)
                        (m1, s1) m2) in
            result

        let pass2 result fold n =
            (* Second pass: Rescan all values, computing total value and total value of selected keys *)
            let zeroed () = Maplot.map (fun _k _v -> 0.) result, 0. in
            let result, rest = fold
                (fun (k, v) (m, rest) ->
                    try Maplot.update_exn m k ((+.) v), rest
                    with Not_found -> m, rest +. v)
                zeroed
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
