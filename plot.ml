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
let top_plot_datasets max_graphs datasets nb_steps label_of_key other_key =
    let max_peak = Array.make (max_graphs-1) None (* ordered by max peak (bigger first) *)
    and others_pts = Array.make nb_steps 0.
    and max_pts pts = Array.fold_left max min_float pts
    and need_others = ref false in
    let add_to_others k =
        need_others := true ;
        Hashtbl.find datasets k |>
        Array.iteri (fun i y ->
            (* add it to others then *)
            others_pts.(i) <- others_pts.(i) +. y) in
    let insert_max max k =
        let rec aux i =
            if i < Array.length max_peak then (
                if (match max_peak.(i) with
                    | None -> true
                    | Some (m, _) -> m < max) then
                (
                    (* make one place by moving last entry into others *)
                    (match max_peak.(Array.length max_peak - 1) with
                        | None -> ()
                        | Some (_m, k) -> add_to_others k) ;
                    (* and scrolling entries *)
                    if i < Array.length max_peak - 1 then
                        Array.blit max_peak i max_peak (i+1) (Array.length max_peak - i - 1) ;
                    max_peak.(i) <- Some (max, k) ;
                    true
                ) else (
                    aux (i+1)
                )
            ) else false in
        aux 0 in
    Hashtbl.iter (fun k pts ->
        let max = max_pts pts in
        if not (insert_max max k) then (
            add_to_others k
        )) datasets ;
    (* recompose datasets from max_peak and others *)
    let rec add_next_dataset prev i =
        if i < 0 then prev else
        add_next_dataset (match max_peak.(i) with
            | None -> prev
            | Some (_max, k) -> (label_of_key k, Hashtbl.find datasets k)::prev)
            (pred i) in
    add_next_dataset
        (if !need_others then [ other_key, others_pts ] else [])
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

let row_of_time start step t =
    Int64.div (Int64.sub t start) step |> Int64.to_int

(* clip the given y (between t1 and t2) value according to specified time intervals.
 * then return the time interval as ints and divide y between all these rows. *)
let clip_y start stop step t1 t2 y =
    let t1, y =
        if t1 >= start then t1, y
        else start, Int64.(to_int (div (mul (of_int y) (sub start t1)) (sub t2 t1))) in
    let t2, y =
        if t2 <= stop then t2, y
        else stop, Int64.(to_int (div (mul (of_int y) (sub t2 stop)) (sub t2 t1))) in
    let r1 = row_of_time start step t1
    and r2 = row_of_time start step t2 in
    (*
    let check_r r =
        if r < 0 || r >= nb_steps then Printf.printf "XXX: r=%d while nb_steps=%d\n%!" r nb_steps in
    check_r r1 ; check_r r2 ;*)
    if r1 = r2 then (
        r1, r2, y
    ) else (
        (* We should split value more accurately here *)
        let dt = r2-r1 in
        let y = (y + (dt/2)) / dt in
        r1, r2-1, y
    )

(* clip the given y (between t1 and t2) value according to specified time intervals.
 * then return the time interval as ints and divide y between all these rows. *)
let clip_y_int start stop step t1 t2 y =
    let t1, y =
        if t1 >= start then t1, y
        else start, y - (Int64.to_int (Int64.div (Int64.sub start t1) (Int64.sub t2 t1))) in
    let t2, y =
        if t2 <= stop then t2, y
        else stop, y - (Int64.to_int (Int64.div (Int64.sub t2 stop) (Int64.sub t2 t1))) in
    let r1 = row_of_time start step t1
    and r2 = row_of_time start step t2 in
    (*
    let check_r r =
        if r < 0 || r >= nb_steps then Printf.printf "XXX: r=%d while nb_steps=%d\n%!" r nb_steps in
    check_r r1 ; check_r r2 ;*)
    if r1 = r2 then (
        r1, r2, y
    ) else (
        (* We should split value more accurately here *)
        let dt = r2-r1 in
        let y = y / dt in
        r1, r2-1, y
    )

(* clip the given y (between t1 and t2) value according to specified time intervals *)
let clip_y_only ?start ?stop t1 t2 y =
    let t1, y = match start with
        | Some start ->
            if t1 >= start then t1, y
            (*FIXME: c'est faux cette formule de clip!!*)
            else start, y - (Int64.to_int (Int64.div (Int64.sub start t1) (Int64.sub t2 t1)))
        | None -> t1, y in
    match stop with
    | Some stop ->
        if t2 <= stop then y
        else y - (Int64.to_int (Int64.div (Int64.sub t2 stop) (Int64.sub t2 t1)))
    | None -> y


(* [fold] iterate over some portion of the database, and call back with
 * timestamp, a distribution, and the previous value.  We are going to
 * accumulate the distribution for each time step, and return an array of
 * distributions. *)
let per_date start stop step fold =
    assert (step > 0L) ;
    (* Fetch min and max available time *)
    let nb_steps = row_of_time start step stop |> succ in
    assert (nb_steps > 0) ;
    let flat_dataset () = Array.make nb_steps Distribution.zero in
    (* accumulation of a distribution into a.(r) *)
    let accum_distr a r d =
        a.(r) <- Distribution.combine d a.(r) in
    fold (fun ts d a ->
        if ts < start || ts >= stop then a else
        let r = row_of_time start step ts in
        accum_distr a r d ;
        a)
        flat_dataset
        (fun a1 a2 -> (* merge array a2 into a1 *)
            Array.iteri (fun r d -> accum_distr a1 r d) a2 ;
            a1)


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

(* Given a fold function yielding key1, key2 (ordered) and user_data,
 * returns a hash of key1 to a hash of key2 to the user_values,
 * aggregated with [aggr user_value1 user_value2]. *)
let netgraph fold aggr =
    let update_h h k1 k2 v =
        let peers = hash_find_or_insert h k1 (fun () ->
            Hashtbl.create 3) in
        hash_update_with_default v
            peers k2
            (aggr v) in
    let update_h_node h k1 peers =
        try let prev_peers = Hashtbl.find h k1 in
            (* merge peers into prev_peers *)
            Hashtbl.iter (fun k2 v ->
                hash_update_with_default v
                    prev_peers k2
                    (aggr v)) peers
        with Not_found ->
            Hashtbl.add h k1 peers in
    fold (fun k1 k2 v h ->
        update_h h k1 k2 v ;
        h)
        (fun () -> Hashtbl.create 31)
        (fun h1 h2 -> (* merge two hashs, h1 being the big one, so merge h2 into h1 *)
            Hashtbl.iter (fun k1 n ->
                (* add k1->n into h1 *)
                update_h_node h1 k1 n) h2 ;
            h1)

(* We often build arrays of Ys, merge them, etc. This representation is supposed
 * to be fast. *)
type y_array = Array of int array
             | Chunk of (int (*start*) * int (*stop*) * int (*value*))
             | Empty

let array_of_chunk nb_steps (r1, r2, y) =
    Array.init nb_steps (fun r -> if r < r1 || r > r2 then 0 else y)

let array_of_y_array nb_steps = function
    | Empty -> Array.make nb_steps 0
    | Chunk c -> array_of_chunk nb_steps c
    | Array a -> a

(* Merge _destructively_ two y_arrays *)
let rec merge_y_array nb_steps ya1 ya2 =
    match ya1, ya2 with
    | Chunk c, Chunk _ ->
        merge_y_array nb_steps (Array (array_of_chunk nb_steps c)) ya2
    | Chunk (r1, r2, y), (Array a as ya)
    | (Array a as ya), Chunk (r1, r2, y) ->
        for r = r1 to r2 do a.(r) <- a.(r) + y done ;
        ya
    | Array a1, Array a2 ->
        for r = 0 to nb_steps-1 do a1.(r) <- a1.(r) + a2.(r) done ;
        ya1
    | Empty, x | x, Empty -> x

(* fold iterate over the database, calling back with the key, X start, X stop and Y value. *)
let per_time ?(max_graphs=10) start stop step fold label_of_key other_key =
    assert (step > 0L) ;
    (* Fetch min and max available time *)
    let nb_steps = row_of_time start step stop |> succ in
    assert (nb_steps > 0) ;
    (* Now prepare the datasets as a map of Key.t to array of Y *)
    let cumul_y m k r1 r2 y =
        Hashtbl.modify_opt k (function
            | None ->
                let a = Array.init nb_steps (fun i -> if i >= r1 && i <= r2 then y else 0) in
                Some a
            | Some a as x ->
                for i = r1 to r2 do Array.unsafe_set a i (Array.unsafe_get a i + y) done ;
                x) m in
    let m =
        fold (fun k t1 t2 y m ->
            (* clip t1 and t2. beware that [t1;t2] is closed while [start;stop[ is semi-closed *)
            (* FIXME: use timestamps comparison function, sub, etc..? *)
            assert (t1 < stop && t2 >= start) ;
            let r1, r2, y = clip_y start stop step t1 t2 y in
            cumul_y m k r1 r2 y ;
            m)
            (fun () -> Hashtbl.create 103)
            (fun m1 m2 -> (* merge two maps, m1 being the big one, so merge m2 into m1 *)
                Hashtbl.iter (fun k a ->
                    (* add k->a into m1 *)
                    Hashtbl.modify_opt k (function
                        | None -> Some a
                        | Some a' as x ->
                            Array.modifyi (fun i y -> y + Array.unsafe_get a i) a' ;
                            x)
                        m1)
                    m2 ;
                m1) in
    let datasets = Hashtbl.create 71
    and step_s = Int64.to_float step /. 1000. in
    (* Build hashtables indexed by label (instead of map indexed by some key), and convert Y into Y per second. *)
    Hashtbl.iter (fun k a ->
        Array.map (fun y -> float_of_int y /. step_s) a |>
        Hashtbl.add datasets k)
        m ;

    (* reduce number of datasets to max_graphs *)
    top_plot_datasets max_graphs datasets nb_steps label_of_key other_key


(* Hashtbl versions of DataSet *)
module FindSignificant =
struct
    (* amongst a collection of (key, value),
     * return N with the property that, if a key worth more than 1/Nth of the
     * total value then it will be returned.
     * A second pass is required to get the list
     * of the at most N (key, tot value) that are significant enough (according to
     * the definition above), and the tot value of all other keys. *)
    let pass1 fold n =
        assert (n > 0) ;
        (* Helper: given a map m of size s, another key k and value v, add this
         * value to the map (which size must not exceed n), either by updating the
         * previously bound value (if k is already bound in m) or by reducing all
         * values of m until eventualy one reach 0 and we can remove it and insert
         * what's left from k. Update m inplace, return both m and its new min. *)
        let update1 (k, v) (m, mi) =
            try let search_min = ref false in
                Hashtbl.modify k (fun v' ->
                if v' = mi then search_min := true ;
                v + v') m ;
                m,
                if !search_min then (
                    (* look for new min *)
                    Hashtbl.fold (fun _k v mi -> min v mi) m max_int
                ) else mi
            with Not_found ->
                if Hashtbl.length m < n then (
                    Hashtbl.add m k v ;
                    m, min mi v
                ) else (
                    (* the map cannot grow: reduce all entries by either v (if it fits) or the min *)
                    if mi > v then (
                        (* m swallow v entirely *)
                        Hashtbl.map_inplace (fun _k v' -> v' - v) m ;
                        m, mi - v
                    ) else (
                        (* reduce by min and remove all entries reaching 0 *)
                        let mi' = ref max_int in
                        Hashtbl.filter_map_inplace (fun _k v' ->
                            if v' > mi then (
                                let new_v' = v' - mi in
                                if new_v' < !mi' then mi' := new_v' ;
                                Some new_v'
                            ) else None)
                            m ;
                        (* add k with what's left from v *)
                        let new_v = v - mi in
                        Hashtbl.add m k new_v ;
                        m, min !mi' new_v
                    )
                ) in
        (* First pass: find all keys that have more than n/Nth of the value (and others) *)
        let result, _mi =
            fold update1
                (fun () -> Hashtbl.create 1000, max_int)
                (fun m_mi1 (m2, _mi2) ->
                    (* Merge the small m2 into the big m1 *)
                    Hashtbl.fold (fun k2 v2 prev1 ->
                        (* FIXME: we should add k2 to v2 even if m1 grows larger than s1 *)
                        update1 (k2, v2) prev1)
                        m2 m_mi1) in
        result

    (* Second pass: Rescan all values, computing final value of selected keys.
     * This time the fold function returns a third parameter: the total value
     * for this entry. Total values are agregated using [tv_aggr] function.
     * See below if you do not need a total value different from v (so that
     * you can reuse the same fold function than the one for pass1) *)
    let pass2 result fold tv_aggr tv_zero n =
        let zeroed = Hashtbl.map (fun _k _v -> 0, tv_zero) result in
        let result, rest, tv_rest = fold
            (fun (k, v, tv) (m, rest, tv_rest) ->
                try Hashtbl.modify k (fun (v', tv') ->
                    v + v', tv_aggr tv tv') m ;
                    m, rest, tv_rest
                with Not_found ->
                    m, v + rest, tv_aggr tv tv_rest)
            (fun () -> Hashtbl.copy zeroed, 0, tv_zero)
            (fun (m1, rest1, tv_rest1) (m2, rest2, tv_rest2) ->
                (* Merge the small m2 into the big m1 *)
                Hashtbl.iter (fun k2 (v2, tv2) ->
                    Hashtbl.modify k2 (fun (v', tv') ->
                        v' + v2, tv_aggr tv' tv2) m1 (* we *must* have k already in m1 ! *))
                    m2 ;
                m1, rest1 + rest2, tv_aggr tv_rest1 tv_rest2) in
        (* Final touch: move insignificant keys from result to rest *)
        let tot_v = Hashtbl.fold (fun _k (v, _tv) p -> v + p) result rest in
        let new_rest = ref rest
        and new_tv_rest = ref tv_rest
        and min_v = tot_v / n in (* min value to stay out of "others" *)
        let new_result = Hashtbl.filter (fun (v, tv) ->
            if v >= min_v then true
            else (
                new_rest := !new_rest + v ;
                new_tv_rest := tv_aggr !new_tv_rest tv ;
                false
            )) result in
        new_result, !new_rest, !new_tv_rest

    (* Same as pass2, but with no additional value *)
    let pass2' result fold n =
        let fold' f i m = fold (fun (k, v) p ->
            f (k, v, ()) p) i m
        and fake_aggr () () = () in
        let new_result, new_rest, () = pass2 result fold' fake_aggr () n in
        Hashtbl.map (fun _k (v, ()) -> v) new_result, new_rest
end

(* Distributions of response times: *)
let distributions_of_response_times prec m rest_rts label_of_key =
    let distrib_of_rts rts =
        let mi, ma =
            List.fold_left (fun (mi, ma) rt ->
                min mi rt, max ma rt)
                (max_float, min_float)
                rts in
        (* return the prec interval in which to store a RT *)
        let floor_rt rt = rt /. prec |> int_of_float in
        let mi = floor_rt mi and ma = floor_rt ma in
        let nb_buckets = ma - mi |> succ in
        let d = Array.create nb_buckets 0 in
        List.iter
            (fun rt ->
                let i = floor_rt rt - mi in
                d.(i) <- succ d.(i))
            rts ;
        mi, ma, d in
    let other_mi, other_ma, other_d = distrib_of_rts rest_rts in
    let mi, ma, d =
        Hashtbl.fold (fun k (_, rts) (prev_mi, prev_ma, prev_d) ->
            let mi, ma, d = distrib_of_rts rts in
            let label = label_of_key k in
            min prev_mi mi,
            max prev_ma ma,
            (label, mi, d) :: prev_d)
            m
            (other_mi, other_ma, [ "others", other_mi, other_d ])
    in
    prec, mi, ma, d

(* Return a mere list of label -> float array from a Maplot of key -> (_ * y_array) *)
let arrays_of_volume_chunks step nb_steps vols rest_vols label_of_key =
    let step_s = Int64.to_float step /. 1000. in
    let to_scaled_array ya =
        let a = array_of_y_array nb_steps ya in
        (* while we are at it, convert from bytes to bytes/secs *)
        Array.map (fun y -> float_of_int y /. step_s) a in
    Hashtbl.fold (fun k (_, ya) prev ->
        (label_of_key k, to_scaled_array ya) :: prev)
        vols ["others", to_scaled_array rest_vols]

let grid_interv n start stop =
    let dv = stop -. start in
    (* find the round value closest to dv/n (by round we mean 1, 5, 10...) *)
    let l = dv /. float_of_int n in
    let f = 10. ** floor (log10 l) in
    let i = floor (l /. f) in
    if i < 2.5 then f else
    if i < 7.5 then 5. *. f else
    10. *. f

(* Given a range of values [start:stop], returns an enum of approximatively [n] round intermediate values *)
let grid n start stop =
    let interv = grid_interv n start stop in
    let lo = interv *. floor (start /. interv) in
    Enum.seq lo ((+.) interv) ((>=) stop)

(* Ascii Art *)

let print_tops tops =
    let print_array a =
        Array.iter (fun s ->
            Printf.printf "%s\t" s) a in
    List.iter (fun (k, v, s_mi, s_ma) ->
        (match k with
        | Some k -> print_array k
        | None   -> Printf.printf "\tothers\t") ;
        print_array v ;
        Printf.printf "\t%s\n" (Datatype.string_of_min_max (float_of_int s_mi) (float_of_int s_ma))) tops

(* Heavy hitters *)

(* Here we do not read k ones by ones but instead read v ks at every step. This
   is mostly equivalent, only faster and more precise. v is an integer we use to
   count the ks, but we also manipulate tv wich are anything that the user want
   to aggregate over the key.

   For generality, we are given:
  - the max number of tracked keys (should be slightly greater than number of
    expected big hitters)
  - a fold function that iterates over the streams, calling our function with k, v, and tv.
  - a function to aggregate two tv
  - the zero value for a tv (ie. neutral via the aggregation function above)
  We return a hashtbl of key to (v,tv,r) (where r is the max amount of "missed" values)
  Merging of two results also make things more complex (we add v+r ks).
  We also compute the total v and tv.
*)
(* TODO: produce also total v and tv, and move into 'others' alls ks that do not hit hard enough *)
let heavy_hitters max_len fold tv_aggr tv_zero =
    assert (max_len > 0) ;
    (* Helper: given a hashtbl h, a total v since begening tot_v,
     * a key k and value v and tv, add this item to the hash (which size
     * must not exceed max_len), either by updating the previously bound value
     * or by replacing the smaller value.  Update h inplace, returns both h and
     * its new min and new tot_v, to be used in a fold. *)
    (* o: number of past untracked items - always grow but when we got a tracked key.
     * pvs : number of past tracked items
     * n: number of past items
     * k_min: key of the minimum value of v0+v in h
     * h: hash from k to (v: number of ks, tv: user value, aggregated,
     *                    v0: value of o when k entered h (so that to expel k from h
     *                    o must raise above v0+v), n0: value of n when k entered
     *                    the h, so that at the end we know that we had no more
     *                    and no less than v ks in the last n-n0 items)
     *)
    let find_min h =
        Hashtbl.fold (fun k (v, _tv, v0, _n0) (_,mi as mi_l) ->
            if v+v0 < mi then Some k, v+v0 else mi_l)
            h (None, max_int) |>
        fst in
    fold (fun (k, v, tv) (h, o, pvs, n, rest_v, rest_tv, k_min) -> (* update *)
        assert (n >= pvs) ;
        try Hashtbl.modify k (fun (v', tv', v0, n0) ->
                v + v', tv_aggr tv' tv, v0, n0) h ;
            h, o, pvs+v, n+v, rest_v, rest_tv, if k_min = Some k then find_min h else k_min
        with Not_found ->
            if Hashtbl.length h < max_len then (
                (* the map is allowed to grow -> o is still 0 at this stage *)
                assert (o = 0) ;
                Hashtbl.add h k (v, tv, o, 0) ;
                h, o, pvs+v, n+v, rest_v, rest_tv,
                match k_min with
                | Some k_mi -> let v_min,_,_,_ = Hashtbl.find h k_mi in
                               if v < v_min then Some k else k_min
                | _ -> Some k
            ) else (
                let v_min, tv_min, v0_min, _n0_min = Hashtbl.find h (BatOption.get k_min) in
                (* the map is not allowed to grow *)
                if v0_min + v_min >= o + v then (
                    (* no need to track k *)
                    h, o+v, pvs, n+v, rest_v+v, tv_aggr rest_tv tv, k_min
                ) else (
                    (* k may be more numerous than k_min, replace k_min with k *)
                    Hashtbl.remove h (BatOption.get k_min) ;
                    let max_untracked = min (n-pvs) (* all currently tracked ks were not k *)
                                            (n/max_len) (* or it would be tracked already *) in
                    Hashtbl.add h k (v, tv, o, max_untracked) ;
                    h, v0_min + v_min, pvs+v-v_min, n+v, rest_v+v_min, tv_aggr rest_tv tv_min, find_min h
                )
            ))
        (fun () -> (* zero *)
            Hashtbl.create max_len, 0, 0, 0, 0, tv_zero, None)
        (fun (_h1, _o1, pvs1, n1, _rv1, _rtv1, _kmin1 as h1_etc) (h2, _o2, pvs2, n2, rv2, rtv2, _kmin2) -> (* merge the small h2 into the big h1 *)
            (* max number of a k that's not in h1 *)
            let max_untracked1 = min (n1-pvs1) (n1/max_len)
            and max_untracked2 = min (n2-pvs2) (n2/max_len) in
            let h1', o1', pvs1', n1', rv1', rtv1', kmin1' = Hashtbl.fold (fun k2 (v2, tv2, _v02, n02) (h1, o1, pvs1, n1, rv1, rtv1, k_min1) ->
                assert (n1 >= pvs1) ;
                try Hashtbl.modify k2 (fun (v', tv', v0, n0) ->
                        v' + v2, tv_aggr tv' tv2, v0, n0 + n02) h1 ;
                    (* beware: n is only n1+v2 and not n1+v2+n02 since n02 is the number of k2s
                     * we might have missed before tracking k2 in h2 and not a number of items
                     * untracked since last update *)
                    h1, o1, pvs1+v2, n1+v2, rv1, rtv1, if k_min1 = Some k2 then find_min h1 else k_min1
                with Not_found ->
                    if Hashtbl.length h1 < max_len then (
                        (* the map is allowed to grow -> o1 is still 0 at this stage *)
                        assert (o1 = 0) ;
                        Hashtbl.add h1 k2 (v2, tv2, o1, max_untracked1 + n02) ;
                        h1, o1, pvs1+v2, n1+v2, rv1, rtv1,
                        match k_min1 with
                        | Some k_mi -> let v_min,_,_,_ = Hashtbl.find h1 k_mi in
                                       if v2 < v_min then Some k2 else k_min1
                        | _ -> Some k2
                    ) else (
                        let v_min, tv_min, v0_min, _n0_min = Hashtbl.find h1 (BatOption.get k_min1) in
                        (* the map is not allowed to grow *)
                        if v0_min + v_min >= o1 + v2 then (
                            (* no need to track k2 *)
                            h1, o1+v2, pvs1, n1+v2, rv1+v2, tv_aggr rtv1 tv2, k_min1
                        ) else (
                            (* k2s may be more numerous than k_min1, replace k_min1 with k2 *)
                            Hashtbl.remove h1 (BatOption.get k_min1) ;
                            Hashtbl.add h1 k2 (v2, tv2, o1, max_untracked1 + n02) ;
                            h1, v0_min + v_min, pvs1+v2-v_min, n1+v2, rv1+v_min, tv_aggr rtv1 tv_min, find_min h1
                        )
                    )) h2 h1_etc in
            (* For all k1 not also in k2, then add max_untracked2 into n01 *)
            Hashtbl.map_inplace (fun k1 (v1, tv1, o1, n01 as t1) ->
                if Hashtbl.mem h2 k1 then t1 else v1, tv1, o1, n01 + max_untracked2) h1' ;
            h1', o1', pvs1', n1'+n2-pvs2, rv1' + rv2, tv_aggr rtv1' rtv2, kmin1')

