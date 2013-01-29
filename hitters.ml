(* Find heavy hitters in one pass, with approx results *)
open Batteries

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
let big_hitters max_len fold tv_aggr =
    assert (max_len > 0) ;
    (* Helper: given a hashtbl h, a total v since begening tot_v,
     * a key k and value v and tv, add this item to the hash (which size
     * must not exceed max_len), either by updating the previously bound value
     * or by replacing the smaller value.  Update h inplace, returns both h and
     * its new min and new tot_v, to be used in a fold. *)
    (* o: number of past untracked items - always grow but when we got a tracked key.
     * n: number of past items
     * ki,mi: minimum value in h (ki being the key, mi being the min of v+v0)
     * h: hash from k to (v: number of ks, tv: user value, aggregated,
     *                    v0: value of o when k entered h (so that to expel k from h
     *                    o must raise to v0+v), n0: value of n when k entered
     *                    the h, so that at the end we know that we had no more
     *                    and no less than v ks in the last n-n0 items)
     *)
    let update (k, v, tv, not_v) (h, o, n, rest_v, rest_tv, k_min) =
        let find_min h =
            Hashtbl.fold (fun k (v, _tv, v0, _n0) (_,mi as mi_l) ->
                if v+v0 < mi then Some k, v+v0 else mi_l)
                h (None, max_int) |>
            fst in
        try Hashtbl.modify k (fun (v', tv', v0, n0) ->
                v + v', tv_aggr tv' tv, v0, n0 - not_v) h ;
            h, o, n+v, rest_v, rest_tv, if k_min = Some k then find_min h else k_min
        with Not_found ->
            let v_min, tv_min, v0_min, _n0_min = Hashtbl.find h k in
            if Hashtbl.length h < max_len then (
                (* the map is allowed to grow -> o is still 0 at this stage *)
                assert (o = 0) ;
                Hashtbl.add h k (v, tv, o, n - not_v) ;
                h, o, n+v, rest_v, rest_tv, if v < v_min then Some k else k_min
            ) else (
                (* the map is not allowed to grow *)
                if v0_min + v_min >= o + v then (
                    (* no need to track k *)
                    h, o+v, n+v, rest_v+v, tv_aggr rest_tv tv, k_min
                ) else (
                    (* k may be more numerous than k_min, replace k_min with k *)
                    Hashtbl.remove h (Option.get k_min) ;
                    Hashtbl.add h k (v, tv, o, n - not_v - v_min (* additional bonus: those k_mins were not ks *)) ;
                    h, o, n+v, rest_v+v_min, tv_aggr rest_tv tv_min,  find_min h
                )
            ) in
    fold update
        (fun () -> Hashtbl.create max_len, 0, 0, (max_int, None))
        (fun (_h1, _o1, n1, _rv1, _rtv1, _kmin1 as h1_etc) (h2, _o2, n2, rv2, rtv2, _kmin2) ->
            (* Merge the small h2 into the big h1 *)
            let h1', o1', n1', rv1', rtv1', kmin1' = Hashtbl.fold (fun k2 (v2, tv2, _v02, n02) h1_etc ->
                let not_k2 = n2 - n02 in (* there were n2-n02 non-k2 before... *)
                update (k2, v2, tv2, not_k2) h1_etc)
                h2 h1_etc in
            (* we already reported the ks and known non-ks, we must now account for the
             * others (up to n2). Ideally we'd like to add them to n1 but that would make
             * all n0s wrong. *)
            assert (n1' <= n1+n2) ;
            let unknowns = n1+n2 - n1' in
            (* so we merely offset each k in h1 - faster alternative: add yet another counter to h1_etc *)
            Hashtbl.map_inplace (fun _k (v, tv, v0, n0) -> (v, tv, v0, n0 + unknowns)) h1' ;
            h1', o1', n1', rv1' + rv2, tv_aggr rtv1' rtv2, kmin1')

