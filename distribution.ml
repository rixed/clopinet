open Datatype

(*
   This one is usefull to store the "distribution" of a NUMBER, ie its
   count, min, max, avg and variance
   (std deviation = sqrt(variance/(count-1))
   See Aggregator.distr for an aggregation function.
 *)

include Tuple5.Make (Integer) (Float) (Float) (Float) (Float)

(* Compute the avg and standard deviation using the well known recurrence formulas (where
 * the standard deviation sigma = sqrt(v/(n-1)) *)
let distr x = function
    | None -> 1, x, x, x, 0.
    | Some (n, mi, ma, avg, v) ->
        let n' = n + 1 in
        let xd = x -. avg in
        let avg' = avg +. (xd /. float_of_int n') in
        n',
        min x mi,
        max x ma,
        avg',
        v +. (xd *. (x -. avg'))

let combine ((n, mi, ma, avg, v) as x) = function
    | None -> x
    | Some (n', mi', ma', avg', v') ->
        assert (n' >= 1) ;
        assert (n >= 1) ;
        assert (v >= 0.) ;
        assert (v' >= 0.) ;
        let fn = float_of_int n and fn' = float_of_int n' in
        (* imagine we have each sample sets split in two, with n/2 samples
         * at avg-sigma and n/2 at avg+sigma. It's then easy to combine the two sets. *)
        let sigma = sqrt (v /. fn)
        and sigma'= sqrt (v' /. fn')
        and comb_avg = ((avg *. fn) +. (avg' *. fn')) /. (fn +. fn')
        and sq x = x *. x in
        let comb_q =
            let half_fn = fn/.2. and half_fn' = fn'/.2. in
            let s1 = avg -. sigma  and s2 = avg +. sigma
            and s1'= avg'-. sigma' and s2'= avg'+. sigma' in
            half_fn *. (sq(s1 -. comb_avg) +. sq(s2 -. comb_avg)) +.
            half_fn'*. (sq(s1'-. comb_avg) +. sq(s2'-. comb_avg)) in
        n+n',
        min mi mi',
        max ma ma',
        comb_avg,
        comb_q

let std_dev (c, _mi, _ma, _avg, v) =
    if c > 1 then sqrt(v /. float_of_int (pred c))
    else 0.

