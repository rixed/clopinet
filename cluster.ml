(* Homemade clustering stream algorithm:
 * we build an n-dimensional tree (n-tree) storing min/max and number of items,
 * suitable for items of dimension log2(n).
 * We keep scarcity (inverse of density) below a given limit and limit the size of
 * the tree. *)

open Batteries

module type VECTOR =
sig
    type t
    val dimension : int (* how many dimensions do we have *)
    val min : t -> t -> t (* the min (dimension by dimension) of two vectors *)
    val max : t -> t -> t (* the max od two vectors *)
    val eval : t -> t -> int -> float (* the evaluation function for cluster min,max,count *)
    val quadrant : t -> t -> int (* tells in which "quadrant" a vector is *)
    val to_string : t -> string
end

module type AGGR =
sig
    type t
    val add : t -> t -> t (* aggregate two values together *)
end

module Make (V : VECTOR) (A: AGGR) =
struct

    type node = { center : V.t ;
                  mutable min : V.t ; (* with the idea that for any t, we have a distance function from t*t to int *)
                  mutable max : V.t ;
                  mutable count : int ;
                  mutable aggr : A.t ;
                  children : node option array (* of size 2^V.dimension *) ;
                  id : int }

    and tree = { mutable nb_nodes : int ;
                 mutable max_degr : float ; (* a merge that does not degrade the position by more than this is performed without further examination *)
                 mutable min_next_degr : float ; (* min encountered value of degr that's above max_degr - useful to grow it later *)
                 mutable root : node option }

    let nb_children = 1 lsl V.dimension

    let empty () =
        { nb_nodes = 0 ; max_degr = 0. ; min_next_degr = max_float ; root = None }

    let seq = ref 0

    let make_node k c a =
        incr seq ;
        { center = k ; min = k ; max = k ;
          count = c ; aggr = a ;
          children = Array.create nb_children None ;
          id = !seq }

    (* Utilities *)

    let iter f t =
        let rec aux p q = function
            | None -> ()
            | Some n ->
                for q = 0 to nb_children-1 do
                    aux (Some n) (Some q) n.children.(q)
                done ;
                f p q n
        in
        aux None None t.root

    let rec nb_nodes m =
        let n = ref 1 in
        for q = 0 to nb_children-1 do match m.children.(q) with
            | None -> ()
            | Some n' -> n := !n + nb_nodes n'
        done ;
        !n

    let length t =
        match t.root with None -> 0 | Some n -> nb_nodes n

    let eval_node n = V.eval n.min n.max n.count

    let degr n m =
        let min' = V.min n.min m.min
        and max' = V.max n.max m.max in
        let e = V.eval min' max' (m.count + n.count) in
        let d = eval_node n +. eval_node m -. e in
        Printf.printf "merging %d and %d would cost %f\n" n.id m.id d ;
        d

    let best_merge_fast t m =
        let rec aux n m min_degr best =
            let d = degr n m in
            let min_degr, best =
                if d < min_degr then d, n else min_degr, best in
            let q = V.quadrant n.center m.center in
            assert (q < nb_children) ;
            match n.children.(q) with
            | None    -> min_degr, best, n, q
            | Some n' -> aux n' m min_degr best in
        Option.map (fun n -> aux n m max_float n) t.root

    let best_merge_slow t m =
        let rec aux n_is_root n m min_degr best =
            let min_d, best =
                if n_is_root || n.id < m.id then (
                    let d = degr n m in
                    if d < min_degr then d, n else min_degr, best
                ) else min_degr, best in
            Array.fold_left (fun (min_d, b as min_d_b) c -> match c with
                | None    -> min_d_b
                | Some n' ->
                    let min_d', b' = aux false n' m min_d b in
                    if min_d' < min_d then min_d', b' else min_d_b) (min_d, best) n.children
        in
        Option.map (fun n -> aux true n m max_float n) t.root

    (* add node m in t, increasing t size as needed. *)
    let rec add_node t m =
        match best_merge_fast t m with
        | None ->
            assert (t.nb_nodes = 0) ;
            t.root <- Some m ;
            t.nb_nodes <- nb_nodes m ;
        | Some (min_degr, best, leaf, q) ->
            if min_degr <= t.max_degr then (
                (* merge into best *)
                Printf.printf "Merging node with %d points within %d points ([%s:%s], %g)\n" m.count best.count (V.to_string best.min) (V.to_string best.max) min_degr ;
                best.count <- best.count + m.count ;
                best.aggr <- A.add best.aggr m.aggr ;
                best.min <- V.min best.min m.min ;
                best.max <- V.max best.max m.max ;
                for q=0 to nb_children-1 do match m.children.(q) with
                    | None -> ()
                    | Some m' ->
                        m.children.(q) <- None ;
                        add_node t m'
                done
            ) else (
                (* new node *)
                Printf.printf "New leaf for node with %d points\n" m.count ;
                leaf.children.(q) <- Some m ;
                t.nb_nodes <- t.nb_nodes + nb_nodes m
            )

    (* add k, c, a in t, increasing t size as needed. *)
    let add t k c a =
        add_node t (make_node k c a) ;
        assert (length t = t.nb_nodes)

    (* compact t by one *)
    let blur t =
        assert (t.root <> None) ;
        assert (length t = t.nb_nodes) ;
        let min_d = ref max_float and best = ref None in
        let blur_one p q n =
            if p = None then () (* dont try to suppress root *) else (
                let p = Option.get p and q = Option.get q in
                match best_merge_slow t n with
                | None -> assert false
                | Some (min_d', best') ->
                    if min_d' < !min_d then (
                        min_d := min_d' ;
                        best := Some (p, q, n, best')
                    )
            ) in
        Printf.printf "wanna blur %d nodes\n" t.nb_nodes ;
        iter blur_one t ;
        match !best with
        | None ->
            Printf.printf "Cannot blur anything!\n"
        | Some (p, q, n, b) ->
            (* merge n, qth child of p, into node b (and dispose of its children) *)
            Printf.printf "Bluring %d points from %d ([%s:%s], %g) within %d points from %d ([%s:%s], %g) -> %g\n" n.count n.id (V.to_string n.min) (V.to_string n.max) (eval_node n) b.count b.id (V.to_string b.min) (V.to_string b.max) (eval_node b) (degr n b) ;
            b.count <- b.count + n.count ;
            b.aggr <- A.add b.aggr n.aggr ;
            b.min <- V.min b.min n.min ;
            b.max <- V.max b.max n.max ;
            p.children.(q) <- None ;
            t.nb_nodes <- t.nb_nodes - 1 ;
            for q'=0 to nb_children-1 do match n.children.(q') with
                | None -> ()
                | Some m ->
                    n.children.(q') <- None ;
                    t.nb_nodes <- t.nb_nodes - nb_nodes m ;
                    add_node t m
            done

end
