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
    val surface : t -> t -> int (* the surface spawned by two vectors, as an integer *)
    val quadrant : t -> t -> t -> t -> int (* tells in which "quadrant" a vector is, or -1 if it's within the given min and max *)
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
                  children : node option array (* of size 2^V.dimension *)}

    and tree = { mutable nb_nodes : int ;
                 mutable max_scarcity : int ; (* no node should be more scarce than this - we relax this slowly to meet size requirement *)
                 mutable min_next_scarcity : int ; (* min encountered value of scarcity that's above max_scarcity - useful to grow it later *)
                 mutable root : node option }
    
    let nb_children = 1 lsl V.dimension

    let empty () =
        { nb_nodes = 0 ; max_scarcity = 0 ; min_next_scarcity = max_int ; root = None }

    let is_leaf n =
        not (Array.exists ((<>) None) n.children)

    let scarcity n k c =
        let min' = V.min n.min k
        and max' = V.max n.max k in
        let surf = V.surface min' max'
        and cnt = n.count + c in
        surf / cnt

    (* returns true if n' was actually merged (note: n' MUST BE A LEAF) *)
    let try_merge t n n' =
        (* figure out how scarce the union of n and n' would be *)
        let min' = V.min n.min n'.min
        and max' = V.max n.max n'.max in
        let surf = V.surface min' max'
        and cnt = n.count + n'.count in
        let s = surf / cnt in
        if s <= t.max_scarcity then ( (* do merge *)
            Printf.printf "Merging %d points ([%s:%s], scarcity %d) within %d points ([%s:%s], scarcity %d), new scarcity: %d\n" n'.count (V.to_string n'.min) (V.to_string n'.max) (V.surface n'.min n'.max / n'.count) n.count (V.to_string n.min) (V.to_string n.max) (V.surface n.min n.max / n.count) s ;
            n.count <- cnt ;
            n.aggr <- A.add n.aggr n'.aggr ;
            n.min <- min' ; n.max <- max' ;
            true
        ) else (
            (* update min_next_scarcity *)
            if s < t.min_next_scarcity then t.min_next_scarcity <- s ;
            false
        )

    (* add k, c, a in t, increasing t size as needed. *)
    let add t k c a =
        let make_node k c a =
            { center = k ; min = k ; max = k ;
              count = c ; aggr = a ;
              children = Array.create nb_children None } in

        (* Loop from n to k (following straight path), stopping whenever k is within n own region.
         * At the end, add a new node or merge into best. returns diff in node numbers (ie 0 or 1). *)
        let rec aux n k c a min_scarcity best =
            let q = V.quadrant n.center n.min n.max k in
            if q = -1 then ( (* k is within [min;max] *)
                n.count <- n.count + c ;
                n.aggr <- A.add n.aggr a ;
                0
            ) else (
                assert (q < nb_children) ;
                let s = scarcity n k c in
                let min_scarcity, best =
                    if s < min_scarcity then s, n else min_scarcity, best in
                match n.children.(q) with
                | None ->
                    if min_scarcity <= t.max_scarcity then (
                        (* merge into best *)
                        Printf.printf "Merging new point %s within %d points ([%s:%s], scarcity %d)\n" (V.to_string k) best.count (V.to_string best.min) (V.to_string best.max) min_scarcity ;
                        best.count <- best.count + c ;
                        best.aggr <- A.add best.aggr a ;
                        best.min <- V.min best.min k ;
                        best.max <- V.max best.max k ;
                        0
                    ) else (
                        (* new node *)
                        Printf.printf "New node for single point\n" ;
                        n.children.(q) <- Some (make_node k c a) ;
                        1
                    )
                | Some n' ->
                    aux n' k c a min_scarcity best
            ) in

        let add_root t k c a = match t.root with
            | None ->
                t.root <- Some (make_node k c a) ;
                1
            | Some n ->
                aux n k c a max_int n in
        let diff = add_root t k c a in
        t.nb_nodes <- t.nb_nodes + diff

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

    (* compact t as much as permitted by max_scarcity *)
    let blur t =
        let diff = ref 0 in
        let iter_leaves f t =
            iter (fun p q n -> if is_leaf n then f p q n) t in
        let blur_leaf p q n =
            if p = None then () else
            let p = Option.get p and q = Option.get q in
            (* try to merge n anywhere *)
            try iter (fun _p _q n' ->
                    if n != n' then (
                        if try_merge t n' n then (
                            p.children.(q) <- None ;
                            decr diff ;
                            raise Exit
                        )
                    )
                ) t
            with Exit -> () in
        iter_leaves blur_leaf t ;
        t.nb_nodes <- t.nb_nodes + !diff


end
