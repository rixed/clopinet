open Batteries
open Graphics

let debug = ref false

module Vector_2D : Cluster.VECTOR with type t = int * int =
struct
    type t = int * int
    let dimension = 2
    let map f (x1,y1) (x2,y2) = f x1 x2, f y1 y2
    let min = map min
    let max = map max
    let square x = x * x
    let surface (x1,y1) (x2,y2) = square (x1-x2) + square (y1-y2)
    let quadrant (xc,yc) (x,y) =
        if x < xc then (
            if y < yc then 3 else 0
        ) else (
            if y < yc then 2 else 1
        )
    let to_string (x,y) = Printf.sprintf "(%d,%d)" x y
end

module Null_Aggr =
struct
    type t = unit
    let add () () = ()
end

module C = Cluster.Make (Vector_2D) (Null_Aggr)

(* [rand_point c r] returns a random point centered at c, at max distance r *)
let rand_point (xc, yc) r =
    let r = Random.float (float_of_int r)
    and a = Random.float (2. *. Float.pi) in
    xc + int_of_float (r *. cos a), yc + int_of_float (r *. sin a)

(* Build a cluster of points centered at c, of max radius r *)
let make_cluster c r =
    Enum.from (fun () -> rand_point c r)

(* Build a dataset with [nc] clusters of approximately [np] points each, centered at [c], in [0;w]^2 *)
let make_dataset w nc np =
    let rec aux nc =
        if nc = 0 then Enum.empty () else
        let cluster_r = 10 + Random.int (w/10) in
        let c = rand_point (w/2,w/2) (w/2 - cluster_r) and n = 5 + Random.int (np*2) in
        Enum.append (Enum.take n (make_cluster c cluster_r)) (aux (pred nc)) in
    aux nc |> Random.shuffle

let draw_point (x,y) =
    draw_circle x y 2

(* Draw the given dataset *)
let draw_dataset d added =
    let grey = rgb 200 200 200 in
    Array.iteri (fun i v ->
        set_color (if added.(i) then black else grey) ;
        draw_point v) d

let draw_node p _q n =
    let open C in
    let x0, y0 = n.min and x1, y1 = n.max and xc, yc = n.center in
    let w = x1-x0+1 and h = y1-y0+1 in
    set_color red ;
    draw_rect x0 y0 w h ;
    set_color green ;
    draw_rect x0 yc w 1 ;
    draw_rect xc y0 1 h ;
    Option.may (fun p ->
        let xp, yp = p.center in
        set_color blue ;
        moveto xc yc ;
        lineto xp yp) p

let display d added c =
    clear_graph () ;
    (* display result *)
    draw_dataset d added ;
    (* and the cluster, blured to more or less max_scarcity *)
    if !debug then Printf.printf "Result tree have %d nodes, max scarcity = %d.\n%!" c.C.nb_nodes c.C.max_scarcity ;
    C.iter draw_node c ;
    (* pause *)
    ignore (read_key ())


let test seed nb_clusters cluster_size max_scarcity max_size max_work_size =
    let max_work_size = Option.default (1 lsl Vector_2D.dimension * max_size) max_work_size in
    Random.init seed ;
    open_graph "" ;
    resize_window 1024 768 ;
    let w = min (size_x ()) (size_y ()) in
    let d = make_dataset w nb_clusters cluster_size in
    let added = Array.(make (length d) false) in
    if !debug then Printf.printf "%d points in dataset\n" (Array.length d) ;

    (* clusterize this dataset *)
    let c = { C.empty () with C.max_scarcity = max_scarcity } in
    let blur_once c =
        let n1 = c.C.nb_nodes in
        C.blur c ;
        if c.C.nb_nodes >= n1 then (
            if c.C.min_next_scarcity < max_int && c.C.min_next_scarcity > c.C.max_scarcity then (
                c.C.max_scarcity <- c.C.min_next_scarcity ;
                c.C.min_next_scarcity <- max_int
            ) else ( (* we have no idea how to increase max_scarcity... *)
                c.C.max_scarcity <- c.C.max_scarcity + 1 (* FIXME: look for best match? *)
            ) ;
            C.blur c
        ) ;
        if !debug then display d added c
    in
    Array.iteri (fun i v ->
        C.add c v 1 () ;
        added.(i) <- true ;
        if !debug then Printf.printf "Added one point\n" ;
        if c.C.nb_nodes > max_work_size then blur_once c
    ) d ;
    while c.C.nb_nodes > max_size do blur_once c done ;
    display d added c

let () =
    let seed = ref 0 and nb_clusters = ref 3 and cluster_size = ref 30
    and max_scarcity = ref 0 and max_size = ref 6 and max_work_size = ref None in
    Arg.(parse [
        "-seed", Set_int seed, "seed for random" ;
        "-nb-clusters", Set_int nb_clusters, "how many clusters in dataset" ;
        "-cluster-size", Set_int cluster_size, "how many points per cluster" ;
        "-max-scarcity", Set_int max_scarcity, "max scarcity" ;
        "-max-size", Set_int max_size, "max final size" ;
        "-max-work-size", Int (fun n -> max_work_size := Some n), "max work size" ;
        "-debug", Set debug, "debug" ]
        (fun x -> raise (Bad x))
        "Test stream clustering") ;
    test !seed !nb_clusters !cluster_size !max_scarcity !max_size !max_work_size
