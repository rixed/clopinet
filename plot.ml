open Bricabrac

(* Can be ploted, although no as stacked area, with:
 * plot for [i=0:50] 'traf' index i using 1:2 title columnheader(1) with lines smooth unique *)
let stacked_area datasets =
    Hashtbl.iter (fun label pts ->
        Printf.printf "%s\n" label ;
        List.iter (fun (x, y) ->
            Printf.printf "%f %f\n" x y) pts ;
        Printf.printf "\n\n") datasets
