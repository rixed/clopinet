open Bricabrac

let stacked_area datasets =
    Hashtbl.iter (fun label pts ->
        Printf.printf "%s\n" label ;
        List.iter (fun (x, y) ->
            Printf.printf "%f %f\n" x y) pts ;
        Printf.printf "\n") datasets
