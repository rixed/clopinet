open Printf

(* FIXME: usr Dynlinker.maketuple *)

let maketuple n =
    let foreach f =
        for i = 0 to n-1 do f i done in
    let unfold f =
        let rec aux prevs i =
            if i >= n then List.rev prevs else
            aux ((f i)::prevs) (i+1) in
        aux [] 0 in
    printf "open Datatype\nmodule Make" ;
    foreach (fun i -> printf " (T%d: DATATYPE)" i) ;
    printf " :\n    DATATYPE with type t = " ;
    foreach (fun i -> if i > 0 then printf " * " ; printf "T%d.t" i) ;
    printf " =\nstruct\n    module Tuple%d_base = struct\n    type t = " n ;
    foreach (fun i -> if i > 0 then printf " * " ; printf "T%d.t" i) ;
    printf "\n\n" ;
    let parms c =
        String.concat "," (unfold (fun i -> sprintf "%c%d" c i)) in
    printf "    let equal (%s) (%s) =\n" (parms 'a') (parms 'b') ;
    foreach (fun i -> printf "        " ; if i > 0 then printf "&& " ; printf "T%d.equal a%d b%d\n" i i i) ;
    printf "    let compare (%s) (%s) =\n" (parms 'a') (parms 'b') ;
    foreach (fun i -> printf "        " ;
                      if i < n-1 then printf "let c = T%d.compare a%d b%d in if c <> 0 then c else\n" i i i
                                 else printf "T%d.compare a%d b%d" i i i) ;
    printf "    let hash = Hashtbl.hash\n" ;
    printf "    let name = " ;
    foreach (fun i -> if i > 0 then printf "^\"*\"^" ; printf "T%d.name" i) ;
    printf "\n" ;
    printf "    let samples = []\n" ;
    printf "    let write oc (%s) =\n" (parms 't') ;
    foreach (fun i -> printf "        " ; if i > 0 then printf "; " ; printf "T%d.write oc t%d\n" i i) ;
    printf "    let write_txt oc (%s) =\n" (parms 't') ;
    foreach (fun i -> printf "        " ; if i > 0 then printf "; Output.char oc '\\t' ; " ; printf "T%d.write_txt oc t%d\n" i i) ;
    printf "    let read ic =\n" ;
    foreach (fun i -> printf "        let t%d = T%d.read ic in\n" i i) ;
    printf "        %s\n" (parms 't') ;
    printf "    let to_imm (%s) = \"(\"^" (parms 't') ;
    foreach (fun i -> if i > 0 then printf " ^" ; printf " T%d.to_imm t%d" i i) ;
    printf " ^\")\"\n" ;
    printf "    let parzer ?(picky=false) bs =\n" ;
    printf "        ignore picky ;\n" ;
    printf "        let open Peg in\n" ;
    foreach (fun i ->
        printf "        (match T%d.parzer bs with Fail -> Fail | Res (res%d, bs) ->\n" i i ;
        if i <> n-1 then printf "        (match item '\\t' bs with Fail -> Fail | Res (_, bs) ->\n") ;
    printf "          Res ((" ;
    foreach (fun i -> printf "%sres%d" (if i > 0 then "," else "") i) ;
    printf "), bs)" ;
    foreach (fun i -> printf ")%s" (if i <> n-1 then ")" else "")) ; printf "\n" ;
    printf "    end\n" ;
    printf "    include Tuple%d_base\n" n ;
    printf "    include Datatype_of(Tuple%d_base)\n" n ;
    printf "end\n"

let main =
    if Array.length Sys.argv != 2 then (
        printf "Give me N\n" ;
        exit 1
    ) ;
    maketuple (int_of_string Sys.argv.(1))

