open Batteries

exception Cannot_compile of string * string

let load_string str =
    let fname =
        File.with_temporary_out ~prefix:"clopinet_" ~suffix:".ml" ~mode:[`create;(*`delete_on_exit;*)`text]
            (fun oc fname ->
                IO.nwrite oc str ;
                fname) in
    let cmxs = Dynlink.adapt_filename fname in
    let cmd = Printf.sprintf "PATH=/bin:/usr/bin:/home/rixed/ocalme/bin OCAMLPATH=/home/rixed/share/src /home/rixed/ocalme/bin/ocamlfind ocamlopt -o %s -package clopinet -S -inline 9 -shared %s" cmxs fname in
    match Unix.system cmd with
    | Unix.WEXITED 0 ->
        (try Dynlink.loadfile cmxs
             (* TODO: rm fname and cmxs *)
        with (Dynlink.Error e) as exn ->
            Printf.fprintf stderr "%s\n%!" (Dynlink.error_message e) ;
            raise exn)
    | _ ->
        raise (Cannot_compile (fname, cmd))

let check opt_v to_imm fmt =
    Option.map (fun v -> Printf.sprintf fmt (to_imm v)) opt_v

let ocaml_of_user_filter = function
    | None      -> "true"
    | Some expr -> User_filter.ocaml_of_expr expr

let load_filter module_name usr_fields ?usr_filter checks =
    let param_names = List.map fst usr_fields |> String.join "," in
    (* We have two filters combined: one from preset filter terms, and the free usr_filter, ANDed *)
    let checks =
        List.fold_left (fun p ->
            function None -> p | Some x -> x::p) [] checks |>
        List.rev in
    let preset_filter =
        if checks = [] then "true" else
        String.concat " && " checks in
    let os = IO.output_string () in
    Printf.fprintf os "\
open Batteries
let () =\n\
    %s.set_filter (fun (%s) ->\n\
        (* user free filters *)\n\
        %s &&\n\
        (* preset filters *)\n\
        %s)\n"
            module_name param_names
            (ocaml_of_user_filter usr_filter) preset_filter ;
    IO.close_out os |>
    load_string

open Datatype
open Metric

let maketuple name datatypes =
    let str = IO.output_string () in
    let printf fmt = Printf.fprintf str fmt in
    let n = List.length datatypes in
    let foreach f =
        for i = 0 to n-1 do f i done in
    let unfold f =
        let rec aux prevs i =
            if i >= n then List.rev prevs else
            aux ((f i)::prevs) (i+1) in
        aux [] 0 in
    printf "module %s =\nstruct\n" name ;
    List.iteri (fun i dt ->
        printf "    module T%d = %s\n" i dt) datatypes ;
    printf "    module %s_base = struct\n    type t = " name ;
    foreach (fun i -> if i > 0 then printf " * " ; printf "T%d.t" i) ;
    printf "\n\n" ;
    let parms c =
        String.concat "," (unfold (fun i -> Printf.sprintf "%c%d" c i)) in
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
    printf "    include %s_base\n" name ;
    printf "    include Datatype.Datatype_of(%s_base)\n" name ;
    printf "end\n" ;
    IO.close_out str

let load_top_two_pass modname fields ?start ?stop ?ip_src ?usr_filter ~max_graphs sort_by key_fields aggr_fields dbdir name =
"open "^ modname ^"
open Batteries

let top () =
    "^ (start |> BatOption.map (fun s -> "let min_start = "^ (s |> Timestamp.to_imm) ^" in") |> BatOption.default "") ^"
    "^ (stop  |> BatOption.map (fun s -> "let max_stop  = "^ (s |> Timestamp.to_imm) ^" in") |> BatOption.default "") ^"
    "^ (ip_src|> BatOption.map (fun i -> "let req_ip_src = "^ (i |> Cidr.to_imm) ^" in") |> BatOption.default "") ^"
    let fold1 f i m =
        "^ modname ^".fold_all
            "^ (start |> BatOption.map (fun _ -> "~start:min_start") |> BatOption.default "") ^"
            "^ (stop  |> BatOption.map (fun _ -> "~stop:max_stop") |> BatOption.default "") ^"
            "^ (ip_src|> BatOption.map (fun _ -> "~ip_src:req_ip_src") |> BatOption.default "") ^"
            "^ Text.to_imm dbdir ^" "^ Text.to_imm name ^"
            (fun ("^ (fields |> List.map (fun (n,_f) -> n) |> String.concat ", ") ^") p ->
                if "^ (usr_filter |> BatOption.map User_filter.ocaml_of_expr |> BatOption.default "true") ^" &&
                   "^ (start |> BatOption.map (fun _ -> "Datatype.Timestamp.compare stop min_start >= 0") |> BatOption.default "true") ^" &&
                   "^ (stop  |> BatOption.map (fun _ -> "Datatype.Timestamp.compare max_stop start > 0") |> BatOption.default "true") ^" &&
                   "^ (ip_src|> BatOption.map (fun _ -> "Datatype.in_cidr ip_src req_ip_src") |> BatOption.default "true") ^"
                then (
                    let y = "^ sort_by ^" in
                    "^ (start |> BatOption.map (fun _ ->
                    "let start, y =
                        if start >= min_start then start, y
                        else min_start, Int64.(to_int (div (mul (of_int y) (sub stop min_start)) (sub stop start))) in
                    ") |> BatOption.default "") ^"
                    "^ (stop |> BatOption.map (fun _ ->
                    "let stop, y =
                        if stop <= max_stop then stop, y
                        else max_stop, Int64.(to_int (div (mul (of_int y) (sub max_stop start)) (sub stop start))) in
                    ") |> BatOption.default "") ^"
                    let k = "^ String.concat ", " key_fields ^" in
                    f (k, y) p
                ) else p)
            i m
        in
    let interm = Plot.FindSignificant.pass1 fold1 "^ Integer.to_imm (max_graphs-1) ^" in
    let fold2 f i m =
            "^ modname ^".fold_all
            "^ (start |> BatOption.map (fun _ -> "~start:min_start") |> BatOption.default "") ^"
            "^ (stop  |> BatOption.map (fun _ -> "~stop:max_stop") |> BatOption.default "") ^"
            "^ (ip_src|> BatOption.map (fun _ -> "~ip_src:req_ip_src") |> BatOption.default "") ^"
            "^ Text.to_imm dbdir ^" "^ Text.to_imm name ^"
            (fun ("^ (fields |> List.map (fun (n,_f) -> n) |> String.concat ", ") ^") p ->
                if "^ (usr_filter |> BatOption.map User_filter.ocaml_of_expr |> BatOption.default "true") ^" &&
                   "^ (start |> BatOption.map (fun _ -> "Datatype.Timestamp.compare stop min_start >= 0") |> BatOption.default "true") ^" &&
                   "^ (stop  |> BatOption.map (fun _ -> "Datatype.Timestamp.compare max_stop start > 0") |> BatOption.default "true") ^" &&
                   "^ (ip_src|> BatOption.map (fun _ -> "Datatype.in_cidr ip_src req_ip_src") |> BatOption.default "true") ^"
                then (
                    let y = "^ sort_by ^" in
                    "^ (start |> BatOption.map (fun _ ->
                    "let start, y =
                        if start >= min_start then start, y
                        else min_start, Int64.(to_int (div (mul (of_int y) (sub stop min_start)) (sub stop start))) in
                    ") |> BatOption.default "") ^"
                    "^ (stop |> BatOption.map (fun _ ->
                    "let stop, y =
                        if stop <= max_stop then stop, y
                        else max_stop, Int64.(to_int (div (mul (of_int y) (sub max_stop start)) (sub stop start))) in
                    ") |> BatOption.default "") ^"
                    let k = "^ String.concat ", " key_fields ^" in
                    let tv = "^ (if aggr_fields = [] then "()" else String.concat ", " (List.map (fun (fn,an) ->
                        let f = List.assoc fn fields in
                        let a = List.assoc an f.aggrs in
                        a.singleton ^" "^ fn) aggr_fields)) ^" in
                    f (k, y, tv) p
                ) else p)
            i m
        in

    (* We now build a distribution function for the whole tuple, out of the individual field aggr functions *)
    let tv_zero = "^ (if aggr_fields = [] then "()" else (aggr_fields |> List.map (fun (fn,an) ->
        let f = List.assoc fn fields in
        let a = List.assoc an f.aggrs in
        a.zero) |> String.concat ", ")) ^"
    and tv_aggr
        ("^ (aggr_fields |> List.map (fun (fn,an) -> fn ^"_"^ an ^"_1") |> String.concat ", ") ^")
        ("^ (aggr_fields |> List.map (fun (fn,an) -> fn ^"_"^ an ^"_2") |> String.concat ", ") ^") =
        "^ (if aggr_fields = [] then "()" else (aggr_fields |> List.map (fun (fn,an) ->
            let f = List.assoc fn fields in
            let a = List.assoc an f.aggrs in
            a.func^" "^fn^"_"^an^"_1 "^fn^"_"^an^"_2") |> String.concat ", ")) ^" in
    let result, rest, rest_tv = Plot.FindSignificant.pass2 interm fold2 tv_aggr tv_zero "^ Integer.to_imm (max_graphs-1) ^" in
    (* We want to return a list of (Some array of string) * array of string * volume *)
    Hashtbl.fold
        (fun ("^ (String.concat ", " key_fields) ^")
             (sort_v, ("^ (String.concat ", " (List.map (fun (fn,an) -> fn^"_"^an) aggr_fields)) ^"))
             lst ->
        let k_a = [| "^ (key_fields |> List.map (fun f -> (List.assoc f fields).datatype ^ ".to_string "^f) |> String.concat "; ") ^" |]
        and v_a = [| "^ (aggr_fields |> List.map (fun (fn,an) ->
            let f = List.assoc fn fields in
            let a = List.assoc an f.aggrs in
            f.datatype ^ ".to_string ("^a.fin ^" "^fn^"_"^an^")") |> String.concat "; ") ^" |] in
        (Some k_a, v_a, sort_v, sort_v) :: lst)
        result
        "^ (if aggr_fields = [] then "[]" else "
        (let "^ (aggr_fields |> List.map (fun (fn,an) -> "rest_"^fn^"_"^an) |> String.concat ", ") ^" = rest_tv in
            [ None, [| "^ (aggr_fields |> List.map (fun (fn,an) ->
                let f = List.assoc fn fields in
                let a = List.assoc an f.aggrs in
                f.datatype ^ ".to_string ("^a.fin ^" rest_"^fn^"_"^an^")") |> String.concat "; ") ^" |], rest, rest ])
        ")^" |>
        List.sort (fun (_,_,v1mi,_) (_,_,v2mi,_) -> compare v2mi v1mi)

let () =
    dyn_top := top" |>
    load_string


let load_top_single_pass modname fields ?start ?stop ?ip_src ?usr_filter ~max_graphs sort_by key_fields aggr_fields dbdir name =
"open "^ modname ^"
open Batteries

let top () =
    "^ (start |> BatOption.map (fun s -> "let min_start = "^ (s |> Timestamp.to_imm) ^" in") |> BatOption.default "") ^"
    "^ (stop  |> BatOption.map (fun s -> "let max_stop  = "^ (s |> Timestamp.to_imm) ^" in") |> BatOption.default "") ^"
    "^ (ip_src|> BatOption.map (fun i -> "let req_ip_src = "^ (i |> Cidr.to_imm) ^" in") |> BatOption.default "") ^"
    let fold f i m =
        "^ modname ^".fold_all
            "^ (start |> BatOption.map (fun _ -> "~start:min_start") |> BatOption.default "") ^"
            "^ (stop  |> BatOption.map (fun _ -> "~stop:max_stop") |> BatOption.default "") ^"
            "^ (ip_src|> BatOption.map (fun _ -> "~ip_src:req_ip_src") |> BatOption.default "") ^"
            "^ Text.to_imm dbdir ^" "^ Text.to_imm name ^"
            (fun ("^ (fields |> List.map (fun (n,_f) -> n) |> String.concat ", ") ^") p ->
                if "^ (usr_filter |> BatOption.map User_filter.ocaml_of_expr |> BatOption.default "true") ^" &&
                   "^ (start |> BatOption.map (fun _ -> "Datatype.Timestamp.compare stop min_start >= 0") |> BatOption.default "true") ^" &&
                   "^ (stop  |> BatOption.map (fun _ -> "Datatype.Timestamp.compare max_stop start > 0") |> BatOption.default "true") ^" &&
                   "^ (ip_src|> BatOption.map (fun _ -> "Datatype.in_cidr ip_src req_ip_src") |> BatOption.default "true") ^"
                then (
                    let y = "^ sort_by ^" in
                    "^ (start |> BatOption.map (fun _ ->
                    "let start, y =
                        if start >= min_start then start, y
                        else min_start, Int64.(to_int (div (mul (of_int y) (sub stop min_start)) (sub stop start))) in
                    ") |> BatOption.default "") ^"
                    "^ (stop |> BatOption.map (fun _ ->
                    "let stop, y =
                        if stop <= max_stop then stop, y
                        else max_stop, Int64.(to_int (div (mul (of_int y) (sub max_stop start)) (sub stop start))) in
                    ") |> BatOption.default "") ^"
                    let k = "^ String.concat ", " key_fields ^" in
                    let tv = "^ (if aggr_fields = [] then "()" else String.concat ", " (List.map (fun (fn,an) ->
                        let f = List.assoc fn fields in
                        let a = List.assoc an f.aggrs in
                        a.singleton ^" "^ fn) aggr_fields)) ^" in
                    f (k, y, tv) p (* call update and ignore f for better inlining? *)
                ) else p)
            i m
        in

    (* We now build a distribution function for the whole tuple, out of the individual field aggr functions *)
    let tv_zero = "^ (if aggr_fields = [] then "()" else (aggr_fields |> List.map (fun (fn,an) ->
        let f = List.assoc fn fields in
        let a = List.assoc an f.aggrs in
        a.zero) |> String.concat ", ")) ^"
    and tv_aggr
        ("^ (aggr_fields |> List.map (fun (fn,an) -> fn ^"_"^ an ^"_1") |> String.concat ", ") ^")
        ("^ (aggr_fields |> List.map (fun (fn,an) -> fn ^"_"^ an ^"_2") |> String.concat ", ") ^") =
        "^ (if aggr_fields = [] then "()" else (aggr_fields |> List.map (fun (fn,an) ->
            let f = List.assoc fn fields in
            let a = List.assoc an f.aggrs in
            a.func^" "^fn^"_"^an^"_1 "^fn^"_"^an^"_2") |> String.concat ", ")) ^" in
    let h, rv, rtv = Plot.heavy_hitters "^ Integer.to_imm max_graphs ^" fold tv_aggr tv_zero in
    (* We want to return a (string array option * string array * int) list *)
    Hashtbl.fold
        (fun ("^ (String.concat ", " key_fields) ^") (* h key *)
             (v, ("^ (String.concat ", " (List.map (fun (fn,an) -> fn^"_"^an) aggr_fields)) ^"), v0, n0) (* h value *)
             lst ->
            let k_a = [| "^ (key_fields |> List.map (fun f -> (List.assoc f fields).datatype ^".to_string "^f) |> String.concat "; ") ^" |]
            and v_a = [| "^ (aggr_fields |> List.map (fun (fn,an) ->
                let f = List.assoc fn fields in
                let a = List.assoc an f.aggrs in
                f.datatype ^".to_string ("^a.fin ^" "^fn^"_"^an^")") |> String.concat "; ") ^" |] in
            (Some k_a, v_a, v, v+n0) :: lst)
        h"^ (if aggr_fields = [] then "[]" else "
        (let "^ (aggr_fields |> List.map (fun (fn,an) -> "r_"^fn^"_"^an) |> String.concat ", ") ^" = rtv in
            [ None, [| "^ (aggr_fields |> List.map (fun (fn,an) ->
                let f = List.assoc fn fields in
                let a = List.assoc an f.aggrs in
                f.datatype ^".to_string ("^a.fin ^" r_"^fn^"_"^an^")") |> String.concat "; ") ^" |], rv, rv ])
        ")^" |>
        List.sort (fun (_,_,v1mi,_) (_,_,v2mi,_) -> compare v2mi v1mi)

let () =
    dyn_top := top" |>
    load_string

