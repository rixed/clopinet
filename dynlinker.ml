open Batteries

let loadstring str =
    let fname =
        File.with_temporary_out ~prefix:"mlrrd_" ~suffix:".ml" ~mode:[`create;(*`delete_on_exit;*)`text]
            (fun oc fname ->
                IO.nwrite oc str ;
                fname) in
    let cmxs = Dynlink.adapt_filename fname in
    let cmd = Printf.sprintf "PATH=/bin:/usr/bin:/home/rixed/ocalme/bin OCAMLPATH=/home/rixed/share/src ocamlfind ocamlopt -o %s -package mlrrd -S -rectypes -inline 9 -shared %s" cmxs fname in
    match Unix.system cmd with
    | Unix.WEXITED 0 ->
        Dynlink.loadfile cmxs
        (* TODO: rm fname and cmxs *)
    | _ ->
        Printf.fprintf stderr "Cannot compile %s using %s\n" fname cmd

let check opt_v to_imm fmt =
    Option.map (fun v -> Printf.sprintf fmt (to_imm v)) opt_v

let load_filter module_name usr_fields ?usr_filter checks =
    let param_names = List.map fst usr_fields |> String.join "," in
    (* We have two filters combined: one from preset filter terms, and the free usr_filter, ANDed *)
    let usr_filter = match usr_filter with
        | None -> "true"
        | Some str ->
            let open User_filter in
            let expr = expression usr_fields str in
            check TBool expr ;
            ocaml_of_expr expr in
    let checks =
        List.fold_left (fun p ->
            function None -> p | Some x -> x::p) [] checks |>
        List.rev in
    let preset_filter =
        if checks = [] then "true" else
        String.concat " && " checks in
    let os = IO.output_string () in
    Printf.fprintf os "\
let () =\n\
    %s.set_filter (fun (%s) ->\n\
        (* user free filters *)\n\
        %s &&\n\
        (* preset filters *)\n\
        %s)\n"
            module_name param_names
            usr_filter preset_filter ;
    let str = IO.close_out os in
    loadstring str

