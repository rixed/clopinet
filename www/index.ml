(* CGI script for visualizing clopinet datas *)
open Batteries
open Input.Ops
module View = My_view
module Ctrl = My_ctrl

let get_page = function
    | ["info"] ->
        Ctrl.Info.run
    | [""] | ["main"] ->
        Ctrl.main
    | ["Reports"; name] ->
        Ctrl.Report.build name
    | ["Config"; "Preferences"] ->
        Ctrl.Config.preferences
    | ["Config"; "Preferences"; "save"] ->
        Ctrl.Config.save_preferences
    | [cat; "help"] ->
        Help.page cat
    | [cat; title] ->
        let descr = Ctrl.find_chart cat title in
        Ctrl.ChartDescr.filtered_chart descr
    | [cat; title; "show"] ->
        let descr = Ctrl.find_chart cat title in
        Ctrl.ChartDescr.filtered_chart_show descr
    | _ ->
        Ctrl.Invalid.run

let cgi_entry_point () =
    (* Get preferences from the session *)
    let get_from_cookie name =
        try let s = List.assoc name !Dispatch.current_cookies in
            Some (decode_cookie s)
        with Not_found -> None in
    Prefs.set_overwrite_function get_from_cookie ;
(*    Log.info "env: %a" (Enum.print (fun fmt (n,v) -> Printf.fprintf fmt "%s=%s; " n v)) (Prefs.enum ()) ;*)
    Dispatch.run (fun name getter -> get_page name getter |> View.make_app_page Ctrl.menu_entries)

let cli_entry_point () =
    let action = ref "main"
    and h = Hashtbl.create 11 in
    Arg.(parse
        [ "-action", Set_string action, "what page to render" ]
        (fun s ->
            try let n, v = String.split ~by:"=" s in
                Hashtbl.add h n v
            with Not_found -> raise (Bad s))
        ("Render an HTML page: "^Sys.argv.(0)^" param1=value1 param2=value2...")) ;
    let getter = Hashtbl.find_all h in
    get_page (String.nsplit ~by:"/" !action) getter |>
    View.make_app_page_for_email ("?action="^ !action) |>
    List.iter (Html.print stdout)

let _ =
    let is_envvar_set s =
        try ignore (Sys.getenv s) ; true
        with Not_found -> false in
    if is_envvar_set "REQUEST_METHOD" then
        cgi_entry_point ()
    else
        cli_entry_point ()

