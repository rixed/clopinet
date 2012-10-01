(* CGI script for visualizing mlrrd datas *)
open Batteries
open Html
open Datatype
open Input.Ops

let dbdir = "../test.db"

module View =
struct
    include View

    (* notification system *)

    let msgs = ref []

    let add_msg_with_class c txt =
        msgs := (p ~attrs:["class",c] [cdata txt]) :: !msgs
    let add_msg = add_msg_with_class "ok"
    let add_err = add_msg_with_class "nok"

    let msgs () = div ~attrs:["id","notifs"] !msgs

    (* rendering of pages *)

    let menu () =
        let html_of_entry e = tag "li" [ tag "a" ~attrs:["href","?action=main/"^e] [cdata e] ]
        and menu_entries = ["traffic"; "DNS"; "Web"; "logout"] in
        tag ~attrs:["class","menu"] "ul" (List.map html_of_entry menu_entries)

    (* add the menu *)
    let make_app_page content =
        let body = menu () :: msgs () :: [ tag "div" ~attrs:["id","page"] content ]
        and head = [ title "MlRRD" ; link_css "static/css/style.css" ] in
        html head body

    let table_of_datasets datasets =
        let all_rows =
            Hashtbl.fold (fun label pts doc ->
                let rows =
                    List.sort (fun (x1, _) (x2, _) -> compare x1 x2) pts |>
                    List.map (fun (x, y) ->
                        tr [ td [ cdata (string_of_float x) ] ;
                             td [ cdata (string_of_float y) ] ]) in
                (tr [ th ~attrs:["colspan", "2"] [ cdata label ] ] ::
                rows) @ doc)
                datasets [] in
        [ table (tr [ th [ cdata "time" ] ;
                      th [ cdata "qtt" ] ] ::
                 all_rows) ]
end

module InputOfDatatype (D : DATATYPE) :
    TYPE with type t = D.t =
struct
    module String = Input.String (struct let min = 1 let max = max_int end)
    type t = D.t
    let name = D.name
    let to_html v = [ cdata (html_of_user_value D.to_string v) ]
    let edit name v =
        [ input [ "name", name ;
                  "value", input_of_user_value D.to_string v ] ] @
        err_msg_of v
    let from_args name args =
        match String.from_args name args with
        | Error _ as x -> x
        | Value s  ->
            (try Value (D.of_string s)
            with End_of_file -> Error ("Not enough data", s)
               | Overflow    -> Error ("Integer overflow", s)
               | exn         -> Error (Printexc.to_string exn, s))
end

module OptInputOfDatatype (D : DATATYPE) :
    TYPE with type t = D.t option =
struct
    module String = Input.String (struct let min = 0 let max = max_int end)
    type t = D.t option
    let name = D.name
    let to_html v = [ cdata (html_of_user_value (function None -> "<i>unset</i>"
                                                        | Some s -> D.to_string s) v) ]
    let edit name v =
        [ input [ "name", name ;
                  "value", input_of_user_value (function None -> ""
                                                       | Some s -> D.to_string s) v ] ] @
        err_msg_of v
    let from_args name args =
        match String.from_args name args with
        | Error _ as x -> x
        | Value s ->
            (try Value (Some (D.of_string s))
            with End_of_file -> if s = "" then Value None else Error ("Not enough data", s)
               | Overflow    -> Error ("Integer overflow", s)
               | exn         -> Error (Printexc.to_string exn, s))
end

module Forms =
struct
    open Input

    module LoginField = struct
        module Type = Input.String (struct let min = 1 let max = max_int end)
        let name = "name"
    end
    module PasswdField = struct
        module Type = Input.Password (struct let min = 3 let max = max_int end)
        let name = "password"
    end
    
    module Login = RecordOf (ConsOf (FieldOf (LoginField))
                            (ConsOf (FieldOf (PasswdField))
                                    (NulType)))

    module StartField = struct
        module Type = OptInputOfDatatype(Timestamp)
        let name = "start"
    end
    module StopField = struct
        module Type = OptInputOfDatatype(Timestamp)
        let name = "stop"
    end
    module VlanField = struct
        module Type = OptInputOfDatatype(Integer16)
        let name = "vlan"
    end
    module MacSrcField = struct
        module Type = OptInputOfDatatype(EthAddr)
        let name = "eth-src"
    end
    module MacDstField = struct
        module Type = OptInputOfDatatype(EthAddr)
        let name = "eth-dest"
    end
    module EthProtoField = struct
        module Type = OptInputOfDatatype(Integer16)
        let name = "eth-proto"
    end
    module IpProtoField = struct
        module Type = OptInputOfDatatype(Integer8)
        let name = "ip-proto"
    end
    module TimeStepField = struct
        module Type = Integer (struct let min = 1 let max = 60*60*24*365 end)
        let name = "time step (s)"
    end
    module TblNames = struct
        let name = "db-tables"
        let options = [| "30secs";"1hour";"10mins" |]
    end
    module TblNameField = struct
        module Type = Enum (TblNames)
        let name = "db-table"
    end
    module GroupByField = struct
        module Type = Enum (struct let name = "key"
                                   let options = [| "macs";"ips";"apps" |] end)
        let name = "group by"
    end
    module Traffic = RecordOf (ConsOf (FieldOf (StartField))
                              (ConsOf (FieldOf (StopField))
                              (ConsOf (FieldOf (VlanField))
                              (ConsOf (FieldOf (MacSrcField))
                              (ConsOf (FieldOf (MacDstField))
                              (ConsOf (FieldOf (EthProtoField))
                              (ConsOf (FieldOf (IpProtoField))
                              (ConsOf (FieldOf (TimeStepField))
                              (ConsOf (FieldOf (TblNameField))
                              (ConsOf (FieldOf (GroupByField))
                                      (NulType)))))))))))

end

module Ctrl =
struct
    include Ctrl

    (* returns the 'user-id' *)
    let auth name passwd =
        if name = "admin" && passwd = "secret" then Some 1 else None

    (* Main app page *)
    let main _args =
        let msg = "yo!" in
        View.make_app_page [cdata msg]

    let login args =
        let login = Forms.Login.from_args "login" args in
        let login_page () =
            View.make_app_page [ h1 "Authentification" ;
                                 form "login" (Forms.Login.edit "login" login) ] in
        let may_add_err = function
            | Error (err, inp) -> View.add_err (inp^":&nbsp;"^err)
            | Value _ -> () in
        match login with
        | Value name, (Value passwd, ()) ->
            if auth name passwd <> None then (
                View.add_msg "Login OK" ;
                (* TODO: this should not be in dispatch! *)
                (* TODO: better auth with a nonce *)
                Dispatch.add_cookie "login" name ;
                Dispatch.add_cookie "password" passwd ;
                main args
            ) else (
                View.add_err "Login failure" ;
                login_page ()
            )
        | x, (y, ()) ->
            may_add_err x ;
            may_add_err y ;
            login_page ()

    let logout _args =
        Dispatch.add_cookie "login" "" ;
        Dispatch.add_cookie "password" "" ;
        View.make_app_page [ p [ cdata "You are now logged out" ] ]

    (* DB search pages *)
    module Traffic =
    struct
        include Traffic
        let dbdir = dbdir^"/traffic"
        type search_type = Macs | Ips | App
        let search args =
            let filters = Forms.Traffic.from_args "filter" args in
            let graph = match filters with
            | Value start, (Value stop, (Value vlan, (Value mac_src, (Value mac_dst, (Value eth_proto, (Value ip_proto, (Value time_step, (Value tblname, (Value group_by, ()))))))))) ->
                let tblname = Forms.TblNames.options.(tblname) in
                let datasets = match group_by with
                    | 0 (* macs *) ->
                        eth_plot_vol_time ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_proto time_step dbdir tblname
                    | 2 (* apps *) ->
                        app_plot_vol_time ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_proto time_step dbdir tblname
                    | _ (* defaults + ips *) ->
                        ip_plot_vol_time ?start ?stop ?vlan ?mac_src ?mac_dst ?eth_proto ?ip_proto time_step dbdir tblname in
                View.table_of_datasets datasets
            | _ -> [ cdata "fill in the form!" ] in
            View.make_app_page
                (h1 "Traffic" ::
                 form "main/traffic" (Forms.Traffic.edit "filter" filters) ::
                 graph)
    end

    let ensure_logged runner args =
        match Hashtbl.find_option args "login",
              Hashtbl.find_option args "password" with
        | None, _ | _, None -> login args
        | Some name, Some passwd ->
            if auth name passwd <> None then (
                runner args
            ) else (
                View.add_err "Bad login" ;
                login args
            )
end

let _ =
    Dispatch.run (function
        | ["info"] -> Ctrl.Info.run
        | [""] | ["login"] ->
            Ctrl.login
        | ["main"] ->
            Ctrl.ensure_logged Ctrl.main
        | ["main"; "logout"] ->
            Ctrl.logout
        | ["main";"traffic"] ->
            Ctrl.ensure_logged Ctrl.Traffic.search
        | _ -> Ctrl.Invalid.run)

