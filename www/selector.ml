open Batteries
open Input.Ops
open Metric

module type CONF =
sig
    val fields : (string * selectable_field) list
end

module MakeKey (Conf : CONF) =
struct
    type t = string list

    let to_edit name args =
        let open Html in
        let selected = Hashtbl.find_all args name in
        [ tag "select"
              ~attrs:[ "multiple", "multiple" ;
                       "name", name ;
                       "size", "6" ]
            (List.enum Conf.fields //@
            (fun (name, descr) ->
                if descr.keyable then Some name else None) /@
            (fun name ->
                tag "option"
                    ~attrs:(("name", name)::
                            (if List.mem name selected then ["selected", "selected"]
                                                       else []))
                    [ cdata name ]) |>
            List.of_enum) ]

    let from_args name args =
        let selection = Hashtbl.find_all args name in
        if selection = [] then input_error "You must choose a Key"
        else selection
end

module MakeAggr (Conf : CONF) =
struct
    type t = string list (* field/aggr_function *)

    let to_edit name args =
        let open Html in
        let selection = Hashtbl.find_all args name in
        [ tag "select"
              ~attrs:[ "multiple", "multiple" ;
                       "name", name ;
                       "size", "8" ]
            (List.enum Conf.fields //@
            (fun (name, descr) ->
                if descr.aggrs <> [] then Some (name, descr.aggrs) else None) /@
            (fun (name, aggrs) ->
                tag "optgroup"
                    ~attrs:["label",name]
                    (List.map (fun (aggr_name, _) ->
                        let opt_name = name ^"."^ aggr_name in
                        let selected = List.mem opt_name selection in
                        tag "option"
                            ~attrs:(("value", opt_name)::
                                    (if selected then ["selected", "selected"] else []))
                            [ cdata aggr_name ])
                        aggrs)) |>
            List.of_enum) ]

    let from_args name args =
        Hashtbl.find_all args name
end

module MakeSort (Conf : CONF) =
struct
    type t = string

    let to_edit name args =
        let open Html in
        let selected = Hashtbl.find_option args name in
        [ tag "select"
              ~attrs:[ "name", name ]
            (List.enum Conf.fields //@
            (fun (name, descr) ->
                if descr.sortable <> "" then Some name else None) /@
            (fun name ->
                tag "option"
                    ~attrs:(("name", name)::
                            (if selected = Some name then ["selected", "selected"] else []))
                    [ cdata name ]) |>
            List.of_enum) ]

    let from_args name args =
        match Hashtbl.find_option args name with
        | Some f -> f
        | None -> input_error "You must sort by some field"
end


