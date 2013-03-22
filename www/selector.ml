open Batteries
open Input.Ops
open Datatype

module type CONF =
sig
    val fields : (string * selectable_field) list
end

module MakeKey (Conf : CONF) =
struct
    type t = string list

    let to_edit name getter =
        let open Html in
        let selected = getter name in
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

    let from name getter =
        let selection = getter name in
        if selection = [] then input_error "You must choose a Key"
        else selection
end

module MakeAggr (Conf : CONF) =
struct
    type t = string list (* field/aggr_function *)

    let to_edit name getter =
        let open Html in
        let selection = getter name in
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

    let from name getter = getter name
end

module MakeSort (Conf : CONF) =
struct
    type t = string

    let to_edit name getter =
        let open Html in
        [ tag "select"
              ~attrs:[ "name", name ]
            (List.enum Conf.fields //@
            (fun (n, descr) ->
                if descr.sortable <> "" then Some n else None) /@
            (fun n ->
                tag "option"
                    ~attrs:(("name", n)::
                            (if List.mem n (getter name) then ["selected", "selected"] else []))
                    [ cdata n ]) |>
            List.of_enum) ]

    let from name getter =
        match getter name with
        | f::_ -> f
        | [] -> input_error "You must sort by some field"
end


