(* Absolute or relative (to now) timestamp *)
open Batteries
open Input.Ops
module Timestamp = Datatype.Timestamp
module Interval = Datatype.Interval

type t = Abs of Timestamp.t | Rel of Interval.t

let to_string = function
    | Abs ts -> Timestamp.to_string ts
    | Rel iv -> Interval.to_string iv

let to_timeval = function
    | Abs ts -> ts
    | Rel iv -> Timestamp.add_interval (Timestamp.now ()) iv

module Optional = struct
    type time = t
    type t = time option

    let name = "optional time"

    let to_html uv =
        [ Html.cdata (html_of_user_value (Option.map_default to_string "<i>unset</i>") uv) ]

    let edit name uv =
        [ Html.input [ "name", name ;
                  "value", input_of_user_value (Option.map_default to_string "") uv ] ] @
        err_msg_of uv

    let from_args name args =
        match Hashtbl.find_option args name with
            | None | Some "" -> Value None
            | Some s ->
                try Value (Some (Abs (Timestamp.of_string s)))
                with _ -> try Value (Some (Rel (Interval.of_string s)))
                with _ -> Error ("Can not parse as date nor time", s)
end

module Mandatory = struct
    module O = Optional
    type time = t
    type t = time
    let name = "time"
    let make_opt = function
        | Error _ as x -> x
        | Value n -> Value (Some n)
    let to_html v = O.to_html (make_opt v)
    let edit name v = O.edit name (make_opt v)
    let from_args name args =
        match O.from_args name args with
        | Error _ as e -> e
        | Value None -> missing_field
        | Value (Some n) -> Value n
end
