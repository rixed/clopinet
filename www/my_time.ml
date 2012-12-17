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

let to_edit name args =
    [ Html.input
        [ "name", name ;
          "value", input_text_of name args ] ]

let from_args name args =
    match Hashtbl.find_option args name with
        | None | Some "" -> missing_field ()
        | Some s ->
            try Abs (Timestamp.of_string s)
            with _ -> try Rel (Interval.of_string s)
            with _ -> input_error "Can not parse as date nor time"
