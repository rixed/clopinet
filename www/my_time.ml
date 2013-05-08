(* Absolute or relative (to now) timestamp *)
open Batteries
open Input.Ops
module Timestamp = Datatype.Timestamp
module Interval = Datatype.Interval

module Base =
struct
    type t = Abs of Timestamp.t | Rel of Interval.t

    let to_string = function
        | Abs ts -> Timestamp.to_string ts
        | Rel iv -> Interval.to_string iv

    exception CantParse of string
    let of_string s =
        try Abs (Timestamp.of_string s)
        with Peg.Parse_error (_,l1 as e1) ->
            try Rel (Interval.of_string s)
            with Peg.Parse_error (_,l2 as e2) ->
                raise @@ CantParse (Peg.string_of_error ~input:s (if l1<l2 then e1 else e2))

    let to_timeval = function
        | Abs ts -> ts
        | Rel iv -> Timestamp.add_interval (Timestamp.now ()) iv

    let random_sample () =
        Interval.samples @ Timestamp.samples |>
        List.enum |>
        Random.choice

    let shifted_getter getter =
        (* replace normal getter by another one that replaces the asked values by
         * a time offset depending on the presence of the "time-travel" value *)
        let tt = getter "time-travel" in
        let dt = if tt = [] then 0. else (
            match getter "filter/start", getter "filter/stop" with
            | [sta],[sto] ->
                let s2tv x = of_string x |> to_timeval in
                (try Timestamp.sub_to_interval (s2tv sto) (s2tv sta) |>
                    Interval.to_secs |>
                    if tt = ["prev"] then Float.neg else identity
                with CantParse e -> Log.info "cannot parse: %s" e ; 0.)
            | _ -> 0.
        ) in
        fun str ->
            let vs = getter str in
            if dt = 0. then vs else
            List.map (fun s ->
                let s' =
                    try let v = of_string s in
                        let t = to_timeval v in
                        Timestamp.add_secs t dt |>
                        Timestamp.to_string
                    with CantParse e -> Log.info "can't parse '%s' -> %s" s e ; s in
               Log.info "shift from %s to %s (dt=%f)" s s' dt ;
               s') vs

    let to_edit name getter =
        [ Html.input
            [ "name", name ;
              "value", input_text_of name (shifted_getter getter) ;
              "placeholder", random_sample () ] ]

    let from name getter =
        match shifted_getter getter name with
            | [] | [""] -> missing_field ()
            | s::_ ->
                try of_string s
                with CantParse e ->
                    input_error e

end

module TimeTravel (Conf : sig val value : string val label : string end) =
struct
    include Base

    let to_edit name getter =
        to_edit name getter @
        [ Html.tag "button"
            ~cls:"time-travel"
            ~attrs:["type","submit" ; "value",Conf.value ; "name","time-travel"]
            [Html.cdata Conf.label] ]
end

module Start = TimeTravel (struct let value = "prev" let label = "<<" end)
module Stop  = TimeTravel (struct let value = "next" let label = ">>" end)
