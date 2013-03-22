(* user inputs of type expression *)
open Batteries
open Datatype
open User_filter
open Input.Ops

module Make (Conf : sig val fields : (string * selectable_field) list end) =
struct
    type t = expr

    let to_edit name getter =
        [ Html.input
            [ "name", name ;
              "value", input_text_of name getter ] ]

    let from name getter =
        match getter name with
            | [] | [""] -> missing_field ()
            | s::_ ->
                try expression TBool Conf.fields s
                with Peg.Parse_error err -> input_error ("Parse Error: "^ Peg.string_of_error err)
                   | Type_error x -> input_error (string_of_type_error x)
end
