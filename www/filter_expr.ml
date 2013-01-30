(* user inputs of type expression *)
open Batteries
open User_filter
open Input.Ops

module Make (Conf : sig val filter_fields : (string * User_filter.expr_type) list end) =
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
                try expression TBool Conf.filter_fields s
                with Peg.Parse_error -> input_error "Parse Error!"
                   | Type_error x -> input_error (string_of_type_error x)
end
