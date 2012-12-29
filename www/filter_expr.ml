(* user inputs of type expression *)
open Batteries
open User_filter
open Input.Ops

module Make (Conf : sig val filter_fields : (string * User_filter.expr_type) list end) =
struct
    type t = expr

    let to_edit name args =
        [ Html.input
            [ "name", name ;
              "value", input_text_of name args ] ]

    let from_args name args =
        match Hashtbl.find_option args name with
            | None | Some "" -> missing_field ()
            | Some s ->
                try expression TBool Conf.filter_fields s
                with Parse_error -> input_error "Parse Error!"
                   | Type_error x -> input_error (string_of_type_error x)
end
