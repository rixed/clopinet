(* Low level functions to fetch all environment *)
open Batteries

external get_nb_envvars : unit -> int = "wrap_nb_envvars"
let nb_envvars = get_nb_envvars ()

external get_env_n : int -> string = "wrap_getenv_n"
let get n =
    if n < nb_envvars then (
        let s = get_env_n n in
        Some (String.split s "=")
    ) else None

