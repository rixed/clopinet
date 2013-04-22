(** Get parameters from the configuration files.
 * Values from a given hashtable may override these (useful for cookies). *)

val set_overwrite_function : (string -> string option) -> unit
(** Installs a function as the primary source of values. *)

val overwrite_many : (string, string) BatHashtbl.t -> unit
(** Add a hash of parameter names to values as a 2nd source of values *)

val overwrite_single : string -> unit
(** Add a single conf line in overwrite values *)

val get_option : string -> string option
(** [get_option "foo/bar"] returns the value for parameter ["foo/bar"],
 * or [None] if it's unset. *)

val get_string : string -> string -> string
(** [get_string "foo/bar" "baz"] returns the value (as a string) of parameter ["foo/bar"],
 * or ["baz"] if it's unset. *)

val enum : unit -> (string * string) BatEnum.t
(** [enum ()] returns an enumeration of all config parameters
 * (but the ones from the overwrite function for obvious reasons) *)

