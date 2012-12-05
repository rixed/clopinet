(** Get parameters from the configuration files.
 * Values from a given hashtable may override these (useful for cookies). *)

val overwrite : (string, string) BatHashtbl.t -> unit
(** Declare a hash of parameter names to values as a priority source of values *)

val overwrite_single : string -> unit
(** Add a single conf line in overwrite values *)

val set_base : string -> unit
(** Set default directory to read conf files from (or default file
 * if you want a unique conf file). Default is ["./conf"]. *)

val get_string : string -> string -> string
(** [get_string "foo/bar" "baz"] returns the value (as a string) of parameter ["foo/bar"],
 * or ["baz"] if it's unset. *)

val get_int : string -> int -> int
(** [get_int "foo/bar" 42] returns the value (as an int) of parameter
 * ["foo/bar"], or [42] if it's unset. *)

val get_float : string -> float -> float
(** [get_float "foo/bar" 42.5] returns the value (as a float) of parameter
 * ["foo/bar"], or [42.5] if it's unset. *)

val get_bool : string -> bool -> bool
(** [get_bool "foo/bar" false] returns the value (as a boolean) of parameter
 * ["foo/bar"], or [false] if it's unset. *)

