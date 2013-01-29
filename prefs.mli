(** Get parameters from the configuration files.
 * Values from a given hashtable may override these (useful for cookies). *)

val set_overwrite_function : (string -> string option) -> unit
(** Installs a function as the primary source of values. *)

val overwrite_many : (string, string) BatHashtbl.t -> unit
(** Add a hash of parameter names to values as a 2nd source of values *)

val overwrite_single : string -> unit
(** Add a single conf line in overwrite values *)

val set_base : string -> unit
(** Set default directory to read conf files from (or default file
 * if you want a unique conf file). Default is ["./conf"].
 * These files will be used as the last resort source of values. *)

val get_option : string -> string option
(** [get_option "foo/bar"] returns the value for parameter ["foo/bar"],
 * or [None] if it's unset. *)

val get_string : string -> string -> string
(** [get_string "foo/bar" "baz"] returns the value (as a string) of parameter ["foo/bar"],
 * or ["baz"] if it's unset. *)

val get_int : string -> int -> int
(** [get_int "foo/bar" 42] returns the value (as an int) of parameter
 * ["foo/bar"], or [42] if it's unset. *)

val get_int_option : string -> int option

val get_float : string -> float -> float
(** [get_float "foo/bar" 42.5] returns the value (as a float) of parameter
 * ["foo/bar"], or [42.5] if it's unset. *)

val get_float_option : string -> float option

val get_bool : string -> bool -> bool
(** [get_bool "foo/bar" false] returns the value (as a boolean) of parameter
 * ["foo/bar"], or [false] if it's unset. *)

val get_bool_option : string -> bool option

val map_inplace : (string -> string -> string) -> unit
(** [map_inplace f] iterates over all defined values in the preference file and replace
 * each previous values [v] for key [k] by [f k v] *)

