(** Get parameters from the configuration files.
 * Values from a given hashtable may override these (useful for cookies). *)

val overwrite : (string, string) BatHashtbl.t -> unit
(** Declare a hash of parameter names to values as a priority source of values *)

val set_dir : string -> unit
(** Set default directory to read conf files from. Default is cwd. *)

val get_string : string -> string -> string
(** [get_string "foo/bar" "baz"] returns the value (as a string) of parameter "foo/bar",
 * or "baz" if it's unset. *)

val get_int : string -> int -> int
(** [get_int "foo/bar" 42] returns the value (as an int) of parameter
 * "foo/bar", or 42 if it's unset. *)

