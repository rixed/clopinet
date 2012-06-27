(** Returns the dirname of a particular serie.
  Files are identified by the path of the table they belongs to,
  their index and then their sequence number. *)
val dir : string -> int -> string

(** Returns the complete path of an individual file *)
val path : string -> int -> int -> string

(** Returns an Output.t for the given file, reusing the suggested one if
  possible. Note that the table paths are compared as pointer not strings!
  Output.t is locked on write (so you have exclusive write perm on it) *)
val get : ?prev:int -> string -> int -> int -> (int * Output.t)

(** Close + release the file descriptor for the given file. *)
val close : ?prev:int -> string -> int -> int -> unit

(** Unix permissions to create new files with *)
val perm : int
