(** Returns the dirname of a particular serie.
  Files are identified by the path of the table they belongs to,
  their index and then their sequence number. *)
val dir : string -> int -> string

(** Returns the complete path of an individual file *)
val path : string -> int -> int -> string

(** Returns a out_channel for the given file, reusing the suggested one if
  possible. The last bool controls the exclusive flag. Note that the table paths
  are compared as pointer not strings! *)
val get : ?prev:int -> string -> int -> int -> bool -> (int * out_channel)

(** Close + release the file descriptor for the given file. *)
val close : ?prev:int -> string -> int -> int -> unit

(** Unix permissions to create new files with *)
val perm : int
