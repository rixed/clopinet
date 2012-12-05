
(** {2} Reading *)

val read_meta_fname : string -> (Serial.ibuf -> 'a) -> 'a option

val max_file_size : int

val max_hash_size : int

val iter_fname : string -> (Serial.ibuf -> 'a) -> ('a -> unit) -> unit

val iter_file : string -> int -> int -> (Serial.ibuf -> 'a) -> ('a -> unit) -> unit

(* f is called with snum and the optional meta data *)
val iter_snums : string -> int -> (Serial.ibuf -> 'a) -> (int -> 'a option -> unit) -> unit

val iter_hnums : string -> (int -> unit) -> unit

val iter : string -> (Serial.ibuf -> 'a) -> ('a -> unit) -> unit

(* [fold_file dir hnum snum reader op start] perform a fold on the given file *)
val fold_file : string -> int -> int -> (Serial.ibuf -> 'a) -> ('a -> 'b -> 'b) -> 'b -> 'b

(** [fold_snums dir hnum reader f start merge] will call f for each snum file
 * with the starting value [start] and will combine all partial results using [merge]. *)
val fold_snums : string -> int -> (Serial.ibuf -> 'a) -> (int -> 'a option -> 'b -> 'b) -> 'b -> ('b -> 'b -> 'b) -> 'b

(** [fold_hnums dir f start merge] runs [f hnum (start ())] for each hnum directory in [dir],
 * starting from [start] initial value for each one and recombining the partial
 * results with [merge] at the end.
 * We do not pass the transient result from one hnum to the next since we want to be
 * able to split the work in hnum between various cores, each working on a subset of
 * the content starting from the {i initial position}.
 * But we'd like to allow [f] to modify a mutable starting value, so you must provide [start]
 * as a function returning the initial value so that we can provide [f] with as
 * many copies of it as needed.
 * If [start] is immutable you are of course free to return a constant here. *)
val fold_hnums : string -> (int -> 'b -> 'b) -> (unit -> 'b) -> ('b -> 'b -> 'b) -> 'b

(** Same as above but looks only in these hnums *)
val fold_some_hnums : int list -> (int -> 'b -> 'b) -> (unit -> 'b) -> ('b -> 'b -> 'b) -> 'b

val fold : string -> (Serial.ibuf -> 'a) -> ('a -> 'b -> 'b) -> (unit -> 'b) -> ('b -> 'b -> 'b) -> 'b

(** {2} Writing *)

type ('a, 'b) t

(* TODO: introduce a reader and a writer to simplify this: *)

val create : string -> ('a -> int) -> (Serial.obuf -> 'a -> unit) ->
             ('a, 'b) Aggregator.t -> (Serial.ibuf -> 'b) -> (Serial.obuf -> 'b -> unit) ->
             ('a, 'b) t

val append : ('a, 'b) t -> 'a -> unit

val close : ('a, 'b) t -> unit

