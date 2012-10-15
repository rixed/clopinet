
(** {2} Reading *)

val max_file_size : int

val max_hash_size : int

val ncores : int ref

val iter_file : string -> int -> int -> (Serial.ibuf -> 'a) -> ('a -> unit) -> unit

(* f is called with snum and the optional meta data *)
val iter_snums : string -> int -> (Serial.ibuf -> 'a) -> (int -> 'a option -> unit) -> unit

val iter_hnums : string -> (int -> unit) -> unit

val iter : string -> (Serial.ibuf -> 'a) -> ('a -> unit) -> unit

(* [fold_file dir hnum snum reader op start] perform a fold on the given file *)
val fold_file : string -> int -> int -> (Serial.ibuf -> 'a) -> ('a -> 'b -> 'b) -> 'b -> 'b

val fold_snums : string -> int -> (Serial.ibuf -> 'a) -> (int -> 'a option -> 'b -> 'b) -> 'b -> ('b -> 'b -> 'b) -> 'b

val fold_hnums : string -> (int -> 'b -> 'b) -> 'b -> ('b -> 'b -> 'b) -> 'b

val fold : string -> (Serial.ibuf -> 'a) -> ('a -> 'b -> 'b) -> 'b -> ('b -> 'b -> 'b) -> 'b

(** {2} Writing *)

type ('a, 'b) t

(* TODO: introduce a reader and a writer to simplify this: *)

val create : string -> ('a -> int) -> (Serial.obuf -> 'a -> unit) ->
             ('a, 'b) Aggregator.t -> (Serial.ibuf -> 'b) -> (Serial.obuf -> 'b -> unit) ->
             ('a, 'b) t

val append : ('a, 'b) t -> 'a -> unit

val close : ('a, 'b) t -> unit

