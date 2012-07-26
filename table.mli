
(** {2} Reading *)

val max_file_size : int

val max_hash_size : int

val ncores : int ref

val iter_file : string -> int -> int -> (BinInput.t -> 'a) -> ('a -> unit) -> unit

(* f is called with snum and the optional meta data *)
val iter_snums : ?fork:bool -> string -> int -> (BinInput.t -> 'a) -> (int -> 'a option -> unit) -> unit

val iter_hnums : string -> (int -> unit) -> unit

val iter : ?fork:bool -> string -> (BinInput.t -> 'a) -> ('a -> unit) -> unit

(** {2} Writing *)

type ('a, 'b) t

(* TODO: introduce a reader and a writer to simplify this: *)

val create : string -> ('a -> int) -> (Output.t -> 'a -> unit) ->
             ('a, 'b) Aggregator.t -> (BinInput.t -> 'b) -> (Output.t -> 'b -> unit) ->
             ('a, 'b) t

val append : ('a, 'b) t -> 'a -> unit

val close : ('a, 'b) t -> unit

