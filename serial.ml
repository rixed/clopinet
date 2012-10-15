(* Low level functions for fast serialization *)

type obuf
type ibuf
external make_obuf : string -> obuf = "obuf_open"
external make_ibuf : string -> ibuf = "ibuf_open"
external close_obuf : obuf -> unit = "obuf_close"
external close_ibuf : ibuf -> unit = "ibuf_close"
external deser1 : ibuf -> bool = "read1"
external ser1 : obuf -> bool -> unit = "write1"
external deser8 : ibuf -> int = "read8"
external ser8 : obuf -> int -> unit = "write8"
external deser16 : ibuf -> int = "read16"
external ser16 : obuf -> int -> unit = "write16"
external deser32 : ibuf -> int = "read32"
external ser32 : obuf -> int -> unit = "write32"
external deser64 : ibuf -> int = "read64"
external ser64 : obuf -> int -> unit = "write64"
external deser_var : ibuf -> int = "read_varint"
external ser_var : obuf -> int -> unit = "write_varint"
