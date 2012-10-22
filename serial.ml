(* Low level functions for fast serialization *)

type obuf
type ibuf

external make_obuf : string -> obuf = "obuf_open"
external make_ibuf : string -> ibuf = "ibuf_open"
external close_obuf : obuf -> unit = "obuf_close"
external close_ibuf : ibuf -> unit = "ibuf_close"
external deser1 : ibuf -> bool = "wrap_read1"
external ser1 : obuf -> bool -> unit = "wrap_write1"
external deser8 : ibuf -> int = "wrap_read8"
external ser8 : obuf -> int -> unit = "wrap_write8"
external deser16 : ibuf -> int = "wrap_read16"
external ser16 : obuf -> int -> unit = "wrap_write16"
external deser32 : ibuf -> int32 = "wrap_read32"
external ser32 : obuf -> int32 -> unit = "wrap_write32"
external deser64 : ibuf -> int64 = "wrap_read64"
external ser64 : obuf -> int64 -> unit = "wrap_write64"
external deser_varint : ibuf -> int = "wrap_read_varint"
external ser_varint : obuf -> int -> unit = "wrap_write_varint"
external deser_string : ibuf -> string = "wrap_read_string"
external ser_string : obuf -> string -> unit = "wrap_write_string"
external deser_chars : ibuf -> int -> char array = "wrap_read_chars"
external ser_chars : obuf -> char array -> unit = "wrap_write_chars"

let with_file_in fname f =
    let ibuf = make_ibuf fname in
    BatPervasives.with_dispose ~dispose:close_ibuf f ibuf

let with_file_out fname f =
    let obuf = make_obuf fname in
    BatPervasives.with_dispose ~dispose:close_obuf f obuf
