open Batteries
open Serial

let peek_eof2nl ic =
    try TxtInput.peek ic with End_of_file -> '\n'
let swallow_all c ic =
    while peek_eof2nl ic = c do
        TxtInput.swallow ic
    done
let try_swallow_one c ic =
    if peek_eof2nl ic = c then TxtInput.swallow ic

(*
   DATATYPES
   What's a DATATYPE? A name, a compact (binary) and friendly (textual) writer and
   reader, an equality and a hash function.
 *)

module type DATATYPE_BASE =
sig
    type t

    val name      : string
    val equal     : t -> t -> bool
    val compare   : t -> t -> int
    val hash      : t -> int
    val write     : obuf -> t -> unit
    val write_txt : Output.t -> t -> unit
    val read      : ibuf -> t
    val read_txt  : TxtInput.t -> t
end

module type DATATYPE =
sig
    include DATATYPE_BASE

    (* automatically added by Datatype_of functor: *)
    val of_string : string -> t
    val to_string : t -> string
end

module Datatype_of (B : DATATYPE_BASE) =
struct
    let of_string str : B.t =
        let inp = TxtInput.of_string str in
        let v = B.read_txt inp in
        (* Check that we've consumed the whole string *)
        try ignore (TxtInput.peek inp) ; failwith "Cannot consume all input"
        with End_of_file -> v
    let to_string (t : B.t) =
        let buf = Buffer.create 32 in
        B.write_txt (Output.of_buffer buf) t ;
        Buffer.contents buf
end

(* Many of them are numbers of some sort *)

module type NUMBER =
sig
    include DATATYPE
    val zero : t
    val add  : t -> t -> t
    val sub  : t -> t -> t
    val idiv : t -> int -> t
    val imul : t -> int -> t
    val mul  : t -> t -> t
end

(* Some tools *)

let string_of_list l =
    let str = String.create (List.length l) in
    let rec aux i = function
        | [] -> str
        | c::l' -> str.[i] <- c ; aux (i+1) l' in
    aux 0 l

let write_char_list oc l =
    Output.string oc (string_of_list l)

module VarInt =
struct
    let write = ser_varint
    let read = deser_varint
end

module VarInt32 =
struct
    let write oc n = Int32.to_int n |> ser_varint oc
    let read ic = deser_varint ic |> Int32.of_int
end

module VarInt64 =
struct
    (* TODO: (de)ser_varint64 ? *)
    let write = ser64
    let read = deser64
end

let read_txt_until ic delim =
    let rec aux p =
        try (
            let b = TxtInput.peek ic in
            if String.contains delim b then (
                string_of_list (List.rev p)
            ) else (
                TxtInput.swallow ic ;
                aux (b :: p)
            )
        ) with End_of_file ->
            if p = [] then raise End_of_file
            else string_of_list (List.rev p) in
    aux []

let peek_sign ic =
    let c = TxtInput.peek ic in
    if c = '-' then (
        TxtInput.swallow ic ;
        true
    ) else if c = '+' then (
        TxtInput.swallow ic ;
        false
    ) else false

(* Some predefined types *)

module Bool_base = struct
    type t = bool
    let equal = (=)
    let compare a b = if a = b then 0 else if a then -1 else 1
    let hash = function true -> 1 | false -> 0
    let name = "bool"
    let write = ser1
    let write_txt oc t = Output.char oc (if t then 't' else 'f')
    let read = deser1
    let read_txt ic = TxtInput.read ic = 't'
end
module Bool : DATATYPE with type t = bool =
struct
    include Bool_base
    include Datatype_of(Bool_base)
end

module Void_base = struct
    type t = unit
    let equal () () = true
    let compare () () = 0
    let hash () = 0
    let name = "void"
    let write _ _ = ()
    let write_txt _ _ = ()
    let read _ = ()
    let read_txt _ = ()
end
module Void : DATATYPE with type t = unit =
struct
    include Void_base
    include Datatype_of(Void_base)
end

module Text_base = struct
    type t = string
    let name = "string"
    let equal = (=)
    let compare = String.compare
    let hash = Hashtbl.hash
    let write oc t =
        ser_string oc t
    let write_txt = Output.string
    let read ic =
        deser_string ic
    let read_txt ic =
        read_txt_until ic "\t\n"
end
module Text : DATATYPE with type t = string =
struct
    include Text_base
    include Datatype_of(Text_base)
end

module Float_base = struct
    type t = float
    let equal = (=)
    let compare a b = if a = b then 0 else if a < b then -1 else 1
    let hash = Hashtbl.hash
    let name = "float"
    let write oc f =
        Int64.bits_of_float f |> ser64 oc
    let write_txt oc f =
        let s = string_of_float f in
        Output.string oc s
    let read ic =
        deser64 ic |> Int64.float_of_bits
    let read_txt ic =
        (* hello, I'm slow! *)
        Text.read_txt ic |> float_of_string
end
module Float : NUMBER with type t = float =
struct
    include Float_base
    include Datatype_of(Float_base)
    let zero = 0.
    let add = (+.)
    let sub = (-.)
    let idiv t i = t /. (float_of_int i)
    let imul t i = t *. (float_of_int i)
    let mul t1 t2 = t1 *. t2
end

exception Overflow
module Integer_base = struct
    type t = int
    let name = "int"
    let equal = (=)
    let compare a b = if a = b then 0 else if a < b then -1 else 1
    let hash = identity
    let write = ser_varint
    let write_txt oc i =
        if i = 0 then Output.char oc '0' else
        let rec aux l p i =
            if i = 0 then write_char_list oc p else
            aux (1+l) (Char.unsafe_chr ((i mod 10) + Char.code '0') :: p) (i/10) in
        if i > 0 then aux 0 [] i
        else (
            Output.char oc '-' ;
            aux 0 [] (-i)
        )
    let read = deser_varint
    let read_txt ic =
        let neg = peek_sign ic in
        let rec aux v =
            let d = peek_eof2nl ic in
            if d < '0' || d > '9' then v else (
                TxtInput.swallow ic ;
                let new_v = v*10 + (int_of_char d - Char.code '0') in
                if new_v < v then raise Overflow else aux new_v
            ) in
        (if neg then (~-) else identity) (aux 0)
end
module Integer : NUMBER with type t = int =
struct
    include Integer_base
    include Datatype_of(Integer_base)
    let zero = 0
    let add = (+)
    let sub = (-)
    let idiv = (/)
    let imul = ( * )
    let mul = ( * )
end

module UInteger : NUMBER with type t = int =
struct
    include Integer
    let read_txt ic =
        let rec aux v =
            let d = peek_eof2nl ic in
            if Char.is_digit d then (
                TxtInput.swallow ic ;
                let new_v = v*10 + (int_of_char d - Char.code '0') in
                if new_v < v then raise Overflow else aux new_v
            ) else v in
        aux 0
end

(* FIXME: use phantom types to prevent this uint8 to be compatible with an uint16? *)
module UInteger8 : NUMBER with type t = int =
struct
    include Integer
    let name = "uint8"
    let write = ser8
    let read = deser8
    let checked n = if n < 0 || n >= 256 then raise Overflow else n
    let read_txt ic =
        read_txt ic |> checked
    let of_string s =
        of_string s |> checked
end

module Integer8 : NUMBER with type t = int =
struct
    include Integer
    let name = "int8"
    let write = ser8
    let read ic =
        let n = deser8 ic in
        if n < 128 then n else n-256
    let checked n = if n < -128 || n >= 128 then raise Overflow else n
    let read_txt ic =
        read_txt ic |> checked
    let of_string s =
        of_string s |> checked
end

module UInteger16 : NUMBER with type t = int =
struct
    include Integer
    let name = "uint16"
    let write = ser16
    let read = deser16
    let checked n = if n < 0 || n >= 65536 then raise Overflow else n
    let read_txt ic =
        read_txt ic |> checked
    let of_string s =
        of_string s |> checked
end

module Integer16 : NUMBER with type t = int =
struct
    include UInteger16
    let name = "int16"
    let read ic =
        let n = deser16 ic in
        if n < 32768 then n else n-65536
    let checked n = if n < -32768 || n >= 32768 then raise Overflow else n
    let read_txt ic =
        read_txt ic |> checked
    let of_string s =
        of_string s |> checked
end

module UInteger32_base = struct
    type t = Int32.t
    let name = "int32"
    let equal = (=)
    let compare a b = if a = b then 0 else if a < b then -1 else 1
    let hash = Int32.to_int
    let write = ser32
    let write_txt oc i = Printf.sprintf "%lu" i |> Output.string oc
    let read = deser32
    let read_txt ic =
        let rec aux v =
            let d = peek_eof2nl ic in
            if d < '0' || d > '9' then v else (
                TxtInput.swallow ic ;
                let new_v = Int32.add (Int32.mul v 10l) (Int32.of_int (int_of_char d - Char.code '0')) in
                if new_v < v then raise Overflow else aux new_v
            ) in
        aux 0l
end
module UInteger32 : NUMBER with type t = Int32.t =
struct
    include UInteger32_base
    include Datatype_of(UInteger32_base)
    let zero = 0l
    let add = Int32.add
    let sub = Int32.sub
    let idiv t i = Int32.div t (Int32.of_int i)
    let imul t i = Int32.mul t (Int32.of_int i)
    let mul = Int32.mul
end

module Integer32 : NUMBER with type t = Int32.t =
struct
    include UInteger32
    let name = "uint32"
    let write_txt oc i = Printf.sprintf "%ld" i |> Output.string oc
    let read_txt ic =
        let neg = peek_sign ic in
        let n = read_txt ic in
        if neg then Int32.neg n else n
    (* TODO: check overflow *)
end

module UInteger64_base = struct
    type t = Int64.t
    let name = "int64"
    let equal = (=)
    let compare a b = if a = b then 0 else if a < b then -1 else 1
    let hash = Int64.to_int
    let write = ser64
    let write_txt oc i = Printf.sprintf "%Lu" i |> Output.string oc
    let read = deser64
    let read_txt ic =
        let rec aux ?(first=false) v =
            let d = try TxtInput.peek ic with End_of_file when not first -> '\n' in (* any non digit char would do *)
            if d < '0' || d > '9' then v else (
                TxtInput.swallow ic ;
                let new_v = Int64.add (Int64.mul v 10L) (Int64.of_int (int_of_char d - Char.code '0')) in
                if new_v < v then raise Overflow else aux new_v
            ) in
        aux ~first:true 0L
end
module UInteger64 : NUMBER with type t = Int64.t =
struct
    include UInteger64_base
    include Datatype_of (UInteger64_base)
    let zero = 0L
    let add = Int64.add
    let sub = Int64.sub
    let idiv t i = Int64.div t (Int64.of_int i)
    let imul t i = Int64.mul t (Int64.of_int i)
    let mul = Int64.mul
end

module Integer64 : NUMBER with type t = Int64.t =
struct
    include UInteger64
    let name = "uint64"
    let write_txt oc i = Printf.sprintf "%Ld" i |> Output.string oc
    let read_txt ic =
        let neg = peek_sign ic in
        let n = read_txt ic in
        if neg then Int64.neg n else n
    (* TODO: check overflow *)
end

module InetAddr_base = struct
    type t = Unix.inet_addr
    let name = "inetAddr"
    let equal = (=)
    let compare = compare
    let hash = Hashtbl.hash
    let write oc t =
        let (str : string) = Obj.magic t in (* underlying repr of a inet_addr is a string for some reason, probably to handle both v4 and v6 *)
        Text.write oc str
    let write_txt oc t =
        Unix.string_of_inet_addr t |>
        Output.string oc
    let read ic =
        let str = Text.read ic in
        Obj.magic str
    let read_txt ic =
        Text.read_txt ic |>
        Unix.inet_addr_of_string
end
module InetAddr : DATATYPE with type t = Unix.inet_addr =
struct
    include InetAddr_base
    include Datatype_of(InetAddr_base)
end

module Cidr_base = struct
    type t = InetAddr.t * UInteger16.t
    let name = "cidr"
    let equal (n1,m1) (n2,m2) =
        n1 = n2 && m1 = m2
    let compare = compare
    let hash (n,m) =
        InetAddr.hash n + m
    let write oc (n,m) =
        InetAddr.write oc n ; UInteger16.write oc m
    let write_txt oc (n,m) =
        InetAddr.write_txt oc n ;
        Output.char oc '/' ;
        UInteger16.write_txt oc m
    let read ic =
        let n = InetAddr.read ic in
        n, UInteger16.read ic
    let read_txt ic =
        let n = read_txt_until ic "/\t\n" |>
                Unix.inet_addr_of_string in
        let delim = peek_eof2nl ic in
        if delim = '/' then (
            TxtInput.swallow ic ;
            n, UInteger16.read_txt ic
        ) else n, 32
end
module Cidr : DATATYPE with type t = InetAddr.t * UInteger16.t =
struct
    include Cidr_base
    include Datatype_of (Cidr_base)
end

(* Usefull function to check if an IP belongs to a CIDR *)
let in_cidr (addr : Unix.inet_addr) ((net : Unix.inet_addr), mask) =
    let n = Obj.magic net and a = Obj.magic addr in
    assert (mask <= String.length n * 8 && mask >= 0) ;
    let rec addr_match i =
        let ib = i*8 in
        ib >= mask || (
            let m = if mask >= ib+8 then 0xff else 0xff lsl (8 - (mask - ib)) in
            Char.code a.[i] land m = Char.code n.[i] &&
            addr_match (i+1)
        ) in
    String.length n = String.length a && addr_match 0

(* Tells whether the intersection of 2 CIDR is not empty *)
let inter_cidr (((net1 : Unix.inet_addr), _mask1) as cidr1) (((net2 : Unix.inet_addr), _mask2) as cidr2) =
    in_cidr net1 cidr2 || in_cidr net2 cidr1

(* Usefull function to degrade an InetAddr into a subnet.
  Returns IP*mask *)
let cidr_of_inetaddr subnets ip =
    let masklen_of (t : Unix.inet_addr) =
        let s = Obj.magic t in String.length s * 8 in
    let net, mask =
        try List.find (in_cidr ip) subnets
        with Not_found -> ip, masklen_of ip in
    net, mask

open Bitstring
let fold_ips ((netaddr : Unix.inet_addr), mask) f p =
    let net : string = Obj.magic netaddr in (* reveal the underlying string *)
    let net = bitstring_of_string net in
    let addr_bits_len = bitstring_length net in
    assert (mask <= addr_bits_len && mask >= 0) ;
    let width_bits = addr_bits_len - mask in
    if width_bits = 0 then (
        (* Special shortcut since bitstring do not allow length of 0 bits in (BITSTRING {...}) *)
        f netaddr p
    ) else (
        let max_i = Int64.shift_left 1L width_bits in
        let rec aux i p =
            if i >= max_i then p else
            let ip : Unix.inet_addr =
                Bitstring.concat [
                    Bitstring.takebits mask net ;
                    (BITSTRING { i : width_bits }) ] |>
                string_of_bitstring |>
                Obj.magic in
            aux (Int64.succ i) (f ip p) in
        aux 0L p
    )

let iter_ips cidr f =
    fold_ips cidr (fun ip () -> f ip) ()

let subnet_size ((net : Unix.inet_addr), mask) =
    let net : string = Obj.magic net in
    let width_bits = String.length net * 8 - mask in
    1 lsl width_bits

module EthAddr_base = struct
    type t = char * char * char * char * char * char
    let name = "ethAddr"
    let equal = (=)
    let compare (a1,a2,a3,a4,a5,a6) (b1,b2,b3,b4,b5,b6) =
        let c = int_of_char b1 - int_of_char a1 in if c <> 0 then c else
        let c = int_of_char b2 - int_of_char a2 in if c <> 0 then c else
        let c = int_of_char b3 - int_of_char a3 in if c <> 0 then c else
        let c = int_of_char b4 - int_of_char a4 in if c <> 0 then c else
        let c = int_of_char b5 - int_of_char a5 in if c <> 0 then c else
        int_of_char b6 - int_of_char a6
    let hash = Hashtbl.hash
    let write oc t =
        let (a : char array) = Obj.magic t in
        ser_chars oc a
    let write_txt oc (a,b,c,d,e,f) =
        let byte n =
            let n = Char.code n in
            Output.hexdigit oc ((n lsr 4) land 15) ;
            Output.hexdigit oc (n land 15) in
        let byte_sep n = byte n ; Output.char oc ':' in
        byte_sep a ; byte_sep b ;
        byte_sep c ; byte_sep d ;
        byte_sep e ; byte f
    let read ic =
        let a = deser_chars ic 6 in
        Obj.magic a
    let read_txt ic =
        let byte () =
            let hi = TxtInput.hexdigit ic in
            let lo = TxtInput.hexdigit ic in
            let n = (hi lsl 4) lor lo in
            Char.unsafe_chr n in
        let byte_sep () =
            let b = byte () in
            let s = TxtInput.read ic in assert (s = ':') ;
            b in
        let a = byte_sep () in let b = byte_sep () in
        let c = byte_sep () in let d = byte_sep () in
        let e = byte_sep () in let f = byte () in
        a,b,c,d,e,f
end
module EthAddr : DATATYPE with type t = char * char * char * char * char * char =
struct
    include EthAddr_base
    include Datatype_of(EthAddr_base)
end

module Timestamp = struct
    include UInteger64 (* for number of milliseconds *)
    let name = "timestamp"
    let read_txt ic =
        let s = UInteger64.read_txt ic in
        let ms =
            if peek_eof2nl ic = '.' then (
                (* floating point notation *)
                TxtInput.swallow ic ;
                let rec read_next_digit value nb_digits =
                    let to_digit c = Char.code c - Char.code '0' in (* FIXME: move me in Batteries *)
                    let c = peek_eof2nl ic in
                    if Char.is_digit c then (
                        TxtInput.swallow ic ;
                        let c = to_digit c in
                        let value' = value + match nb_digits with
                            | 0 -> c*100
                            | 1 -> c*10
                            | 2 -> c*1
                            | 3 -> if c > 5 then 1 else 0
                            | _ -> 0 in
                        read_next_digit value' (succ nb_digits)
                    ) else value in
                read_next_digit 0 0
            ) else (
                swallow_all ' ' ic ;
                try_swallow_one 's' ic ;
                swallow_all ' ' ic ;
                let ms = ref (try UInteger.read_txt ic with End_of_file -> 0) in
                swallow_all ' ' ic ;
                let c = peek_eof2nl ic in
                if c = 'u' then (
                    (* microseconds!*)
                    TxtInput.swallow ic ;
                    ms := (!ms+499) / 1000
                ) else if c = 'm' then
                    TxtInput.swallow ic ;
                try_swallow_one 's' ic ;
                !ms
            ) in
        Int64.add (Int64.mul s 1000L)
                  (Int64.of_int ms)

    let seconds t = Int64.div t 1000L
    let milliseconds t = Int64.rem t 1000L
    let of_unixfloat ts = Int64.of_float (1000. *. ts)
    let to_unixfloat t = (Int64.to_float t) *. 0.001

    let zero = 0L
    let add = Int64.add
    let sub = Int64.sub
    let idiv t i =
        let i64 = Int64.of_int i in
        Int64.div t i64
    let imul t i =
        let i64 = Int64.of_int i in
        Int64.mul t i64
    let mul _ _ = failwith "Cannot mul timestamps!"

    (* Helper to build a Timestamp.t from a user friendly string *)
    exception Invalid_date of (int * int * int * int * int * float)
    exception Invalid_unit of char

    let of_tm y mo d h mi s =
        let open Unix in
        if y > 2100 || y < 2000 || mo < 1 || mo > 12 ||
           d < 1 || d > 31 || h > 24 || mi > 60 || s > 60. then
           raise (Invalid_date (y, mo, d, h, mi, s)) ;
        let s_f, s_i = modf s in
        let tm = { tm_sec = int_of_float s_i ;
                   tm_min = mi ; tm_hour = h ;
                   tm_mday = d ; tm_mon = mo-1 ;
                   tm_year = y - 1900 ;
                   (* rest is ignored by mktime *)
                   tm_wday = 0 ; tm_yday = 0 ; tm_isdst = false } in
        let ts, _ = mktime tm in
        of_unixfloat (ts +. s_f)

    let of_datestring str =
        let open Unix in
        let to_min   y mo d h mi = of_tm y mo d h mi 0. in
        let to_hour  y mo d h    = of_tm y mo d h 0  0. in
        let to_day   y mo d      = of_tm y mo d 0 0  0. in
        let to_month y mo        = of_tm y mo 1 0 0  0. in
        let to_year  y           = of_tm y 1  1 0 0  0. in
        let today_full h mi s =
            let tm = localtime (time ()) in
            of_tm (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday h mi s in
        let today_to_min  h mi = today_full h mi 0. in
        let today_to_hour h    = today_full h 0 0. in
        try (
            try Scanf.sscanf str "%4u-%2u-%2u %2u:%2u:%f" of_tm
            with End_of_file -> (
                try Scanf.sscanf str "%4u-%2u-%2u %2u:%2u" to_min
                with End_of_file ->
                    try Scanf.sscanf str "%4u-%2u-%2u %2u" to_hour
                    with End_of_file ->
                        try Scanf.sscanf str "%4u-%2u-%2u" to_day
                        with End_of_file ->
                            try Scanf.sscanf str "%4u-%2u" to_month
                            with End_of_file ->
                                Scanf.sscanf str "%4u" to_year)
        ) with _ -> (
            try Scanf.sscanf str "%2u:%2u:%f" today_full
            with End_of_file ->
                try Scanf.sscanf str "%2u:%2u" today_to_min
                with End_of_file ->
                    Scanf.sscanf str "%2u" today_to_hour
        )

    let of_interval str =
        let open Unix in
        let rel_to_now s i u =
            let now = localtime (time ()) in
            let i = if s = "-" then ~-i else i in
            let u = Char.lowercase u in
            if u <> 'y' && u <> 'm' && u <> 'd' &&
               u <> 'h' && u <> 'n' && u <> 's' then
                raise (Invalid_unit u) ;
            of_tm (now.tm_year + 1900 + if u = 'y' then i else 0)
                  (now.tm_mon  +    1 + if u = 'm' then i else 0)
                  (now.tm_mday        + if u = 'd' then i else 0)
                  (now.tm_hour        + if u = 'h' then i else 0)
                  (now.tm_min         + if u = 'n' then i else 0)
                 ((now.tm_sec         + if u = 's' then i else 0) |> float_of_int)
            in
        Scanf.sscanf str "%1[+-]%u %c" rel_to_now

    let of_string str : t =
        try of_datestring str
        with _ ->
            try of_interval str
            with _ -> of_string str

    let to_string (t : t) =
        let open Unix in
        let tm = localtime (to_unixfloat t) in
        let s = (float_of_int tm.tm_sec) +. (milliseconds t |> Int64.to_float)*.0.001 in
        Printf.sprintf "%04u-%02u-%02u %02u:%02u:%06.3f"
            (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
            tm.tm_hour tm.tm_min s

    let write_txt oc t =
        let str = to_string t in
        Output.string oc str
end

let round_timestamp ?(ceil=false) n t =
    let n = Int64.of_int n in
    let r = Int64.rem t n in
    let t' = Int64.sub t r in
    if ceil && r <> 0L then Int64.add t' n else t'

(*
   Functors to easily assemble more complex types
 *)

module ListOf_base (T : DATATYPE) = struct
    type t = T.t list

    let rec equal a b = match a, b with
        | [], x -> x = []
        | a::a', b::b' -> T.equal a b && equal a' b'
        | _ -> false

    let rec compare a b = match a, b with
        | [], x -> if x = [] then 0 else -1
        | _, [] -> 1
        | a::a', b::b' ->
            let c = T.compare a b in
            if c = 0 then compare a' b'
            else c

    let hash = Hashtbl.hash (* Hum no! should use the T.hash function on the first elements *)

    let name = "(listof " ^ T.name ^ ")"

    let write oc t =
        VarInt.write oc (List.length t) ;
        List.iter (T.write oc) t

    let write_txt oc t =
        Output.char oc '[' ;
        let rec aux = function
        | []   -> () (* it's stupid to search for this at each iteration, but the compiler warns if [] is unmatched *)
        | [x]  -> T.write_txt oc x
        | x::l -> T.write_txt oc x ;
                  Output.char oc ';' ;
                  aux l in
        aux t ;
        Output.char oc ']'

    let read ic =
        let len = VarInt.read ic in
        assert (len >= 0) ;
        let res = ref [] in
        for i = 1 to len do
            res := (T.read ic) :: !res
        done ;
        !res

    let read_txt ic =
        let fst = TxtInput.read ic in
        assert (fst = '[') ;
        let res =
            let rec aux p =
                if TxtInput.peek ic = ']' then p else
                aux (T.read_txt ic :: p) in
            aux [] in
        let lst = TxtInput.read ic in
        assert (lst = ']') ;
        res
end
module ListOf (T : DATATYPE) :
    DATATYPE with type t = T.t list =
struct
    include ListOf_base (T)
    include Datatype_of(ListOf_base (T))
end

module Altern1_base (T: DATATYPE) = struct
    type t = T.t
    let equal = T.equal
    let compare = T.compare
    let hash = T.hash
    let name = "V1of1 of "^T.name
    let write oc t1 =
        ser8 oc 0 ; (* So that we will be able to decode it later when we add version *)
        T.write oc t1
    let write_txt = T.write_txt
    let read ic =
        let v = deser8 ic in
        if v <> 0 then Printf.fprintf stderr "bad version: %d\n%!" v ;
        assert (v = 0) ;
        T.read ic
    let read_txt = T.read_txt
end
module Altern1 (T:DATATYPE) :
    DATATYPE with type t = T.t =
struct
    include Altern1_base (T)
    include Datatype_of(Altern1_base (T))
end

type ('a, 'b) versions_2 = V1of2 of 'a | V2of2 of 'b
module Altern2_base (T1 : DATATYPE) (T2 : DATATYPE) = struct
    type t = (T1.t, T2.t) versions_2
    let equal a b = match a, b with
        | V1of2 a, V1of2 b -> T1.equal a b
        | V2of2 a, V2of2 b -> T2.equal a b
        | _ -> false
    let compare a b = match a, b with
        | V1of2 a, V1of2 b -> T1.compare a b
        | V2of2 a, V2of2 b -> T2.compare a b
        | V1of2 _, V2of2 _ -> -1
        | V2of2 _, V1of2 _ -> 1
    let hash = function
        | V1of2 a -> T1.hash a
        | V2of2 a -> T2.hash a
    let name = "V1of2 of "^T1.name^" | V2of2 of "^T2.name
    let write oc = function
        | V1of2 a -> ser8 oc 0 ; T1.write oc a
        | V2of2 a -> ser8 oc 1 ; T2.write oc a
    let write_txt oc = function
        | V1of2 a -> Output.string oc "v1:" ; T1.write_txt oc a
        | V2of2 a -> Output.string oc "v2:" ; T2.write_txt oc a
    let read ic = match deser8 ic with
        | 0 -> V1of2 (T1.read ic)
        | 1 -> V2of2 (T2.read ic)
        | v -> failwith ("Bad version "^string_of_int v)
    let read_txt ic =
        let v = TxtInput.nread ic 3 in
        assert (v.[0] = 'v' && v.[2] = ':') ;
        match v.[1] with
        | '1' -> V1of2 (T1.read_txt ic)
        | '2' -> V2of2 (T2.read_txt ic)
        | c -> failwith ("Bad version "^Char.escaped c)
end
module Altern2 (T1:DATATYPE) (T2:DATATYPE) :
    DATATYPE with type t = (T1.t, T2.t) versions_2 =
struct
    include Altern2_base (T1) (T2)
    include Datatype_of(Altern2_base (T1) (T2))
end

module Option_base (T : DATATYPE) = struct
    type t = T.t option
    let equal a b = match a,b with
        | Some x, Some y -> T.equal x y
        | _ -> false
    let compare a b = match a,b with
        | Some x, Some y -> T.compare x y
        | Some _, None -> 1
        | None, Some _ -> -1
        | None, None -> 0
    let hash = function
        | Some x -> T.hash x
        | None -> 0
    let name = T.name^" option"
    let write oc = function
        | None -> ser8 oc 0
        | Some x ->
            ser8 oc 1 ;
            T.write oc x
    let write_txt oc = function
        | None -> Output.string oc "None"
        | Some x -> Output.string oc "Some " ;
                    T.write_txt oc x
    let read ic =
        let o = deser8 ic in
        if o <> 0 then (
            assert (o = 1) ;
            Some (T.read ic)
        ) else None
    let read_txt ic =
        let o = TxtInput.nread ic 4 in
        if o = "None" then None
        else (
            assert (o = "Some") ;
            let sep = TxtInput.read ic in assert (sep = ' ') ;
            Some (T.read_txt ic)
        )
end
module Option (T:DATATYPE) :
    DATATYPE with type t = T.t option =
struct
    include Option_base (T)
    include Datatype_of(Option_base (T))
end

