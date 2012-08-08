open Bricabrac

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
    val hash      : t -> int
    val write     : Output.t -> t -> unit
    val write_txt : Output.t -> t -> unit
    val read      : BinInput.t -> t
    val read_txt  : TxtInput.t -> t
end

module type DATATYPE =
sig
    include DATATYPE_BASE

    (* automatically added by Datatype_of functor: *)
    val of_string : string -> t
    val to_string : t -> string
end

module Datatype_of (B : DATATYPE_BASE) :
    DATATYPE with type t = B.t =
struct
    include B
    let of_string str =
        TxtInput.of_string str |> read_txt
    let to_string t =
        let buf = Buffer.create 32 in
        write_txt (Output.of_buffer buf) t ;
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
    val lt   : t -> t -> bool
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

module MakeVarInt (Int : sig
        type t
        val zero : t
        val i128 : t
        val succ : t -> t
        val logor : t -> t -> t
        val shift_left : t -> int -> t
        val shift_right_logical : t -> int -> t
        val of_int : int -> t
        val to_int : t -> int
        val neg : t -> t
    end) =
struct
    let write oc n =
        (* Recode negative number so that higher bits are 0 : stores (abs(n) lsl 1)+sign.
         * Notice that if n > 0 then we can mult it by 2 since we have a 0 bit at highest
         * position, but then ocaml will think it's negative again, so we have to
         * consider than n<0 means n is actually huge. *)
        let n = if n >= Int.zero then Int.shift_left n 1
                else Int.succ (Int.shift_left (Int.neg n) 1) in
        let rec aux n =
            if n >= Int.zero && n < Int.i128 then Output.byte oc (Int.to_int n)
            else (
                Output.byte oc ((Int.to_int n) lor 128) ;
                aux (Int.shift_right_logical n 7)
            ) in
        aux n

    let rec read ic =
        let rec aux n dec =
            let b = BinInput.read ic in
            (* FIXME: 3*loop+1 allocs for reading a single 64bit value is too much! *)
            let n = Int.logor n (Int.shift_left (Int.of_int (b land 127)) dec) in
            if b < 128 then n else aux n (dec+7) in
        let n = aux Int.zero 0 in
        (* bit 0 is actually the sign bit *)
        (if Int.to_int n land 1 = 0 then id else Int.neg)
            (Int.shift_right_logical n 1)
end

(*module VarInt64 = struct
    let read ic =
        let bits = BinInput.nread ic 8 |>
                   Bitstring.bitstring_of_string in
        bitmatch bits with { n : 64 } -> n
end*)
module VarInt64 = MakeVarInt(struct include Int64 let i128=128L end)
module VarInt32 = MakeVarInt(struct include Int32 let i128=128l end)
module VarInt = MakeVarInt(
    struct
        type t = int
        let zero = 0
        let i128 = 128
        let succ n = n+1
        let logor = (lor)
        let shift_left = (lsl)
        let shift_right_logical = (lsr)
        let of_int = id
        let to_int = id
        let neg n = -n
    end)

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

module Bool : DATATYPE with type t = bool =
Datatype_of (struct
    type t = bool
    let equal = (=)
    let hash = function true -> 1 | false -> 0
    let name = "bool"
    let write oc t = Output.byte oc (if t then 1 else 0)
    let write_txt oc t = Output.char oc (if t then 't' else 'f')
    let read ic = BinInput.read ic = 1
    let read_txt ic = TxtInput.read ic = 't'
end)

module Void : DATATYPE with type t = unit =
Datatype_of (struct
    type t = unit
    let equal () () = true
    let hash () = 0
    let name = "void"
    let write _ _ = ()
    let write_txt _ _ = ()
    let read _ = ()
    let read_txt _ = ()
end)

module Text : DATATYPE with type t = string =
Datatype_of (struct
    type t = string
    let name = "string"
    let equal = (=)
    let hash = Hashtbl.hash
    let write oc t =
        VarInt.write oc (String.length t) ;
        Output.string oc t
    let write_txt = Output.string
    let read ic =
        let len = VarInt.read ic in
        assert (len >= 0) ;
        BinInput.nread ic len
    let read_txt ic =
        read_txt_until ic "\t\n"
end)

module Float : NUMBER with type t = float =
struct
    include Datatype_of (struct
        type t = float
        let equal = (=)
        let hash = Hashtbl.hash
        let name = "float"
        let write oc f =
            Int64.bits_of_float f |> VarInt64.write oc
        let write_txt oc f =
            let s = string_of_float f in
            Output.string oc s
        let read ic =
            VarInt64.read ic |> Int64.float_of_bits
        let read_txt ic =
            (* hello, I'm slow! *)
            Text.read_txt ic |> float_of_string
    end)
    let zero = 0.
    let add = (+.)
    let sub = (-.)
    let idiv t i = t /. (float_of_int i)
    let imul t i = t *. (float_of_int i)
    let mul t1 t2 = t1 *. t2
    let lt = (<)
end

module Integer : NUMBER with type t = int =
struct
    include Datatype_of (struct
        type t = int
        let name = "int"
        let equal = (=)
        let hash = id
        let write = VarInt.write
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
        let read = VarInt.read
        let read_txt ic =
            let neg = peek_sign ic in
            let rec aux v =
                let d = try TxtInput.peek ic with End_of_file -> '\n' in
                if d < '0' || d > '9' then v else (
                    TxtInput.swallow ic ;
                    aux (v*10 + (int_of_char d - Char.code '0'))
                ) in
            (if neg then (~-) else id) (aux 0)
    end)
    let zero = 0
    let add = (+)
    let sub = (-)
    let idiv = (/)
    let imul = ( * )
    let mul = ( * )
    let lt = (<)
end

module Integer8 : NUMBER with type t = int =
struct
    include Integer
    let name = "int8"
    let write = Output.byte
    let read ic =
        let n = BinInput.read ic in
        if n < 128 then n else n-256
    let zero = 0
    let add = (+)
    let sub = (-)
    let idiv = (/)
    let imul = ( * )
    let mul = ( * )
    let lt = (<)
end

module Integer16 : NUMBER with type t = int =
struct
    include Integer
    let name = "int16"
    let write oc t =
        Output.byte oc t ;
        Output.byte oc (t lsr 8)
    let read ic =
        let fst_b = BinInput.read ic in
        let snd_b = BinInput.read ic in
        let n = fst_b + (snd_b lsl 8) in
        if n < 32768 then n else n-65536
    let zero = 0
    let add = (+)
    let sub = (-)
    let idiv = (/)
    let imul = ( * )
    let mul = ( * )
    let lt = (<)
end

module Integer32 : NUMBER with type t = Int32.t =
struct
    include Datatype_of (struct
        type t = Int32.t
        let name = "int32"
        let equal = (=)
        let hash = Int32.to_int
        let write oc t = VarInt64.write oc (Int64.of_int32 t)
        let write_txt oc i = Printf.sprintf "%ld" i |> Output.string oc
        let read = VarInt32.read
        let read_txt ic =
            let neg = peek_sign ic in
            let rec aux v =
                let d = try TxtInput.peek ic with End_of_file -> '\n' in
                if d < '0' || d > '9' then v else (
                    TxtInput.swallow ic ;
                    aux (Int32.add (Int32.mul v 10l) (Int32.of_int (int_of_char d - Char.code '0')))
                ) in
            (if neg then Int32.neg else id) (aux 0l)
    end)
    let zero = 0l
    let add = Int32.add
    let sub = Int32.sub
    let idiv t i = Int32.div t (Int32.of_int i)
    let imul t i = Int32.mul t (Int32.of_int i)
    let mul = Int32.mul
    let lt = (<)
end

module Integer64 : NUMBER with type t = Int64.t =
struct
    include Datatype_of (struct
        type t = Int64.t
        let name = "int64"
        let equal = (=)
        let hash = Int64.to_int
        let write = VarInt64.write
        let write_txt oc i = Printf.sprintf "%Ld" i |> Output.string oc
        let read  = VarInt64.read
        let read_txt ic =
            let neg = peek_sign ic in
            let rec aux v =
                let d = try TxtInput.peek ic with End_of_file -> '\n' in (* any non digit char would do *)
                if d < '0' || d > '9' then v else (
                    TxtInput.swallow ic ;
                    aux (Int64.add (Int64.mul v 10L) (Int64.of_int (int_of_char d - Char.code '0')))
                ) in
            (if neg then Int64.neg else id) (aux 0L)
    end)
    let zero = 0L
    let add = Int64.add
    let sub = Int64.sub
    let idiv t i = Int64.div t (Int64.of_int i)
    let imul t i = Int64.mul t (Int64.of_int i)
    let mul = Int64.mul
    let lt = (<)
end

module InetAddr : DATATYPE with type t = Unix.inet_addr =
Datatype_of (struct
    type t = Unix.inet_addr
    let name = "inetAddr"
    let equal = (=)
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
end)

module Cidr : DATATYPE with type t = InetAddr.t * Integer16.t =
Datatype_of (struct
    type t = InetAddr.t * Integer16.t
    let name = "cidr"
    let equal (n1,m1) (n2,m2) =
        n1 = n2 && m1 = m2
    let hash (n,m) =
        InetAddr.hash n + m
    let write oc (n,m) =
        InetAddr.write oc n ; Integer16.write oc m
    let write_txt oc (n,m) =
        InetAddr.write_txt oc n ;
        Output.char oc '/' ;
        Integer16.write_txt oc m
    let read ic =
        let n = InetAddr.read ic in
        n, Integer16.read ic
    let read_txt ic =
        let n = read_txt_until ic "/\t\n" |>
                Unix.inet_addr_of_string in
        let delim = try TxtInput.peek ic with End_of_file -> '\n' in
        if delim = '/' then (
            TxtInput.swallow ic ;
            n, Integer16.read_txt ic
        ) else n, 32
end)

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
let iter_ips ((netaddr : Unix.inet_addr), mask) f =
    let net : string = Obj.magic netaddr in (* reveal the underlying string *)
    let net = bitstring_of_string net in
    let addr_bits_len = bitstring_length net in
    assert (mask <= addr_bits_len && mask >= 0) ;
    let width_bits = addr_bits_len - mask in
    if width_bits = 0 then (
        (* Special shortcut since bitstring do not allow length of 0 bits in (BITSTRING {...}) *)
        f netaddr
    ) else (
        let max_i = Int64.shift_left 1L width_bits in
        let rec aux i =
            if i < max_i then (
                let ip : Unix.inet_addr =
                    Bitstring.concat [
                        Bitstring.takebits mask net ;
                        (BITSTRING { i : width_bits }) ] |>
                    string_of_bitstring |>
                    Obj.magic in
                f ip ;
                aux (Int64.succ i)
            ) in
        aux 0L
    )

let subnet_size ((net : Unix.inet_addr), mask) =
    let net : string = Obj.magic net in
    let width_bits = String.length net * 8 - mask in
    1 lsl width_bits

module EthAddr : DATATYPE with type t = char * char * char * char * char * char =
Datatype_of (struct
    type t = char * char * char * char * char * char
    let name = "ethAddr"
    let equal = (=)
    let hash = Hashtbl.hash
    let write oc (a,b,c,d,e,f) =
        let open Output in
        char oc a ; char oc b ;
        char oc c ; char oc d ;
        char oc e ; char oc f
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
        BinInput.(let a = char ic in let b = char ic in
                  let c = char ic in let d = char ic in
                  let e = char ic in let f = char ic in
                  a,b,c,d,e,f)
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
end)

module Timestamp =
struct
    include Datatype_of (struct
        type t = Int64.t * int (* secs, usecs *)
        let name = "timestamp"
        let equal = (=)
        let hash (a, b) = Int64.to_int a + b
        let write oc (s,u) =
            VarInt64.write oc s ;
            VarInt.write oc u
        let write_txt oc (s,u) =
            Integer64.write_txt oc s ;
            Output.string oc "s " ;
            Integer.write_txt oc u ;
            Output.string oc "us"
        let read ic =
            let s = VarInt64.read ic in
            let u = VarInt.read ic in
            s, u
        let read_txt ic =
            let s = Integer64.read_txt ic in
            let u = try (
                while
                    let c = TxtInput.peek ic in c = 's' || c = ' '
                do
                    TxtInput.swallow ic
                done ;
                let u = Integer.read_txt ic in
                try while
                        let c = TxtInput.peek ic in c = 'u' || c = 's'
                    do
                        TxtInput.swallow ic
                    done ;
                    u
               with End_of_file -> u
            ) with End_of_file -> 0 in
            s, u
    end)

    let seconds (s,_u) = s
    let microseconds (_s,u) = u

    let zero = 0L, 0
    let add (s1,u1) (s2,u2) = Int64.add s1 s2, u1 + u2
    let sub (s1,u1) (s2,u2) =
        assert (s1 > s2 || (s1 = s2 && u1 > u2)) ;
        let s' = Int64.sub s1 s2 in
        if u1 > u2 then
            s', u1-u2
        else
            Int64.pred s', 1_000_000 + u1 - u2
    let idiv (s,u) i =
        let i64 = Int64.of_int i in
        Int64.div s i64,
        (Int64.div
            (Int64.add (Int64.mul (Int64.rem s i64) 1_000_000L) (Int64.of_int u))
            i64) |> Int64.to_int
    let imul (s,u) i =
        let i64 = Int64.of_int i in
        let u' = Int64.mul i64 (Int64.of_int u) in
        Int64.add (Int64.mul s i64) (Int64.div u' 1_000_000L),
        Int64.rem u' 1_000_000L |> Int64.to_int
    let mul _ _ = failwith "Cant mul timestamps!"
    let lt (s1,u1) (s2,u2) = s1 < s2 || (s1 = s2 && u1 < u2)
end

(* Useful to round a timestamp to some amount of seconds *)
let round_sec n sec =
    let n = Int64.of_int n in
    Int64.mul n (Int64.div sec n)

let round_timestamp ?(ceil=false) n (sec, _usec) =
    Int64.add (round_sec n sec) (if ceil then Int64.of_int n else 0L), 0

(*
   Functors to easily assemble more complex types
 *)

module ListOf (T : DATATYPE) :
    DATATYPE with type t = T.t list =
Datatype_of (struct
    type t = T.t list

    let rec equal a b = match a, b with
        | [], x -> x = []
        | a::a', b::b' -> T.equal a b && equal a' b'
        | _ -> false

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
end)

module Altern1 (T1:DATATYPE) :
    DATATYPE with type t = T1.t =
Datatype_of (struct
    type t = T1.t
    let equal = T1.equal
    let hash = T1.hash
    let name = "V1of1 of "^T1.name
    let write oc t1 =
        Output.byte oc 0 ; (* So that we will be able to decode it later when we add version *)
        T1.write oc t1
    let write_txt = T1.write_txt
    let read ic =
        let v = BinInput.read ic in
        if v <> 0 then Printf.fprintf stderr "bad version: %d\n%!" v ;
        assert (v = 0) ;
        T1.read ic
    let read_txt = T1.read_txt
end)

type ('a, 'b) versions_2 = V1of2 of 'a | V2of2 of 'b
module Altern2 (T1:DATATYPE) (T2:DATATYPE) :
    DATATYPE with type t = (T1.t, T2.t) versions_2 =
Datatype_of (struct
    type t = (T1.t, T2.t) versions_2
    let equal a b = match a, b with
        | V1of2 a, V1of2 b -> T1.equal a b
        | V2of2 a, V2of2 b -> T2.equal a b
        | _ -> false
    let hash = function
        | V1of2 a -> T1.hash a
        | V2of2 a -> T2.hash a
    let name = "V1of2 of "^T1.name^" | V2of2 of "^T2.name
    let write oc = function
        | V1of2 a -> Output.byte oc 0 ; T1.write oc a
        | V2of2 a -> Output.byte oc 1 ; T2.write oc a
    let write_txt oc = function
        | V1of2 a -> Output.string oc "v1:" ; T1.write_txt oc a
        | V2of2 a -> Output.string oc "v2:" ; T2.write_txt oc a
    let read ic = match BinInput.read ic with
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
end)

