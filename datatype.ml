open Batteries
open Serial

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
    val to_imm    : t -> string
    (* picky is when you are not sure the value may be of the correct type (false by default) *)
    val parzer    : ?picky:bool -> (t, char) Peg.parzer
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
        let open Peg in
        match (B.parzer ++ eof) (String.to_list str) with
        | Res ((v,_), _) -> v
        | Fail -> raise Parse_error
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

(* Some predefined types *)

module Bool_base = struct
    (*$< Bool_base *)
    type t = bool
    let equal (a:t) (b:t) = a = b
    let compare a b = if a = b then 0 else if a then -1 else 1
    let hash = function true -> 1 | false -> 0
    let name = "bool"
    let write = ser1
    let write_txt oc t = Output.char oc (if t then 't' else 'f')
    let read = deser1
    let to_imm = function true -> "true" | false -> "false"

    let parzer ?(picky=false) =
        ignore picky ;
        let open Peg in
        either [ istring "true" >>: (fun _ -> true) ;
                 istring "false" >>: (fun _ -> false) ]
    (*$T parzer
      parzer (String.to_list "TrUe") = Peg.Res (true, [])
      parzer (String.to_list "fAlse") = Peg.Res (false, [])
      parzer (String.to_list "pas glop") = Peg.Fail
     *)

    (*$>*)
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
    let to_imm _ = "()"
    let parzer ?(picky=false) =
        ignore picky ;
        Peg.return ()
end
module Void : DATATYPE with type t = unit =
struct
    include Void_base
    include Datatype_of(Void_base)
end

module Text_base = struct
    type t = string
    let name = "string"
    let equal (a:t) (b:t) = a = b
    let compare = String.compare
    let hash = Hashtbl.hash
    let write oc t =
        ser_string oc t
    let write_txt = Output.string
    let read ic =
        deser_string ic
    let to_imm t = "\""^ String.escaped t ^"\""
    let parzer ?(picky=false) =
        let open Peg in
        if picky then (
            item '"' ++ several (cond (fun c -> c <> '"')) ++ item '"' >>:
            function ((_,cs),_) -> String.of_list cs
        ) else (
            any (cond (fun c -> c <> '\t' && c <> '\n')) >>: String.of_list
        )
end
module Text : DATATYPE with type t = string =
struct
    include Text_base
    include Datatype_of(Text_base)
end

module Float_base = struct
    (*$< Float_base *)
    type t = float
    let equal (a:t) (b:t) = a = b
    let compare (a:t) (b:t) = if a = b then 0 else if a < b then -1 else 1
    let hash = Hashtbl.hash
    let name = "float"
    let write oc f =
        Int64.bits_of_float f |> ser64 oc
    let write_txt oc f =
        let s = string_of_float f in
        Output.string oc s
    let read ic =
        deser64 ic |> Int64.float_of_bits
    let to_imm t = "("^ Float.to_string t ^")"    (* need parenths for negative values *)

    let parzer ?(picky=false) =
        ignore picky ;
        let open Peg in
        let sign =
            optional (either [ item '-' ; item '+' ]) >>: function Some '-' -> -1. | _ -> 1.
        and numeric_suffix_float =
            optional (
                either [
                    item 'G' >>: (fun _ -> 1_000_000_000.) ;
                    item 'M' >>: (fun _ -> 1_000_000.) ;
                    item 'K' >>: (fun _ -> 1_024.) ;
                    item 'k' >>: (fun _ -> 1_000.) ;
                    item 'm' >>: (fun _ -> 0.001) ;
                    item 'u' >>: (fun _ -> 0.000001) ;
                    item 'n' >>: (fun _ -> 0.000000001) ] ++
                check (either [ eof ; ign (cond (fun c -> not (Char.is_letter c))) ]) >>: fst
            ) >>: Option.default 1. in
        sign ++
        decimal_number ++
        optional (
            item '.' ++
            any (item '0') ++
            optional decimal_number) ++
        optional (
            item 'e' ++
            sign ++
            decimal_number
        ) ++
        numeric_suffix_float ++
        (* check we are not followed by a dot to disambiguate from all-numeric evil hostnamesb*)
        check (either [ eof ; ign (cond (fun c -> c <> '.')) ]) >>:
        fun (((((s,n),dec),exp),suf),_) ->
            let n = float_of_int n in
            let n = match dec with
                | None | Some ((_,_),None) -> s *. n
                | Some ((_,zeros),Some d) ->
                    let rec aux f d =
                        if d = 0 then f else
                        let lo = d mod 10 in
                        aux ((float_of_int lo +. f) *. 0.1) (d/10) in
                    let dec = aux 0. d in
                    let nb_zeros = List.length zeros in
                    s *. (n +. (Float.pow 0.1 (float_of_int nb_zeros) *. dec)) in
            let n = match exp with
                | None -> n
                | Some ((_,s),e) ->
                    n *. Float.pow 10. (s *. float_of_int e) in
            n *. suf

    (*$T parzer
      parzer (String.to_list "123.45670") = Peg.Res (123.4567, [])
      parzer (String.to_list "0.12") = Peg.Res (0.12, [])
      parzer (String.to_list "+1.2") = Peg.Res (1.2, [])
      parzer (String.to_list "-9.09") = Peg.Res (-9.09, [])
      parzer (String.to_list "1") = Peg.Res (1., [])
      parzer (String.to_list "1e3") = Peg.Res (1000., [])
      parzer (String.to_list "1.01e2") = Peg.Res (101., [])
      parzer (String.to_list "-10.1e-1") = Peg.Res (-1.01, [])
      parzer (String.to_list "1.5k") = Peg.Res (1500., [])
     *)

    (*$>*)
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

let string_of_vlan = function
    | None -> ""
    | Some v -> string_of_int v

let multiples_bytes = [| "B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB" |]
let multiples = [| ""; "k"; "M"; "G"; "T"; "P" |]
let unpower m v =
    let rec aux e v =
        if e >= Array.length multiples -1 || abs_float v < m then
            e, v
        else
            aux (succ e) (v/.m) in
    aux 0 v

let submultiples = [| ""; "m"; "u"; "n"; "p" |]
let power m v =
    let rec aux e v =
        if e >= Array.length submultiples -1 || abs_float v >= 1. then
            e, v
        else
            aux (succ e) (v*.m) in
    aux 0 v
let string_of_volume v =
    let e, v = unpower 1024. v in
    Printf.sprintf "%.*f%s" (if fst (modf v) < 0.01 then 0 else 2) v multiples_bytes.(e)
let string_of_float v =
    Printf.sprintf "%.*f" (if fst (modf v) <= 0.0005 then 0 else 3) v
let string_of_number v =
    if abs_float v >= 1. then (
        let e, v = unpower 1000. v in
        string_of_float v ^ multiples.(e)
    ) else (
        let e, v = power 1000. v in
        let s = string_of_float v in
        if s = "0" then s else s ^ submultiples.(e)
    )

let numeric_suffix_int =
    let open Peg in
    optional (either [
        item 'G' >>: (fun _ -> 1_000_000_000) ;
        item 'M' >>: (fun _ -> 1_000_000) ;
        item 'K' >>: (fun _ -> 1_024) ;
        item 'k' >>: (fun _ -> 1_000) ]) >>: Option.default 1

exception Overflow
module Integer_base = struct
    (*$< Integer_base *)
    type t = int
    let name = "int"
    let equal (a:t) (b:t) = a = b
    let compare (a:t) (b:t) = if a = b then 0 else if a < b then -1 else 1
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
    let to_imm t = "("^ Int.to_string t ^")"    (* need parenths for negative numbers *)

    let parzer ?(picky=false) =
        let open Peg in
        let sign =
            optional (either [ item '-' ; item '+' ]) >>: function Some '-' -> -1 | _ -> 1 in
        if picky then (
            sign ++ c_like_number ++ numeric_suffix_int ++
            check (either [ eof ; ign (cond (fun c -> c <> '.')) ]) >>:
            fun (((s,n),suf),_) -> s * n * suf
        ) else (
            sign ++ c_like_number ++ numeric_suffix_int >>:
            fun ((s,n),suf) -> s * n * suf
        )

    (*$T parzer
      parzer ['1';'2';'k'] = Peg.Res (12_000, [])
     *)

    (*$>*)
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
end

(* FIXME: use phantom types to prevent this uint8 to be compatible with an uint16? *)
module UInteger8 : NUMBER with type t = int =
struct
    include Integer
    let name = "uint8"
    let write = ser8
    let read = deser8
    let checked n = if n < 0 || n >= 256 then raise Overflow else n
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
    let of_string s =
        of_string s |> checked
end

module UInteger32_base = struct
    (*$< UInteger32_base *)
    type t = Int32.t
    let name = "int32"
    let equal (a:t) (b:t) = a = b
    let compare (a:t) (b:t) = if a = b then 0 else if a < b then -1 else 1
    let hash = Int32.to_int
    let write = ser32
    let write_txt oc i = Printf.sprintf "%lu" i |> Output.string oc
    let read = deser32
    let to_imm t = "("^ Int32.to_string t ^"l)"

    let parzer ?(picky=false) =
        ignore picky ;
        let open Peg in
        Number32.c_like_number ++ numeric_suffix_int >>:
        fun (n,s) -> Int32.mul n (Int32.of_int s)

    (*$T parzer
      parzer ['1';'2';'k'] = Peg.Res (12_000l, [])
     *)

    (*$>*)
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
    (* TODO: check overflow *)
end

module UInteger64_base = struct
    (*$< UInteger64_base *)
    type t = Int64.t
    let name = "int64"
    let equal (a:t) (b:t) = a = b
    let compare (a:t) (b:t) = if a = b then 0 else if a < b then -1 else 1
    let hash = Int64.to_int
    let write = ser64
    let write_txt oc i = Printf.sprintf "%Lu" i |> Output.string oc
    let read = deser64
    let to_imm t = "("^ Int64.to_string t ^"L)"

    let parzer ?(picky=false) =
        ignore picky ;
        let open Peg in
        Number64.c_like_number ++ numeric_suffix_int >>:
        fun (n,s) -> Int64.mul n (Int64.of_int s)

    (*$T parzer
      parzer ['1';'2';'k'] = Peg.Res (12_000L, [])
     *)

    (*$>*)


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
    (* TODO: check overflow *)
end

(* FIXME: purge old entries *)
(* FIXME: use a reentrant resolver *)
let gethostbyaddr_cache = Hashtbl.create 997
let cached_gethostbyaddr addr =
    match Hashtbl.find_option gethostbyaddr_cache addr with
    | Some x -> x
    | None ->
        let open Unix in
        let name = (gethostbyaddr addr).h_name in
        Hashtbl.add gethostbyaddr_cache addr name ;
        name

module InetAddr_base = struct
    (*$< InetAddr_base *)
    type t = Unix.inet_addr
    let name = "inetAddr"
    let equal = (=)
    let compare = compare
    let hash = Hashtbl.hash
    let write oc t =
        let (str : string) = Obj.magic t in (* underlying repr of a inet_addr is a string for some reason, probably to handle both v4 and v6 *)
        Text.write oc str
    let write_txt oc t =
        (let open Unix in
        if Prefs.get_bool "resolver/ip" false then
            try cached_gethostbyaddr t
            with Not_found -> string_of_inet_addr t
        else
            string_of_inet_addr t) |>
        Output.string oc
    let read ic =
        let str = Text.read ic in
        Obj.magic str
    let to_imm t =
        let (str : string) = Obj.magic t in
        "(Obj.magic \""^ String.escaped str ^"\" : Unix.inet_addr)"


    let dotted_ip_addr =
        let open Peg in
        times 4 ~sep:(item '.')
              (number_in_range 0 255) >>:
        (function [a;b;c;d] ->
            (* poor man's ip_addr_of_quad: *)
            let s = Printf.sprintf "%d.%d.%d.%d" a b c d in
            Unix.inet_addr_of_string s
         | _ -> assert false)

    let hostname =
        let open Peg in
        let namechar = either [ alphanum ; item '-' ] in
        let label = repeat ~min:1 ~max:63 namechar >>: String.of_list in
        several ~sep:(item '.') label >>: (String.concat ".")

    (*$T hostname
      hostname (String.to_list "www.google.com") = Peg.Res ("www.google.com", [])
      hostname (String.to_list "www.google.com glop") = Peg.Res ("www.google.com", [' ';'g';'l';'o';'p'])
      hostname (String.to_list "{/}") = Peg.Fail
     *)

    let parzer ?(picky=false) =
        let open Peg in
        let p =
            either [ dotted_ip_addr ;
                     hostname >>= fun s ->
                                    try return Unix.((gethostbyname s).h_addr_list.(0))
                                    with _ -> fail ] in
        if picky then p ++ check (either [ eof ; ign blank ]) >>: fst
        else p

    (*$T parzer
      parzer (String.to_list "123.123.123.123") = \
        Peg.Res (Unix.inet_addr_of_string "123.123.123.123", [])
      parzer (String.to_list "eneide.happyleptic.org") = \
        Peg.Res (Unix.inet_addr_of_string "213.251.171.101", [])
     *)

    (*$>*)
end
module InetAddr : DATATYPE with type t = Unix.inet_addr =
struct
    include InetAddr_base
    include Datatype_of(InetAddr_base)
end

module Cidr_base = struct
    (*$< Cidr_base *)
    type t = InetAddr.t * UInteger8.t
    let name = "cidr"
    let equal (n1,m1) (n2,m2) =
        n1 = n2 && m1 = m2
    let compare = compare
    let hash (n,m) =
        InetAddr.hash n + m
    let write oc (n,m) =
        InetAddr.write oc n ; UInteger8.write oc m
    let write_txt oc (n,m) =
        InetAddr.write_txt oc n ;
        Output.char oc '/' ;
        UInteger8.write_txt oc m
    let read ic =
        let n = InetAddr.read ic in
        n, UInteger8.read ic
    let to_imm (n,m) =
        "("^ InetAddr.to_imm n ^", "^ UInteger8.to_imm m ^")"

    let parzer ?(picky=false) =
        ignore picky ;
        let open Peg in
        either [ InetAddr_base.parzer ++ item '/' ++ number_in_range 0 32 >>:
                    (fun ((ip,_),w) -> ip, w) ;
                 InetAddr_base.parzer ~picky:true >>: fun ip -> ip, 32 ]

    (*$T parzer
      match parzer (String.to_list "192.168.1.10") with \
        | Peg.Res (_, []) -> true \
        | _ -> false
      match parzer (String.to_list "192.168.1.10/24") with \
        | Peg.Res (_, []) -> true \
        | _ -> false
      match parzer (String.to_list "192.168.1.10/32") with \
        | Peg.Res (_, []) -> true \
        | _ -> false
      match parzer (String.to_list "192.168.1.10/-2") with \
        | Peg.Fail -> true \
        | _ -> false
     *)

    (*$>*)
end
module Cidr : DATATYPE with type t = InetAddr.t * UInteger8.t =
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


let masklen_of (t : Unix.inet_addr) =
    let s = Obj.magic t in String.length s * 8

(* Usefull function to degrade an InetAddr into a subnet.
  Returns IP*mask *)
let cidr_of_inetaddr subnets ip =
    let net, mask =
        try List.find (in_cidr ip) subnets
        with Not_found -> ip, masklen_of ip in
    net, mask

let cidr_singleton ip = ip, masklen_of ip

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
    (*$< EthAddr_base *)
    type t = char * char * char * char * char * char
    let name = "ethAddr"
    let equal (a:t) (b:t) = a = b
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
    let to_imm (a,b,c,d,e,f) = Printf.sprintf "(%C,%C,%C,%C,%C,%C)" a b c d e f

    let parzer ?(picky=false) =
        ignore picky ;
        let open Peg in
        times 6 ~sep:(item ':') (digit 16 ++ digit 16 >>: fun (hi,lo) -> char_of_int (hi lsl 4 + lo)) >>:
        function [a;b;c;d;e;f] -> (a,b,c,d,e,f)
               | _ -> assert false

    (*$T parzer
      let c = char_of_int in parzer (String.to_list "12:34:56:78:9a:bc") = \
        Peg.Res ((c 0x12,c 0x34,c 0x56,c 0x78,c 0x9a,c 0xbc), [])
      let c = char_of_int in parzer (String.to_list "12:34:56:78:9A:BC") = \
        Peg.Res ((c 0x12,c 0x34,c 0x56,c 0x78,c 0x9a,c 0xbc), [])
     *)

    (*$>*)
end
module EthAddr : DATATYPE with type t = char * char * char * char * char * char =
struct
    include EthAddr_base
    include Datatype_of(EthAddr_base)
end

(**
  Time Interval, expressed in userfriendly time units
  (which actual sizes may depends on the point of reference)
 *)

module Interval_base = struct
    (*$< Interval_base *)
    type t = { years : float ; months : float ;
               weeks : float ; days   : float ;
               hours : float ; mins   : float ;
               secs  : float ; msecs  : float }

    let name = "interval"

    let compare t1 t2 =
        (* We do not try to equals different units when their equivalence is not trivial. *)
        (* FIXME: try to compute the overall ms number and if the diff is big
         * enought use it *)
        let w_2_ms t =     t.msecs
              +. 1_000. *. t.secs
             +. 60_000. *. t.mins
          +. 3_600_000. *. t.hours
         +. 86_400_000. *. t.days
        +. 604_800_000. *. t.weeks in
        let y_2_m t = t.years *. 12. +. t.months in
        let m1 = y_2_m t1 and m2 = y_2_m t2 in
        if m1 > m2 then 1 else
        if m1 < m2 then -1 else
        if t1.weeks > t2.weeks then 1 else
        if t1.weeks < t2.weeks then -1 else
        let ms1 = w_2_ms t1 and ms2 = w_2_ms t2 in
        if ms1 > ms2 then 1 else
        if ms2 < ms1 then -1 else
        0

    let equal t1 t2 =
        compare t1 t2 = 0

    let hash t = int_of_float (t.years +. t.months +. t.weeks +. t.days +. t.hours +. t.mins +. t.secs +. t.msecs)

    let write oc t =
        let ser oc v = Int64.bits_of_float v |> ser64 oc in
        ser oc t.years ;
        ser oc t.months ;
        ser oc t.weeks ;
        ser oc t.days ;
        ser oc t.hours ;
        ser oc t.mins ;
        ser oc t.secs ;
        ser oc t.msecs

    let write_txt oc t =
        let started = ref false in
        let w v u =
            if v <> 0. then (
                started := true ;
                string_of_float v ^ " " ^ u ^ (if abs_float v > 1. then "s" else "") |>
                Output.string oc
            ) in
        w t.years  "year" ;
        w t.months "month" ;
        w t.weeks  "week" ;
        w t.days   "day" ;
        w t.hours  "hour" ;
        w t.mins   "min" ;
        w t.secs   "sec" ;
        w t.msecs  "msec" ;
        if not !started then Output.char oc '0'

    (* Some of our fields are interchangeable... *)
    let normalize i =
        (* We normalize only msecs, secs, mins and hours.
           A week is always 7 days but we'd rather keep days if the user used days *)
        let tot_msecs =  i.msecs
            +. 1_000. *. i.secs
           +. 60_000. *. i.mins
        +. 3_600_000. *. i.hours in
        let div n k =
            let lo,hi = modf (n /. k) in
            hi, lo*.k in
        let tot_secs, msecs = div tot_msecs 1000. in
        let tot_mins, secs  = div tot_secs 60. in
        let hours, mins     = div tot_mins 60. in
        { i with msecs ; secs ; mins ; hours }

    let read ic =
        let deser ic = deser64 ic |> Int64.float_of_bits in
        let years  = deser ic in
        let months = deser ic in
        let weeks  = deser ic in
        let days   = deser ic in
        let hours  = deser ic in
        let mins   = deser ic in
        let secs   = deser ic in
        let msecs  = deser ic in
        normalize { years ; months ; weeks ; days ; hours ; mins ; secs ; msecs }

    let to_imm t =
        Printf.sprintf "Unix.({ year=(%F); months=(%F); weeks =(%F); days =(%F); hours =(%F); mins =(%F); secs = %F; msecs =(%F)})"
            t.years t.months t.weeks t.days t.hours t.mins t.secs t.msecs

    let zero =
        { years = 0. ; months = 0. ; weeks = 0. ; days  = 0. ;
          hours = 0. ; mins   = 0. ; secs  = 0. ; msecs = 0. }

    let add t1 t2 = { years  = t1.years  +. t2.years ;
                      months = t1.months +. t2.months ;
                      weeks  = t1.weeks  +. t2.weeks ;
                      days   = t1.days   +. t2.days ;
                      hours  = t1.hours  +. t2.hours ;
                      mins   = t1.mins   +. t2.mins ;
                      secs   = t1.secs   +. t2.secs ;
                      msecs  = t1.msecs  +. t2.msecs }

    let parzer ?(picky=false) =
        let open Peg in
        let part =
            Float.parzer ~picky:true ++ any blank ++ word >>=
            fun ((n,_),u) -> match String.lowercase u with
                | "y" | "year" | "years" ->  return { zero with years = n }
                | "month" | "months" ->      return { zero with months = n }
                | "w" | "week" | "weeks" ->  return { zero with weeks = n }
                | "d" | "day" | "days" ->    return { zero with days = n }
                | "h" | "hour" | "hours" ->  return { zero with hours = n }
                | "m" | "min" | "mins" ->    return { zero with mins = n }
                | "s" | "sec" | "secs" ->    return { zero with secs = n }
                | "ms" | "msec" | "msecs" -> return { zero with msecs = n }
                | _ -> fail in
        let rec p bs =
            (part ++ any blank ++ optional p >>: function
                | (i, _), None -> normalize i
                | (i, _), Some i' -> add i i' |> normalize) bs in
        if picky then p else
        either [ p ;
                 Float.parzer ~picky:true >>: fun secs -> normalize { zero with secs } ]

    (*$T parzer
      let open Datatype.Interval in parzer (String.to_list "1year -2 months") = \
        Peg.Res ({ zero with years = 1. ; months = -2. }, [])
     *)

    (*$>*)
end
module Interval : sig
    (* Trick to have the record fields known when opening Interval in addition
     * to Interval_base *)
    include DATATYPE with type t := Interval_base.t
    type t = Interval_base.t =
        { years : float ; months : float ;
          weeks : float ; days   : float ;
          hours : float ; mins   : float ;
          secs  : float ; msecs  : float }
    val zero    : t
    val to_ms   : t -> Int64.t
    val to_secs : t -> float
    val of_secs : float -> t
    val add     : t -> t -> t
    end =
struct
    include Interval_base
    include Datatype_of(Interval_base)

    let zero = Interval_base.zero

    (* for this we consider 'default' length to variable time units *)
    let to_ms_float t =   t.msecs
             +. 1_000. *. t.secs
            +. 60_000. *. t.mins
          +. 3600_000. *. t.hours
        +. 86_400_000. *. t.days
       +. 604_800_000. *. t.weeks
     +. 2_628_000_000. *. t.months
    +. 31_557_600_000. *. t.years

    let to_ms t   = to_ms_float t |> Int64.of_float
    let to_secs t = to_ms_float t /. 1_000.
    let of_secs secs = { zero with secs }
    let add = Interval_base.add
end

(**
  Timestamp
 *)

module Timestamp = struct
    (*$< Timestamp *)
    include UInteger64 (* for number of milliseconds *)
    let name = "timestamp"

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

    let min = min
    let max = max

    (* Helper to build a Timestamp.t from a user friendly string *)
    exception Invalid_date of (int * int * int * int * int * float)
    exception Invalid_unit of char

    let of_tm_nocheck y mo d h mi s =
        let open Unix in
        let s_f, s_i = modf s in
        let tm = { tm_sec = int_of_float s_i ;
                   tm_min = mi ; tm_hour = h ;
                   tm_mday = d ; tm_mon = mo-1 ;
                   tm_year = y - 1900 ;
                   (* rest is ignored by mktime *)
                   tm_wday = 0 ; tm_yday = 0 ; tm_isdst = false } in
        let ts, _ = mktime tm in
        of_unixfloat (ts +. s_f)

    let of_tm y mo d h mi s =
        if y > 2100 || y < 1970 || mo < 1 || mo > 12 ||
           d < 1 || d > 31 || h > 24 || mi > 60 || s > 60. then
            raise (Invalid_date (y, mo, d, h, mi, s)) ;
        of_tm_nocheck y mo d h mi s

    let to_tm t =
        Unix.localtime (to_unixfloat t)

    let add_interval t i =
        let open Unix in
        let open Interval in
        let tm = to_tm t in
        of_tm_nocheck (tm.tm_year + 1900 + (int_of_float i.years))
                      (tm.tm_mon + 1 + (int_of_float i.months))
                      (tm.tm_mday + (int_of_float i.days) + 7*(int_of_float i.weeks))
                      (tm.tm_hour + (int_of_float i.hours))
                      (tm.tm_min + (int_of_float i.mins))
                      (float_of_int tm.tm_sec +.
                       (milliseconds t |> Int64.to_float) *. 0.001 +.
                       i.secs +.
                       (i.msecs *. 0.001))

    let sub_to_interval (t1 : t) (t2 : t) =
        let ds = to_unixfloat t2 -. to_unixfloat t1 in
        Interval.of_secs ds

    let now () = of_unixfloat (Unix.time ())

    let of_datestring str =
        let open Unix in
        let of_tm'   y mo d h mi s r = of_tm y mo d h mi s, r in
        let to_min   y mo d h mi r   = of_tm y mo d h mi 0., r in
        let to_hour  y mo d h r      = of_tm y mo d h 0  0., r in
        let to_day   y mo d r        = of_tm y mo d 0 0  0., r in
        let to_month y mo r          = of_tm y mo 1 0 0  0., r in
        let today_full h mi s r =
            let tm = localtime (time ()) in
            of_tm (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday h mi s, r in
        let today_to_min  h mi r = today_full h mi 0. r in
        if String.starts_with str "now" then now (), String.lchop ~n:3 str else
        try Scanf.sscanf str "%4u-%2u-%2u %2u:%2u:%f%s@\n" of_tm'
        with _ -> try Scanf.sscanf str "%4u-%2u-%2u %2u:%2u%s@\n" to_min
        with _ -> try Scanf.sscanf str "%4u-%2u-%2u %2u%s@\n" to_hour
        with _ -> try Scanf.sscanf str "%4u-%2u-%2u%s@\n" to_day
        with _ -> try Scanf.sscanf str "%4u-%2u%s@\n" to_month
        with _ -> try Scanf.sscanf str "%2u:%2u:%f%s@\n" today_full
        with _ -> try Scanf.sscanf str "%2u:%2u%s@\n" today_to_min
        with _ -> Scanf.sscanf str "%f@\n" of_unixfloat, "" (* only valid if there is nothing left *)

    let to_string (t : t) =
        let open Unix in
        let string_of_secs v =
            if v = 0. then "" else
            let frac, intg = modf v in
            if abs_float frac > 0.0005 then
                Printf.sprintf ":%06.3f" v
            else
                Printf.sprintf ":%02.0f" intg in
        let tm = to_tm t in
        let s = (float_of_int tm.tm_sec) +. (milliseconds t |> Int64.to_float)*.0.001 in
        Printf.sprintf "%04u-%02u-%02u %02u:%02u%s"
            (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
            tm.tm_hour tm.tm_min (string_of_secs s)

    let write_txt oc t =
        let str = to_string t in
        Output.string oc str

    let parzer ?(picky=false) =
        let open Peg in
        let date =
            let sep = either [ item '/' ; item '-' ] in
            number_in_range 1970 2100 ++ sep ++ number_in_range 1 12 ++ sep ++ number_in_range 1 31 >>:
            function ((((y,_),m),_),d) -> y,m,d
        and time =
            let sep = either [ item ':' ; item '-' ] in
            let sec = Float.parzer >>= fun s -> if s < 0. || s > 60. then fail else return s in
            number_in_range 0 23 ++ sep ++ number_in_range 0 59 ++ optional (sep ++ sec) >>:
            function (((h,_),m),s) -> match s with
                | None       -> h,m,0.
                | Some (_,s) -> h,m,s in
        let abs =
            either [
                (* classical date format *)
                date ++ optional (several blank ++ time) >>:
                    (fun ((y,mo,d),h) -> match h with
                        | None              -> of_tm y mo d 0 0 0.
                        | Some (_,(h,mi,s)) -> of_tm y mo d h mi s) ;
                (* special *)
                istring "now" >>: (fun _ -> now ()) ]
        and junkie = (* junkie's format *)
            UInteger64.parzer ++ string "s " ++ Integer.parzer ++ string "us" >>:
            (fun (((s,_),us),_) ->
                Int64.add (Int64.mul s 1000L)
                          (Int64.of_int ((499 + us) / 1000)))
        and unix_ts = Float.parzer >>: fun ts -> Int64.of_float (1000.*.ts) in
        either ((abs ++ optional (any blank ++ Interval.parzer ~picky:true) >>:
                    (fun (ts,i_opt) -> match i_opt with
                        | None -> ts
                        | Some (_, i) -> add_interval ts i)) ::
                 junkie ::
                 if not picky then [unix_ts] else [])

    (*$T parzer
      parzer (String.to_list "now -56 days") <> Peg.Fail
      parzer (String.to_list "now") <> Peg.Fail
      parzer (String.to_list "1976-01-28 15:07") = Peg.Res (191686020000L, [])
      parzer (String.to_list "1976-01-28 15:07:30") = Peg.Res (191686050000L, [])
      parzer (String.to_list "1976-01-28 15:07 +30secs") = Peg.Res (191686050000L, [])
      parzer (String.to_list "1323766040s 992799us") = Peg.Res (1323766040993L, [])
     *)

    (* copied from Datatype_of *)
    let of_string str : t =
        let open Peg in
        match (parzer ++ eof) (String.to_list str) with
        | Res ((v,_), _) -> v
        | Fail -> raise Parse_error

    (*$>*)
end

let round_timestamp ?(ceil=false) n t =
    let r = Int64.rem t n in
    let t' = Int64.sub t r in
    if ceil && r <> 0L then Int64.add t' n else t'

let round_time_interval n start stop =
    let start = round_timestamp n start
    and stop = round_timestamp ~ceil:true n stop in
    if stop <> start then start, stop else start, Int64.add start n


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
        for _i = 1 to len do
            res := (T.read ic) :: !res
        done ;
        !res

    let to_imm t =
        "["^ (List.map T.to_imm t |> String.concat ";") ^"]"

    let parzer ?(picky=false) =
        ignore picky ;
        let open Peg in
        item '[' ++ several T.parzer ++ item ']' >>:
        fun ((_,l),_) -> l
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
    let to_imm = T.to_imm
    let parzer = T.parzer
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
    let to_imm = function
        | V1of2 a -> "(Datatype.Altern1.V1of2 "^ T1.to_imm a ^")"
        | V2of2 a -> "(Datatype.Altern1.V2of2 "^ T2.to_imm a ^")"
    let parzer ?(picky=false) =
        ignore picky ;
        let open Peg in
        either [ string "v1:" ++ T1.parzer >>: (fun (_,v) -> V1of2 v) ;
                 string "v2:" ++ T2.parzer >>: (fun (_,v) -> V2of2 v) ]
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
    let to_imm = function None -> "None" | Some x -> "(Some "^ T.to_imm x ^")"
    let parzer ?(picky=false) =
        ignore picky ;
        let open Peg in
        either [ none (istring "none") ;
                 some (istring "some " ++ T.parzer >>: snd) ]
end
module Option (T:DATATYPE) :
    DATATYPE with type t = T.t option =
struct
    include Option_base (T)
    include Datatype_of(Option_base (T))
end

