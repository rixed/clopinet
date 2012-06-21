open Bricabrac

(*
   DATATYPES
   What's a DATATYPE? A name, a compact (binary) and friendly (textual) writer and
   reader, an equality and a hash function.
 *)

module type DATATYPE =
sig
    type t

    val name      : string
    val equal     : t -> t -> bool
    val hash      : t -> int
    val write     : out_channel -> t -> unit
    val write_txt : out_channel -> t -> unit
    val read      : in_channel -> t
    val read_txt  : TxtInput.t -> t
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
    output_string oc (string_of_list l)

let write_var_int64 oc n =
    (* Recode negative number so that higher bits are 0 : stores (abs(n)<<1)+sign.
     * Notice that if n > 0 then we can mult it by 2 since we have a 0 bit at highest
     * position, but then ocaml will think it's negative again, so we have to
     * consider than n<0 means n is actually huge. *)
    let n = if n >= 0L then Int64.shift_left n 1
            else Int64.add 1L (Int64.shift_left (Int64.neg n) 1) in
    let rec aux n =
        if n >= 0L && n < 128L then output_byte oc (Int64.to_int n)
        else (
            output_byte oc ((Int64.to_int n) lor 128) ;
            aux (Int64.shift_right_logical n 7)
        ) in
    aux n

let write_var_int oc n =
    write_var_int64 oc (Int64.of_int n)

let rec read_var_int64 ic =
    let rec aux n dec =
        let b = input_byte ic in
        let n = Int64.logor n (Int64.shift_left (Int64.of_int (b land 127)) dec) in
        if b < 128 then n else aux n (dec+7) in
    let n = aux 0L 0 in
    (* bit 0 is actually the sign bit *)
    (if Int64.to_int n land 1 = 0 then id else Int64.neg)
        (Int64.shift_right_logical n 1)

let read_var_int ic =
    Int64.to_int (read_var_int64 ic)

let read_txt_until ic delim =
    let rec aux p =
        let b = TxtInput.peek ic in
        if String.contains delim b then (
            string_of_list (List.rev p)
        ) else (
            TxtInput.swallow ic ;
            aux (b :: p)
        ) in
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
struct
    type t = bool
    let equal = (=)
    let hash = function true -> 1 | false -> 0
    let name = "bool"
    let write oc t = output_byte oc (if t then 1 else 0)
    let write_txt oc t = output_char oc (if t then 't' else 'f')
    let read ic = input_byte ic = 1
    let read_txt ic = TxtInput.read ic = 't'
end

module Void : DATATYPE with type t = unit =
struct
    type t = unit
    let equal () () = true
    let hash () = 0
    let name = "void"
    let write _ _ = ()
    let write_txt _ _ = ()
    let read _ = ()
    let read_txt _ = ()
end

module Text : DATATYPE with type t = string =
struct
    type t = string
    let name = "string"
    let equal = (=)
    let hash = Hashtbl.hash
    let write oc t =
        write_var_int oc (String.length t) ;
        output_string oc t
    let write_txt = output_string
    let read ic =
        let len = read_var_int ic in
        let str = String.create len in
        really_input ic str 0 len ;
        str
    let read_txt ic =
        read_txt_until ic "\t\n"
end

module Float : NUMBER with type t = float =
struct
    type t = float
    let equal = (=)
    let hash = Hashtbl.hash
    let name = "float"
    let write oc f =
        Int64.bits_of_float f |> write_var_int64 oc
    let write_txt oc f =
        let s = string_of_float f in
        output_string oc s
    let read ic =
        read_var_int64 ic |> Int64.float_of_bits
    let read_txt ic =
        (* hello, I'm slow! *)
        Text.read_txt ic |> float_of_string
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
    type t = int
    let name = "int"
    let equal = (=)
    let hash = id
    let write = write_var_int
    let write_txt oc i =
        if i = 0 then output_char oc '0' else
        let rec aux l p i =
            if i = 0 then write_char_list oc p else
            aux (1+l) (Char.unsafe_chr ((i mod 10) + Char.code '0') :: p) (i/10) in
        aux 0 [] i
    let read = read_var_int
    let read_txt ic =
        let neg = peek_sign ic in
        let rec aux v =
            let d = TxtInput.peek ic in
            if d < '0' || d > '9' then v else (
                TxtInput.swallow ic ;
                aux (v*10 + (int_of_char d - Char.code '0'))
            ) in
        (if neg then (~-) else id) (aux 0)
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
    let write = output_byte
    let read = input_byte
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
        output_byte oc t ;
        output_byte oc (t lsr 8)
    let read ic =
        let fst_b = input_byte ic in
        let snd_b = input_byte ic in
        fst_b + (snd_b lsl 8)
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
    type t = Int32.t
    let name = "int32"
    let equal = (=)
    let hash = Int32.to_int
    let write oc t = write_var_int64 oc (Int64.of_int32 t)
    let write_txt oc i = Printf.fprintf oc "%ld" i
    let read ic = Int64.to_int32 (read_var_int64 ic)
    let read_txt ic =
        let neg = peek_sign ic in
        let rec aux v =
            let d = TxtInput.peek ic in
            if d < '0' || d > '9' then v else (
                TxtInput.swallow ic ;
                aux (Int32.add (Int32.mul v 10l) (Int32.of_int (int_of_char d - Char.code '0')))
            ) in
        (if neg then Int32.neg else id) (aux 0l)
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
    type t = Int64.t
    let name = "int64"
    let equal = (=)
    let hash = Int64.to_int
    let write = write_var_int64
    let write_txt oc i = Printf.fprintf oc "%Ld" i
    let read  = read_var_int64
    let read_txt ic =
        let neg = peek_sign ic in
        let rec aux v =
            let d = TxtInput.peek ic in
            if d < '0' || d > '9' then v else (
                TxtInput.swallow ic ;
                aux (Int64.add (Int64.mul v 10L) (Int64.of_int (int_of_char d - Char.code '0')))
            ) in
        (if neg then Int64.neg else id) (aux 0L)
    let zero = 0L
    let add = Int64.add
    let sub = Int64.sub
    let idiv t i = Int64.div t (Int64.of_int i)
    let imul t i = Int64.mul t (Int64.of_int i)
    let mul = Int64.mul
    let lt = (<)
end

module InetAddr : DATATYPE with type t = Unix.inet_addr =
struct
    type t = Unix.inet_addr
    let name = "inetAddr"
    let equal = (=)
    let hash = Hashtbl.hash
    let write oc t =
        let (str : string) = Obj.magic t in (* underlying repr of a inet_addr is a string for some reason, probably to handle both v4 and v6 *)
        Text.write oc str
    let write_txt oc t =
        Unix.string_of_inet_addr t |>
        output_string oc
    let read ic =
        let str = Text.read ic in
        Obj.magic str
    let read_txt ic =
        Text.read_txt ic |>
        Unix.inet_addr_of_string
end

module Cidr : DATATYPE with type t = InetAddr.t * Integer16.t =
struct
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
        output_char oc '/' ;
        Integer16.write_txt oc m
    let read ic =
        let n = InetAddr.read ic in
        n, Integer16.read ic
    let read_txt ic =
        let n = read_txt_until ic "/" |>
                Unix.inet_addr_of_string in
        ignore (TxtInput.read ic) ;
        n, Integer16.read_txt ic
end

(* Usefull function to degrade an InetAddr into a subnet.
  Returns IP*mask *)
let cidr_of_inetaddr subnets ip =
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
        String.length n = String.length a && addr_match 0 in
    let masklen_of (t : Unix.inet_addr) =
        let s = Obj.magic t in String.length s * 8 in
    let net, mask =
        try List.find (in_cidr ip) subnets
        with Not_found -> ip, masklen_of ip in
    net, mask

module Timestamp : NUMBER with type t = Int64.t * int =
struct
    type t = Int64.t * int (* secs, usecs *)
    let name = "timestamp"
    let equal = (=)
    let hash (a, b) = Int64.to_int a + b
    let write oc (s,u) =
        write_var_int64 oc s ;
        write_var_int oc u
    let write_txt oc (s,u) =
        Integer64.write_txt oc s ;
        output_string oc "s " ;
        Integer.write_txt oc u ;
        output_string oc "us"
    let read ic =
        let s = read_var_int64 ic in
        let u = read_var_int ic in
        s, u
    let read_txt ic =
        let s = Integer64.read_txt ic in
        let c = TxtInput.read ic in assert (c = 's') ;
        let c = TxtInput.read ic in assert (c = ' ') ;
        let u = Integer.read_txt ic in
        let c = TxtInput.read ic in assert (c = 'u') ;
        let c = TxtInput.read ic in assert (c = 's') ;
        s, u

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
let round_timestamp n (sec, _usec) =
    let n = Int64.of_int n in
    Int64.mul n (Int64.div sec n)

(*
   Functors to easily assemble more complex types
 *)

module ListOf (T : DATATYPE) :
    DATATYPE with type t = T.t list =
struct
    type t = T.t list

    let rec equal a b = match a, b with
        | [], x -> x = []
        | a::a', b::b' -> T.equal a b && equal a' b'
        | _ -> false

    let hash = Hashtbl.hash (* Hum no! should use the T.hash function on the first elements *)

    let name = "(listof " ^ T.name ^ ")"

    let write oc t =
        write_var_int oc (List.length t) ;
        List.iter (T.write oc) t

    let write_txt oc t =
        output_char oc '[' ;
        let rec aux = function
        | []   -> () (* it's stupid to search for this at each iteration, but the compiler warns if [] is unmatched *)
        | [x]  -> T.write_txt oc x
        | x::l -> T.write_txt oc x ;
                  output_char oc ';' ;
                  aux l in
        aux t ;
        output_char oc ']'

    let read ic =
        let len = read_var_int ic in
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

module Tuple2 (T1 : DATATYPE) (T2 : DATATYPE) :
    DATATYPE with type t = T1.t * T2.t =
struct
    type t = T1.t * T2.t

    let equal (a1,a2) (b1,b2) =
        T1.equal a1 b1 && T2.equal a2 b2

    let hash = Hashtbl.hash

    let name = T1.name ^ "*" ^ T2.name

    let write oc (t1, t2) =
        T1.write oc t1 ;
        T2.write oc t2

    let write_txt oc (t1, t2) =
        T1.write_txt oc t1 ;
        output_char oc '\t' ;
        T2.write_txt oc t2

    let read ic =
        (* beware that reads must be performed in order! *)
        let fst = T1.read ic in
        let snd = T2.read ic in
        fst, snd

    let read_txt ic =
        let v1 = T1.read_txt ic in
        let sep = TxtInput.read ic in
        assert (sep = '\t') ;
        let v2 = T2.read_txt ic in
        v1, v2
end

module Tuple3 (T1:DATATYPE) (T2:DATATYPE) (T3:DATATYPE) :
    DATATYPE with type t = T1.t * T2.t * T3.t =
struct
    type t = T1.t * T2.t * T3.t
    let equal (a1,a2,a3) (b1,b2,b3) =
        T1.equal a1 b1 &&
        T2.equal a2 b2 &&
        T3.equal a3 b3
    let hash = Hashtbl.hash
    let name = T1.name^"*"^T2.name^"*"^T3.name
    let write oc (t1,t2,t3) =
        T1.write oc t1 ; T2.write oc t2 ;
        T3.write oc t3
    let write_txt oc (t1,t2,t3) =
        T1.write_txt oc t1 ; output_char oc '\t' ;
        T2.write_txt oc t2 ; output_char oc '\t' ;
        T3.write_txt oc t3
    let read ic =
        let t1 = T1.read ic in
        let t2 = T2.read ic in
        let t3 = T3.read ic in
        t1, t2, t3
    let read_txt ic =
        let t1 = T1.read_txt ic in
        let sep = TxtInput.read ic in assert (sep = '\t') ;
        let t2 = T2.read_txt ic in
        let sep = TxtInput.read ic in assert (sep = '\t') ;
        let t3 = T3.read_txt ic in
        t1,t2,t3
end

module Tuple4 (T1:DATATYPE) (T2:DATATYPE) (T3:DATATYPE) (T4:DATATYPE) :
    DATATYPE with type t = T1.t * T2.t * T3.t * T4.t =
struct
    type t = T1.t * T2.t * T3.t * T4.t
    let equal (a1,a2,a3,a4) (b1,b2,b3,b4) =
        T1.equal a1 b1 &&
        T2.equal a2 b2 &&
        T3.equal a3 b3 &&
        T4.equal a4 b4
    let hash = Hashtbl.hash
    let name = T1.name^"*"^T2.name^"*"^T3.name^"*"^T4.name
    let write oc (t1,t2,t3,t4) =
        T1.write oc t1 ; T2.write oc t2 ;
        T3.write oc t3 ; T4.write oc t4
    let write_txt oc (t1,t2,t3,t4) =
        T1.write_txt oc t1 ; output_char oc '\t' ;
        T2.write_txt oc t2 ; output_char oc '\t' ;
        T3.write_txt oc t3 ; output_char oc '\t' ;
        T4.write_txt oc t4
    let read ic =
        let t1 = T1.read ic in
        let t2 = T2.read ic in
        let t3 = T3.read ic in
        let t4 = T4.read ic in
        t1, t2, t3, t4
    let read_txt ic =
        let t1 = T1.read_txt ic in
        let sep = TxtInput.read ic in assert (sep = '\t') ;
        let t2 = T2.read_txt ic in
        let sep = TxtInput.read ic in assert (sep = '\t') ;
        let t3 = T3.read_txt ic in
        let sep = TxtInput.read ic in assert (sep = '\t') ;
        t1,t2,t3,T4.read_txt ic
end

module Tuple5 (T1:DATATYPE) (T2:DATATYPE) (T3:DATATYPE) (T4:DATATYPE) (T5:DATATYPE) :
    DATATYPE with type t = T1.t * T2.t * T3.t * T4.t * T5.t =
struct
    type t = T1.t * T2.t * T3.t * T4.t * T5.t
    let equal (a1,a2,a3,a4,a5) (b1,b2,b3,b4,b5) =
        T1.equal a1 b1 &&
        T2.equal a2 b2 &&
        T3.equal a3 b3 &&
        T4.equal a4 b4 &&
        T5.equal a5 b5
    let hash = Hashtbl.hash
    let name = T1.name^"*"^T2.name^"*"^T3.name^"*"^T4.name^"*"^T5.name
    let write oc (t1,t2,t3,t4,t5) =
        T1.write oc t1 ; T2.write oc t2 ;
        T3.write oc t3 ; T4.write oc t4 ;
        T5.write oc t5
    let write_txt oc (t1,t2,t3,t4,t5) =
        T1.write_txt oc t1 ; output_char oc '\t' ;
        T2.write_txt oc t2 ; output_char oc '\t' ;
        T3.write_txt oc t3 ; output_char oc '\t' ;
        T4.write_txt oc t4 ; output_char oc '\t' ;
        T5.write_txt oc t5
    let read ic =
        let t1 = T1.read ic in
        let t2 = T2.read ic in
        let t3 = T3.read ic in
        let t4 = T4.read ic in
        let t5 = T5.read ic in
        t1, t2, t3, t4, t5
    let read_txt ic =
        let t1 = T1.read_txt ic in
        let sep = TxtInput.read ic in assert (sep = '\t') ;
        let t2 = T2.read_txt ic in
        let sep = TxtInput.read ic in assert (sep = '\t') ;
        let t3 = T3.read_txt ic in
        let sep = TxtInput.read ic in assert (sep = '\t') ;
        let t4 = T4.read_txt ic in
        let sep = TxtInput.read ic in assert (sep = '\t') ;
        let t5 = T5.read_txt ic in
        t1,t2,t3,t4,t5
end

module Tuple6 (T1:DATATYPE) (T2:DATATYPE) (T3:DATATYPE) (T4:DATATYPE) (T5:DATATYPE) (T6:DATATYPE) :
    DATATYPE with type t = T1.t * T2.t * T3.t * T4.t * T5.t * T6.t =
struct
    type t = T1.t * T2.t * T3.t * T4.t * T5.t * T6.t
    let equal (a1,a2,a3,a4,a5,a6) (b1,b2,b3,b4,b5,b6) =
        T1.equal a1 b1 &&
        T2.equal a2 b2 &&
        T3.equal a3 b3 &&
        T4.equal a4 b4 &&
        T5.equal a5 b5 &&
        T6.equal a6 b6
    let hash = Hashtbl.hash
    let name = T1.name^"*"^T2.name^"*"^T3.name^"*"^T4.name^"*"^T5.name^"*"^T6.name
    let write oc (t1,t2,t3,t4,t5,t6) =
        T1.write oc t1 ; T2.write oc t2 ;
        T3.write oc t3 ; T4.write oc t4 ;
        T5.write oc t5 ; T6.write oc t6
    let write_txt oc (t1,t2,t3,t4,t5,t6) =
        T1.write_txt oc t1 ; output_char oc '\t' ;
        T2.write_txt oc t2 ; output_char oc '\t' ;
        T3.write_txt oc t3 ; output_char oc '\t' ;
        T4.write_txt oc t4 ; output_char oc '\t' ;
        T5.write_txt oc t5 ; output_char oc '\t' ;
        T6.write_txt oc t6
    let read ic =
        let t1 = T1.read ic in
        let t2 = T2.read ic in
        let t3 = T3.read ic in
        let t4 = T4.read ic in
        let t5 = T5.read ic in
        let t6 = T6.read ic in
        t1, t2, t3, t4, t5, t6
    let read_txt ic =
        let t1 = T1.read_txt ic in
        let sep = TxtInput.read ic in assert (sep = '\t') ;
        let t2 = T2.read_txt ic in
        let sep = TxtInput.read ic in assert (sep = '\t') ;
        let t3 = T3.read_txt ic in
        let sep = TxtInput.read ic in assert (sep = '\t') ;
        let t4 = T4.read_txt ic in
        let sep = TxtInput.read ic in assert (sep = '\t') ;
        let t5 = T5.read_txt ic in
        let sep = TxtInput.read ic in assert (sep = '\t') ;
        t1,t2,t3,t4,t5,T6.read_txt ic
end

module Altern1 (T1:DATATYPE) :
    DATATYPE with type t = T1.t =
struct
    type t = T1.t
    let equal = T1.equal
    let hash = T1.hash
    let name = "V1of1 of "^T1.name
    let write oc t1 =
        output_byte oc 0 ; (* So that we will be able to decode it later when we add version *)
        T1.write oc t1
    let write_txt = T1.write_txt
    let read ic =
        let v = input_byte ic in
        if v <> 0 then Printf.fprintf stderr "bad version: %d\n%!" v ;
        assert (v = 0) ;
        T1.read ic
    let read_txt = T1.read_txt
end

type ('a, 'b) versions_2 = V1of2 of 'a | V2of2 of 'b
module Altern2 (T1:DATATYPE) (T2:DATATYPE) :
    DATATYPE with type t = (T1.t, T2.t) versions_2 =
struct
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
        | V1of2 a -> output_byte oc 0 ; T1.write oc a
        | V2of2 a -> output_byte oc 1 ; T2.write oc a
    let write_txt oc = function
        | V1of2 a -> output_string oc "v1:" ; T1.write_txt oc a
        | V2of2 a -> output_string oc "v2:" ; T2.write_txt oc a
    let read ic = match input_byte ic with
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

(*
   This one is usefull to store the "distribution" of a NUMBER, ie its
   count, min, max, avg and std deviation.
   See Aggregator.distr for an aggregation function.
 *)

module Distribution (N : NUMBER) =
struct
    include Tuple5 (Integer) (N) (N) (N) (N)

    (* Compute the avg and standard deviation using the well known recurence formulas (where
     * the standard deviation sigma = sqrt(s/(n-1)) *)
    let distr x = function
        | None -> 1, x, x, x, N.zero
        | Some (n, mi, ma, avg, s) ->
            let n' = n + 1 in
            let xd = N.sub x avg in
            let avg' = N.add avg (N.idiv xd n') in
            n',
            (if N.lt x mi then x else mi),
            (if N.lt ma x then x else ma),
            avg',
            (N.add s (N.mul xd (N.sub x avg')))
end

