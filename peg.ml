open Batteries
let (|?) = Option.(|?)

(* All parsers are stateless *)

type ('a, 'b) parzer_result = Res of 'a * 'b list | Fail
type ('a, 'b) parzer = 'b list (* tokens to add *) -> ('a, 'b) parzer_result

exception Parse_error   (* used here and there *)

(* Matchs the end of input *)
let eof = function
    | [] -> Res ((), [])
    | _ -> Fail

let all bs = Res (bs, [])

(* Fail if p fail, but consume nothing it p match *)
let check p bs =
    match p bs with
    | Fail -> Fail
    | Res (x, _) -> Res (x, bs)

let upto delim_orig bs =
    let rec aux past_delim delim past_bs bs =
        match delim, bs with
        | [], _ -> Res (List.rev past_bs, bs)
        | _, [] -> Fail
        | d::delim', b::bs' ->
            if d = b then aux (d::past_delim) delim' (b::past_bs) bs' else
            (match past_delim with
            | [] ->
                aux [] delim_orig (b::past_bs) bs'
            | [_] -> (* stay in place *)
                aux [] delim_orig past_bs bs
            | _::p' -> (* rollback all chars but one *)
                let rb = List.length p' in
                let rbs, past_bs' = List.split_at rb past_bs in
                aux [] delim_orig past_bs' (List.rev_append rbs bs)) in
    aux [] delim_orig [] bs

(*$T upto
  upto [0;0] [0;0] = Res ([0;0], [])
  upto [0;0] [0] = Fail
  upto [0;0] [] = Fail
  upto [0;0] [1;0] = Fail
  upto [0;0] [1] = Fail
  upto [0;0] [1;2;3;0;0] = Res ([1;2;3;0;0], [])
  upto [0;0] [1;2;3;0;0;4;5] = Res ([1;2;3;0;0], [4;5])
  upto [0;0] [1;2;0;3;0;0;4;5] = Res ([1;2;0;3;0;0], [4;5])
 *)

let cond c = function
    | b::bs when c b -> Res (b, bs)
    | _ -> Fail

let item i = cond ((=) i)

(*$T item
  item 1 [1;1;2;2] = Res (1, [1;2;2])
 *)

(* like item but restricted to string values, and the first element is required to match a regex
   and the returned value is the list of matching substrings *)
let regex re_str bs =
    let re = Str.regexp re_str in
    let str = String.of_list bs in
    if Str.string_match re str 0 then (
        let rec aux prev i =
            try aux ((Str.matched_group i str)::prev) (i+1) with
                | Not_found -> aux (""::prev) (i+1)
                | Invalid_argument _ -> prev in
        Res (List.rev (aux [] 0), [])
    ) else (
        Fail
    )

let take n bs =
    try let res, rem = List.split_at n bs in
        Res (res, rem)
    with Invalid_argument _ -> Fail

(*$T take
  take 2 [1;2;3;4;5] = Res ([1;2], [3;4;5])
  take 3 [1;2;3] = Res ([1;2;3], [])
  take 3 [1;2] = Fail
*)

(* Change value returned by p through f *)
let map p f bs =
    match p bs with
    | Fail -> Fail
    | Res (res, rem) ->
        Res (f res, rem)

let (>>:) = map

let map_filter p f bs =
    match p bs with
    | Fail -> Fail
    | Res (res, rem) -> (match f res with
        | None -> Fail
        | Some x -> Res (x, rem))

let some p = p >>: (fun res -> Some res)
let none p = p >>: (fun _ -> None)
let ign p = p >>: ignore

(* combinators *)

(* sequence of parsers *)
let seq ps bs =
    let rec aux res ps bs = match ps with
        | [] -> Res (List.rev res, bs)
        | p::ps' ->
            (match p bs with
            | Res (res', bs') ->
                aux (res'::res) ps' bs'
            | Fail -> Fail) in
    aux [] ps bs

(*$T seq
  seq [ item 1 ; item 2 ; item 3 ] [1;2;3;4;5] = Res ([1;2;3], [4;5])
 *)

(* Many times, some values are not interresting.
   This version of seq takes parsers that return an optional value, and filter them *)
let seqf ps =
    seq ps >>: List.filter_map identity

(* If you have parsers of different types you may want to cons them: *)
let cons p1 p2 bs =
    match p1 bs with
    | Fail -> Fail
    | Res (res1, rem1) ->
        (match p2 rem1 with
        | Fail -> Fail
        | Res (res2, rem2) ->
            Res ((res1,res2), rem2))

let (++) = cons

(* alternative function that takes a list of parsers and return the first that returns a value *)
let rec either ps bs =
    match ps with
    | p::ps' ->
        (match p bs with
        | (Res _) as x -> x
        | Fail -> either ps' bs)
    | [] -> Fail

(*$T either
  either [item 1; item 2] [1;5] = Res (1, [5])
  either [item 1; item 2] [2;5] = Res (2, [5])
  either [item 1; item 2] [3;1] = Fail
 *)

(* Special seq when you wait for a list of chars: *)
let string s =
    seq (String.to_list s |>
         List.map (fun c -> item c))
let istring s =
    seq (String.to_list s |>
         List.map (fun c ->
            either [ item (Char.uppercase c) ;
                     item (Char.lowercase c) ]))

(* Note that a trailing separator will be swallowed *)
let repeat ?min ?max ?sep p bs =
    if max = Some 0 then Res ([], bs) else
    let out nb_match res bs =
        (match min with
        | None ->
            Res (List.rev res, bs)
        | Some mi when mi <= nb_match ->
            Res (List.rev res, bs)
        | _ -> Fail) in
    let rec aux nb_match res bs =
        match p bs with
        | Res (res', rem) ->
            let nb_match' = succ nb_match in
            if max = Some nb_match' then (
                out nb_match' (res'::res) rem
            ) else (
                match sep with
                | None ->
                    aux nb_match' (res'::res) rem
                | Some sep ->
                    (match sep rem with
                    | Fail ->
                        out nb_match (res'::res) rem
                    | Res (_, rem') ->
                        aux nb_match' (res'::res) rem'))
        | Fail ->
            out nb_match res bs in
    aux 0 [] bs

(*$T repeat
  repeat ~min:2 ~max:4 (item 1) [] = Fail
  repeat ~min:2 ~max:4 (item 1) [1] = Fail
  repeat ~min:2 ~max:4 (item 1) [1;1] = Res ([1;1], [])
  repeat ~min:2 ~max:4 (item 1) [1;1;2] = Res ([1;1], [2])
  repeat ~min:2 ~max:4 (item 1) [1;1;1;2] = Res ([1;1;1], [2])
  repeat ~min:2 ~max:4 (item 1) [1;1;1;1;2] = Res ([1;1;1;1], [2])
  repeat ~min:2 ~max:4 (item 1) [1;1;1;1;1;2] = Res ([1;1;1;1], [1;2])
  repeat ~sep:(item '.') (item 'a') (String.to_list "a.a.a") = Res (['a';'a';'a'],[])
  repeat ~sep:(item '.') (item 'a') (String.to_list "a") = Res (['a'],[])
 *)

let any ?sep p bs = repeat ?sep p bs
let several ?sep p bs = repeat ~min:1 ?sep p bs
let times n ?sep p bs = repeat ~min:n ~max:n ?sep p bs

(* returns either None or Some value *)
let optional p =
    repeat ~max:1 p >>: (function
        | []  -> None
        | [v] -> Some v
        | _   -> assert false)
let repeat_until f p bs =
    let rec aux res bs =
        match p bs with
        | Fail -> Fail
        | Res (res', rem) ->
            if f res' then (
                Res (res' :: List.rev res, rem)
            ) else (
                aux (res'::res) rem
            ) in
    aux [] bs

(* Run parser p until a result is obtained, then give the result to f that will return a new parser.
   Once this new parser got it's result, give new items to first parser and so on *)
let bind p f bs =
    match p bs with
    | Fail -> Fail
    | Res (res, rem) -> (f res) rem

let (>>=) = bind (* as usual *)

(*$T bind
    let positive = cond (fun n -> n >= 0) in \
    let p = bind positive (fun i -> \
        assert (i >= 0) ; \
        (* match a sequence of i zeros *) \
        times i (item 0)) in \
    p [1;0;5] = Res ([0],[5]) && \
    p [1;0;0] = Res ([0],[0]) && \
    p [3;0;0;0;5] = Res ([0;0;0],[5]) && \
    p [3;0;0;5] = Fail
*)

(* unconditionally returns something - usefull for bind *)
let return res bs = Res (res, bs)
(* fails unconditionally *)
let fail _ = Fail

(*$T return
  return `glop [ 1; 2 ] = Res (`glop, [1; 2])
 *)

(* Use the results of the first parser as the input elements of the second.
   Return the results of p2.
   Notice that if p1 is a ('a, 'b) parzer and p2 a ('c, 'a) parzer,
   then pipe p1 p2 is a ('c, 'b) parzer *)
let pipe p1 p2 bs =
    let rec aux prev1 bs = (* prev1 is the list of previous p1 results *)
        match p1 bs with
        | Fail -> Fail
        | Res (res, rem) ->
            let bs' = res :: List.rev prev1 in
            (match p2 bs' with
            | Fail -> (* more luck with more tokens maybe? *)
                aux (res::prev1) rem
            | Res (res', []) ->
                Res (res', rem)
            | _ -> (* should consume everything since we feed it one token at a time *)
                assert false) in
    aux [] bs

(*$T pipe
  (* notice that p1 could parse [5;0] but since p2 can't then [5;0] is left unparsed *) \
  pipe (upto [0]) (item [2;1;0]) [2;1;0;5;0;42] = Res ([2;1;0],[5;0;42])
  pipe (upto [0]) (item [2;1;0]) [5;0;42] = Fail
 *)

(* Various useful parsers *)

let blank = cond Char.is_whitespace
let alphabetic = cond Char.is_letter
let numeric = cond Char.is_digit
let alphanum = either [ alphabetic ; numeric ]

let iitem c = either [ item c ; item (Char.uppercase c) ]
let char_seq str =
    seq (List.map (fun c ->
        if c >= 'a' && c <= 'z' then iitem c else item c)
        (String.to_list str))

let crlf = seq [ item '\r' ; item '\n' ]

let word = several alphabetic >>: String.of_list

let c2i c =
    if c >= '0' && c <= '9' then
        int_of_char c - int_of_char '0'
    else if c >= 'a' && c <= 'z' then
        int_of_char c - int_of_char 'a' + 10
    else if c >= 'A' && c <= 'Z' then
        int_of_char c - int_of_char 'A' + 10
    else (
        Log.error "Cannot convert char '%c' to int" c ;
        assert false
    )

(*$= c2i & ~printer:String.of_int
    5 (c2i '5')
    15 (c2i 'f')
    15 (c2i 'F')
*)

let i2c i =
    if i >= 0 && i <= 9 then char_of_int (int_of_char '0' + i)
    else if i >= 10 && i <= 35 then char_of_int (int_of_char 'a' + i - 10)
    else (
        Log.error "Cannot convert int '%d' to digit" i ;
        assert false
    )

(*$= i2c & ~printer:String.of_char
    '5' (i2c 5)
    'f' (i2c 15)
*)

let digit base =
    map (cond (fun c ->
            (c >= '0' && c <= '9' && c < (char_of_int (int_of_char '0' + base))) ||
            (base > 10 && (
                (c >= 'a' && c < (char_of_int (int_of_char 'a' + (base - 10)))) ||
                (c >= 'A' && c < (char_of_int (int_of_char 'A' + (base - 10))))
            )))) c2i

module Number (N : BatNumber.NUMERIC_BASE) =
struct
    let number base =
        let base' = N.of_int base in
        map (several (digit base)) (fun ds ->
            let rec aux n = function
                | [] -> n
                | d :: d' ->
                    aux (N.add (N.of_int d) (N.mul n base')) d' in
            aux N.zero ds)

    let binary_number = number 2
    let octal_number = number 8
    let decimal_number = number 10
    let hexadecimal_number = number 16

    let c_like_number_prefix c =
        seq [ item '0' ; either [ item (Char.lowercase c) ; item (Char.uppercase c) ] ]

    let c_like_hex_number =
        seqf [ none (c_like_number_prefix 'x') ; some hexadecimal_number ] >>: List.hd

    let c_like_octal_number =
        seqf [ none (c_like_number_prefix 'o') ; some octal_number ] >>: List.hd

    let c_like_binary_number =
        seqf [ none (c_like_number_prefix 'b') ; some binary_number ] >>: List.hd

    let c_like_number =
        either [ c_like_hex_number ;
                 c_like_octal_number ;
                 c_like_binary_number ;
                 decimal_number ]

    let in_range min max n =
        if N.compare n min >= 0 && N.compare n max <= 0 then return n else fail

    let number_in_range min max = decimal_number >>= in_range min max

end

include Number (Int)

module Number32 = Number (Int32)
module Number64 = Number (Int64)

(*$T c_like_number
  c_like_number ['1';'2';'3'] = Res (123,[])
  c_like_number ['0';'x';'1';'2'] = Res (18,[])
  c_like_number ['0';'b';'1';'0'] = Res (2,[])
  c_like_number ['0';'x'] = Res (0, ['x'])
*)

