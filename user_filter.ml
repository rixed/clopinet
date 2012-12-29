(** We could let the user enter straigh ocaml code, but she would
   not get very usefull error messages, and from a security perspective
   it would not be acceptable. *)
open Batteries
open ParserCo
open CharParser
open Peg

(** {2 Parsing of user filters} *)

(** User filters: expressions that we must parse into an AST, type, and then
    convert into an ML string boolean expression. *)

type value = Cidr of Datatype.Cidr.t
           | InetAddr of Datatype.InetAddr.t
           | Bool of Datatype.Bool.t
           | String of Datatype.Text.t
           | Integer of Datatype.Integer.t
           | Float of Datatype.Float.t
           | EthAddr of Datatype.EthAddr.t
           | Interval of Datatype.Interval.t
           | Timestamp of Datatype.Timestamp.t

and expr = Eq of expr * expr
         | Not of expr
         | Or of expr * expr
         | And of expr * expr
         | Gt of expr * expr
         | Lt of expr * expr
         | Ge of expr * expr
         | Le of expr * expr
         | StartsWith of expr * expr
         | Contains of expr * expr
         | ToFloat of expr (* added to promote the int into a float on operations that require it *)
         | Value of value
         | Field of string

let string_of_value = function
    | Cidr v      -> Datatype.Cidr.to_string v
    | InetAddr v  -> Datatype.InetAddr.to_string v
    | Bool v      -> Datatype.Bool.to_string v
    | String v    -> Datatype.Text.to_string v
    | Integer v   -> Datatype.Integer.to_string v
    | Float v     -> Datatype.Float.to_string v
    | EthAddr v   -> Datatype.EthAddr.to_string v
    | Interval v  -> Datatype.Interval.to_string v
    | Timestamp v -> Datatype.Timestamp.to_string v

let rec string_of_expr = function
    | Not e -> Printf.sprintf "! (%s)" (string_of_expr e)
    | Eq (e1, e2) -> Printf.sprintf "(%s) == (%s)" (string_of_expr e1) (string_of_expr e2)
    | Or (e1, e2) -> Printf.sprintf "(%s) || (%s)" (string_of_expr e1) (string_of_expr e2)
    | And (e1, e2) -> Printf.sprintf "(%s) && (%s)" (string_of_expr e1) (string_of_expr e2)
    | Gt (e1, e2) -> Printf.sprintf "(%s) > (%s)" (string_of_expr e1) (string_of_expr e2)
    | Lt (e1, e2) -> Printf.sprintf "(%s) < (%s)" (string_of_expr e1) (string_of_expr e2)
    | Ge (e1, e2) -> Printf.sprintf "(%s) >= (%s)" (string_of_expr e1) (string_of_expr e2)
    | Le (e1, e2) -> Printf.sprintf "(%s) <= (%s)" (string_of_expr e1) (string_of_expr e2)
    | ToFloat e -> string_of_expr e
    | StartsWith (e1, e2) -> Printf.sprintf "%s starts with %s" (string_of_expr e1) (string_of_expr e2)
    | Contains (e1, e2) -> Printf.sprintf "%s contains %s" (string_of_expr e1) (string_of_expr e2)
    | Value v -> string_of_value v
    | Field f -> f

let but_last l =
    let rec aux prev = function
        | [] -> invalid_arg "l"
        | [_] -> List.rev prev
        | a::l' -> aux (a::prev) l' in
    aux [] l

(*$T but_last
  but_last [1;2;3] = [1;2]
  but_last [1;2] = [1]
  but_last [1] = []
 *)

let value =
    (* from most complex to simpler *)
    either [ cidr      >>: (fun v -> Cidr v) ;
             timestamp >>: (fun v -> Timestamp v) ;
             interval  >>: (fun v -> Interval v) ;
             eth_addr  >>: (fun v -> EthAddr v) ;
             float     >>: (fun v -> Float v) ;   (* must be tried before hostname! *)
             ip_addr   >>: (fun v -> InetAddr v) ;
             bool      >>: (fun v -> Bool v) ;
             integer   >>: (fun v -> Integer v) ;
             (item '"' ++ upto ['"']) >>: (fun (_,v) -> String (String.of_list (but_last v))) ]

(*$T value
  value (String.to_list "true") = Peg.Res (Bool true, [])
  value (String.to_list "false") = Peg.Res (Bool false, [])
  value (String.to_list "\"glop\"") = Peg.Res (String "glop", [])
 *)

type expr_type = TBool | TInteger | TFloat | TStr | TIp | TCidr | TEthAddr | TInterval | TTimestamp
let string_of_type = function
    | TBool -> "boolean" | TInteger -> "integer" | TFloat -> "float"
    | TStr  -> "string"  | TIp  -> "IP address"  | TCidr -> "CIDR subnet"
    | TEthAddr -> "mac"  | TInterval -> "time interval" | TTimestamp -> "datetime"

let fields = ref [ "prout", TIp ]

let field_name =
    let field_name_char = either [ alphabetic ; numeric ; item '_' ] in
    several field_name_char >>= (fun w ->
        let w = String.of_list w in
        try let _p = List.assoc w !fields in
            return w
        with Not_found ->
            Log.info "%s is not a field name" w ;
            fail)

let spaced p =
    none (any blank) ++ some p ++ none (any blank) >>: (fun ((_, v), _) -> v)

let eq_op = spaced (string "==")
let not_op = spaced (either [ign (istring "not"); ign (item '!')])
let or_op = spaced (either [istring "or" ; string "||"])
let and_op = spaced (either [istring "and" ; string "&&"])
let gt_op = spaced (string ">")
let lt_op = spaced (string "<")
let ge_op = spaced (string ">=")
let le_op = spaced (string "<=")
let starts_with_op = spaced (istring "starts with")
let contains_op = spaced (istring "contains")

let rec term_1 bs = (* highest priority *)
    either [ (item '(' ++ term_3 ++ item ')') >>: (fun ((_,e),_) -> e) ;
             value >>: (fun v -> Value v) ;
             field_name >>: (fun n -> Field n) ] bs

and term_2 bs =
    either [ (not_op ++ term_1) >>: (fun (_op,e) -> Not e) ;
             (term_1 ++ eq_op ++ term_1) >>: (fun ((e1,_op),e2) -> Eq (e1,e2)) ;
             (* Le/Ge before Lt/Gt since operators share beginning of name *)
             (term_1 ++ ge_op ++ term_1) >>: (fun ((e1,_op),e2) -> Ge (e1,e2)) ;
             (term_1 ++ le_op ++ term_1) >>: (fun ((e1,_op),e2) -> Le (e1,e2)) ;
             (term_1 ++ gt_op ++ term_1) >>: (fun ((e1,_op),e2) -> Gt (e1,e2)) ;
             (term_1 ++ lt_op ++ term_1) >>: (fun ((e1,_op),e2) -> Lt (e1,e2)) ;
             (term_1 ++ starts_with_op ++ term_1) >>: (fun ((e1,_op),e2) -> StartsWith (e1,e2)) ;
             (term_1 ++ contains_op ++ term_1) >>: (fun ((e1,_op),e2) -> Contains (e1,e2)) ;
             term_1 ] bs

and term_3 bs =
    either [ (term_2 ++ or_op ++ term_2) >>: (fun ((e1,_op),e2) -> Or (e1,e2)) ;
             (term_2 ++ and_op ++ term_2) >>: (fun ((e1,_op),e2) -> And (e1,e2)) ;
             term_2 ] bs

(*$T term_3
  term_3 (String.to_list "true==false") = \
    Peg.Res (Eq (Value (Bool true), Value (Bool false)), [])
  term_3 (String.to_list "true == false") = \
    Peg.Res (Eq (Value (Bool true), Value (Bool false)), [])
  term_3 (String.to_list "false starts with true") = \
    Peg.Res (StartsWith (Value (Bool false), Value (Bool true)), [])
  term_3 (String.to_list "true>=false") = \
    Peg.Res (Ge (Value (Bool true), Value (Bool false)), [])
 *)

let expr = term_3 ++ eof >>: fst

(*$T expr
  expr (String.to_list "true") = Peg.Res (Value (Bool true), [])
  expr (String.to_list "true") = Peg.Res (Value (Bool true), [])
  expr (String.to_list "true==") = Peg.Fail
  expr (String.to_list "true==false") = \
    Peg.Res (Eq (Value (Bool true), Value (Bool false)), [])
  expr (String.to_list "(true==false) == false") = \
    Peg.Res (Eq (Eq (Value (Bool true), Value (Bool false)), Value (Bool false)), [])
  expr (String.to_list "true == false || false == true") = \
    Peg.Res (Or (Eq (Value (Bool true), Value (Bool false)), Eq (Value (Bool false), Value (Bool true))), [])
 *)

(** {2 Type checking} *)

exception Type_error of (expr * expr_type (* actual type *) * expr_type (* expected type *))

let rec type_of_expr = function
    | Value v -> type_of_value v
    | Field f -> type_of_field f
    | Eq (e1,e2) | Gt (e1,e2) | Lt (e1,e2) | Ge (e1,e2) | Le (e1,e2) ->
        check_same_type e1 e2 ;
        TBool
    | Not e ->
        check TBool e ;
        TBool
    | Or (e1,e2) | And (e1,e2) ->
        check TBool e1 ;
        check TBool e2 ;
        TBool
    | StartsWith (e1,e2) | Contains (e1,e2) ->
        check TStr e1 ;
        check TStr e2 ;
        TBool
    | ToFloat e ->
        check TInteger e ;
        TFloat
and check t e =
    let t' = type_of_expr e in
    if t' <> t then raise (Type_error (e, t', t))
and check_same_type e1 e2 =
    let t1 = type_of_expr e1 and t2 = type_of_expr e2 in
    if t1 <> t2 then raise (Type_error (e2, t2, t1))
and type_of_field s =
    (* we know at this point that this field exists *)
    List.assoc s !fields
and type_of_value = function
    | Cidr _      -> TCidr
    | InetAddr _  -> TIp
    | Bool _      -> TBool
    | String _    -> TStr
    | Integer _   -> TInteger
    | Float _     -> TFloat
    | EthAddr _   -> TEthAddr
    | Interval _  -> TInterval
    | Timestamp _ -> TTimestamp

(* Promote ints to floats where required by adding ToFloat operations *)
let rec promote_to_float = function
    | Eq (e1, e2) -> let e1', e2' = may_promote e1 e2 in Eq (e1', e2')
    | Gt (e1, e2) -> let e1', e2' = may_promote e1 e2 in Eq (e1', e2')
    | Lt (e1, e2) -> let e1', e2' = may_promote e1 e2 in Eq (e1', e2')
    | Ge (e1, e2) -> let e1', e2' = may_promote e1 e2 in Eq (e1', e2')
    | Le (e1, e2) -> let e1', e2' = may_promote e1 e2 in Eq (e1', e2')
    | x -> x
and may_promote e1 e2 =
    let t1 = type_of_expr e1 and t2 = type_of_expr e2 in
    if t1 = TInteger && t2 = TFloat then (ToFloat e1, e2) else
    if t1 = TFloat && t1 = TInteger then (e1, ToFloat e2) else
    e1, e2

(* helper for the above tests *)
let type_of_string str =
    match expr (String.to_list str) with
    | Res (r, []) -> type_of_expr (promote_to_float r)
    | _ -> assert false

(*$T type_of_string
  type_of_string "(true==false) == false" = TBool
  type_of_string "(666 > 42) && true" = TBool
  type_of_string "\"glop glop\" starts with \"glop\"" = TBool
  type_of_string "1 == 1.0 || 1.2 <= 2" = TBool
 *)

(* {2 Convertion into an OCaml string} *)

let rec ocaml_of_expr = function
    (* TODO: for some e1/e2 that are values of certain types, use this type's cmp instead for Eq and Gt etc *)
    | Eq (e1, e2) -> "("^ ocaml_of_expr e1 ^" = "^ ocaml_of_expr e2 ^")"
    | Not e -> "(!"^ ocaml_of_expr e ^")"
    | And (e1, e2) -> "("^ ocaml_of_expr e1 ^" && "^ ocaml_of_expr e2 ^")"
    | Or (e1, e2) -> "("^ ocaml_of_expr e1 ^" || "^ ocaml_of_expr e2 ^")"
    | Gt (e1, e2) -> "("^ ocaml_of_expr e1 ^" > "^ ocaml_of_expr e2 ^")"
    | Ge (e1, e2) -> "("^ ocaml_of_expr e1 ^" >= "^ ocaml_of_expr e2 ^")"
    | Lt (e1, e2) -> "("^ ocaml_of_expr e1 ^" < "^ ocaml_of_expr e2 ^")"
    | Le (e1, e2) -> "("^ ocaml_of_expr e1 ^" <= "^ ocaml_of_expr e2 ^")"
    | ToFloat v -> "(float_of_int "^ ocaml_of_expr v ^")"
    | StartsWith (s1, s2) -> "(String.starts_with "^ ocaml_of_expr s1 ^" "^ ocaml_of_expr s2 ^")"
    | Contains (s1, s2) -> "(String.exists "^ ocaml_of_expr s2 ^" "^ ocaml_of_expr s1 ^")"
    | Value v -> "("^ ocaml_of_value v ^")"
    | Field f -> "("^ ocaml_of_field f ^")"
and ocaml_of_field f = f
and ocaml_of_value = function
    | Cidr v      -> Datatype.Cidr.to_imm v
    | InetAddr v  -> Datatype.InetAddr.to_imm v
    | Integer v   -> Datatype.Integer.to_imm v
    | Float v     -> Datatype.Float.to_imm v
    | Bool v      -> Datatype.Bool.to_imm v
    | String v    -> Datatype.Text.to_imm v
    | EthAddr v   -> Datatype.EthAddr.to_imm v
    | Interval v  -> Datatype.Interval.to_imm v
    | Timestamp v -> Datatype.Timestamp.to_imm v

(* FIXME: belongs to battery *)
let indent n str =
    let tab = String.make n ' ' in
    let sep = "\n" ^ tab in
    tab ^ (String.nsplit str "\n" |>
           String.concat sep)

let string_of_type_error (e, actual_t, expected_t) =
    Printf.sprintf "Type error: expression\n%s\nshould have type %s but has type %s instead"
        (indent 2 (string_of_expr e))
        (string_of_type expected_t)
        (string_of_type actual_t)

let () =
    Printexc.register_printer (function
        | Type_error x -> Some (string_of_type_error x)
        | _ -> None)

(* Simple utility to get an expression of given type from a string, given some
 * possible fields *)

exception Parse_error

let expression expected_t fields' str =
    fields := fields' ;
    match expr (String.to_list str) with
    | Res (e, []) ->
        let e = promote_to_float e in
        check expected_t e ; e
    | _ -> raise Parse_error

