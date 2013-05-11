open Batteries
open Datatype

let assert_exc exn f x =
    let ok =
        try ignore (f x) ;
            Printf.printf "Got a result instead of \"%s\"\n%!" (Printexc.to_string exn) ;
            false
        with exn' ->
            if exn <> exn' then
                Printf.printf "Got exn \"%s\" instead of \"%s\"\n%!" (Printexc.to_string exn') (Printexc.to_string exn) ;
            assert (exn = exn') ;
            true in
    assert ok

let s2l = String.to_list

let check_datatools () =
    assert (string_of_list ['a'; 'b'; 'c'] = "abc") ;
    assert (string_of_list [] = "")

let check_ints () =
    let open UInteger8 in
    assert (of_string "42" = 42) ;
    assert (of_string "0" = 0) ;
    assert (of_string "-0" = 0) ;
    assert_exc Overflow of_string "-10" ;
    assert_exc Overflow of_string "256" ;
    let open Integer8 in
    assert (of_string "42" = 42) ;
    assert (of_string "0" = 0) ;
    assert (of_string "-0" = 0) ;
    assert (of_string "-10" = ~-10) ;
    assert_exc Overflow of_string "-129" ;
    assert_exc Overflow of_string "128" ;
    (* Check preffixes *)
    let open Integer in
    assert (of_string "42k" = 42_000) ;
    assert (of_string "42K" = 43_008) ;
    assert (of_string "42M" = 42_000_000) ;
    assert (of_string "42G" = 42_000_000_000) ;
    assert_exc (Peg.Parse_error ("EOF expected", 1)) of_string "42m" ;
    (* Non regression tests *)
    let open UInteger16 in
    assert (to_string min_int = "-4611686018427387904")
    (* TODO: more int checks *)

let check_inet_addr () =
    assert (InetAddr.parzer (s2l "193.48.57.46") = Peg.Res (InetAddr.of_string "193.48.57.46", []))

let check_cidr () =
    assert (in_cidr (InetAddr.of_string "192.168.1.2") (Cidr.of_string "192.168.0.0/16")) ;
    (* CIDR must not be well formed, ie masked bits can have any value *)
    assert (in_cidr (InetAddr.of_string "192.168.1.2") (Cidr.of_string "192.168.1.2/16")) ;
    assert (in_cidr (InetAddr.of_string "192.168.1.2") (Cidr.of_string "192.168.1.2/32")) ;
    assert (in_cidr (InetAddr.of_string "192.168.0.0") (Cidr.of_string "192.168.0.0/16")) ;
    assert (in_cidr (InetAddr.of_string "192.168.255.255") (Cidr.of_string "192.168.0.0/16")) ;
    assert (Cidr.of_string "1.2.3.4" = (InetAddr.of_string "1.2.3.4", 32)) ;
    (* we must be able to parse an IP as a Cidr *)
    assert (Cidr.parzer (s2l "1.2.3.4") = Peg.Res ((InetAddr.of_string "1.2.3.4", 32), []))

let check_mac () =
    let open EthAddr in
    assert (of_string "01:23:45:67:89:ab" =
        Char.(chr 1, chr 0x23, chr 0x45, chr 0x67, chr 0x89, chr 0xab)) ;
    assert (parzer (s2l "88:43:e1:1d:6d:01") = Peg.Res (Char.(chr 0x88, chr 0x43, chr 0xe1, chr 0x1d, chr 0x6d, chr 0x01), []))

let check_timestamp () =
    let open Timestamp in
    assert_exc (Peg.Parse_error ("unknown error", 0)) of_string "" ;
    assert (of_string "2012-10-25 11:23:02.3" = 1351156982300L) ;
    assert (of_string "2012-10-25 11:23:02" = 1351156982000L) ;
    assert (of_string "2012-10-25 11:23" = 1351156980000L) ;
    assert (of_string "2012-10-25" = 1351116000000L) ;
    assert (to_string 1351156980000L = "2012-10-25 11:23") ;
    assert_exc (Peg.Parse_error ("EOF expected", 5)) of_string "2012-10-25 11:23:02 glop" ;
    assert_exc (Peg.Parse_error ("Failure: Integer not in [0:23]", 3)) of_string "2012-10-25 70:23" ;
    (* when no units nor sign are given, it's a timestamp *)
    assert (compare (of_string "1323765999.42") (of_string "2011-12-13 09:46:39.42") = 0) ;
    (* parser must handle junkie's format *)
    assert (parzer (s2l "1323766045s 962156us") = Peg.Res (1323766045962L, [])) ;
    assert (parzer (s2l "1323766045s") = Peg.Res (1323766045000L, []))

let check_interval () =
    let open Interval in
    assert (of_string "42years" = { zero with years = 42. }) ;
    (* nothing specified -> seconds (we need this to parse junkie's output) *)
    assert (of_string "42" = { zero with secs = 42. }) ;
    (* should accept any float notation (from ocaml _and_ javascript) *)
    let i = of_string "8.179775280898875e-7" in
    assert (i.secs = 0. && i.msecs > 0.00081 && i.msecs < 0.00082) ;
    (* check various units *)
    assert (of_string "42years" = { zero with years = 42. }) ;
    assert (of_string "42 year" = { zero with years = 42. }) ;
    assert (of_string "42y" = { zero with years = 42. }) ;
    assert (of_string "42 y" = { zero with years = 42. }) ;
    assert (of_string "42 weeks" = { zero with weeks = 42. }) ;
    assert (of_string "42s" = { zero with secs = 42. }) ;
    assert (of_string "42 ms" = { zero with msecs = 42. }) ;
    assert (of_string "42 msec" = { zero with msecs = 42. }) ;
    assert (of_string "42 msecs" = { zero with msecs = 42. }) ;
    assert (of_string "6y 4months 42 secs" =
        { zero with years = 6. ; months = 4. ; secs = 42. }) ;
    (* check we display something for 0 interval *)
    assert (String.length (to_string zero) > 0) ;
    (* check convertion to msecs and secs *)
    assert (of_string "42 ms" |> to_ms = 42L) ;
    assert (of_string "42 ms" |> to_secs = 0.042)

module TestOption = Option (Integer8)
let check_option () =
    let open TestOption in
    assert (of_string "None" = None) ;
    assert (of_string "Some 1" = Some 1) ;
    assert (of_string "Some -42" = Some ~-42) ;
    assert (parzer (s2l "Some 250") = Peg.Res (Some 250, []))

let check_distribution () =
    let open Distribution in
    assert (parzer (s2l "26246\t26246\t0\t567\t7560") = Peg.Res ((26246, 26246., 0., 567., 7560.), []))

let check_serial () =
    let open Serial in
    let fname = "test.check_serial" in
    ignore_exceptions Unix.unlink fname ;
    with_file_out fname (fun obuf ->
        ser1 obuf true ;
        ser8 obuf 42 ;
        ser8 obuf ~-5 ;
        ser16 obuf 10000 ;
        ser32 obuf 123456l ;
        ser64 obuf 123456789L ;
        ser_varint obuf 123456 ;
        ser_varint obuf 0 ;
        ser_varint obuf 12 ;
        ser_varint obuf ~-5 ;
        ser_varint obuf ~-123456 ;
        ser_string obuf "glop glop" ;
        Cidr.write obuf (Cidr.of_string "192.168.1.0/30") ;
        EthAddr.write obuf (EthAddr.of_string "12:fe:34:56:00:0a") ;
        Integer16.write obuf 5) ;
    (* Check we append in the file *)
    with_file_out fname (fun obuf ->
        ser_string obuf "pas glop") ;
    with_file_in fname (fun ibuf ->
        assert (deser1 ibuf = true) ;
        assert (deser8 ibuf = 42) ;
        assert (deser8 ibuf = 251) ;
        assert (deser16 ibuf = 10000) ;
        assert (deser32 ibuf = 123456l) ;
        assert (deser64 ibuf = 123456789L) ;
        assert (deser_varint ibuf = 123456) ;
        assert (deser_varint ibuf = 0) ;
        assert (deser_varint ibuf = 12) ;
        assert (deser_varint ibuf = ~-5) ;
        assert (deser_varint ibuf = ~-123456) ;
        assert (deser_string ibuf = "glop glop") ;
        assert (Cidr.read ibuf |> Cidr.to_string = "192.168.1.0/30") ;
        assert (EthAddr.read ibuf |> EthAddr.to_string = "12:fe:34:56:00:0a") ;
        assert (Integer16.read ibuf = 5) ;
        assert (deser_string ibuf = "pas glop")) ;
    Unix.unlink fname

let check_disp_numbers () =
    assert (string_of_number 4000. = "4k") ;
    assert (string_of_number 40000. = "40k") ;
    assert (string_of_number 0.004 = "4m") ;
    assert (string_of_number ~-.0.004 = "-4m") ;
    assert (string_of_number ~-.4000. = "-4k") ;
    assert (string_of_number 0. = "0") ;
    assert (string_of_number 1500. = "1.5k") ;
    assert (string_of_number 10_000_000. = "10M")

let check_prefs () =
    let open Prefs in
    List.enum [ "over1", "value1" ; "over2", "42" ] |>
    Hashtbl.of_enum |>
    overwrite_many ;
    assert (get_string "over1" "" = "value1") ;
    assert (get_string "comment" "" = "")

let check_expressions () =
    let open User_filter in
    let inet s = Value (InetAddr (InetAddr.of_string s)) in
    assert_exc (Peg.Parse_error ("Failure: Unknown field 'prout'", 0)) (expression TBool []) "1 == prout" ;
    assert_exc (Peg.Parse_error ("Failure: Unknown field 'prout'", 5)) (expression TBool []) "prout == 1" ;
    assert (expression TBool [] "( 10.0.0.2 == 10.0.0.1) || true" =
        Or (Eq (inet "10.0.0.2", inet "10.0.0.1"), Value (Bool true))) ;
    assert (expression TBool Traffic.Traffic.fields "( 10.0.0.2 == 10.0.0.1 ) || true" =
        Or (Eq (inet "10.0.0.2", inet "10.0.0.1"), Value (Bool true))) ;
    (* check time+interval format *)
    assert (match expression TTimestamp [] "2012-10-25 +2months" with Add (Value (Timestamp _), Value (Interval _)) -> true | _ -> false) ;
    assert (match expression TTimestamp [] "now+1d" with Add (Value (Timestamp _), Value (Interval _)) -> true | _ -> false) ;
    assert (match expression TInteger [] "(1 + 2) + 3" with Add (Add (Value (Integer 1), Value (Integer 2)), Value (Integer 3)) -> true | _ -> false) ;
    assert (match expression TInteger [] "1 * 2 + 3" with Add (Mul (Value (Integer 1), Value (Integer 2)), Value (Integer 3)) -> true | _ -> false) ;
    assert (match expression TInteger [] "1 + 2 * 3" with Add (Value (Integer 1), Mul (Value (Integer 2), Value (Integer 3))) -> true | _ -> false) ;
    assert (expression TBool Traffic.Traffic.fields "ip_src == 213.251.171.101" = Eq (Field "ip_src", inet "213.251.171.101"))

module TestList = ListOf (Integer)
module TestAltern = Altern2 (Integer) (Float)
let check_samples () =
    let c samples of_string =
        List.iter (fun s ->
            assert (try ignore (of_string s) ; true with _ -> false))
            samples in
    Bool.(c samples of_string) ;
    Void.(c samples of_string) ;
    Text.(c samples of_string) ;
    Float.(c samples of_string) ;
    Integer.(c samples of_string) ;
    UInteger.(c samples of_string) ;
    Integer8.(c samples of_string) ;
    UInteger8.(c samples of_string) ;
    Integer16.(c samples of_string) ;
    UInteger16.(c samples of_string) ;
    Integer32.(c samples of_string) ;
    UInteger32.(c samples of_string) ;
    Integer64.(c samples of_string) ;
    InetAddr.(c samples of_string) ;
    Cidr.(c samples of_string) ;
    EthAddr.(c samples of_string) ;
    Interval.(c samples of_string) ;
    Timestamp.(c samples of_string) ;
    TestList.(c samples of_string) ;
    TestAltern.(c samples of_string) ;
    TestOption.(c samples of_string) ;
    VLan.(c samples of_string) ;
    Origin.(c samples of_string)

let ok = ref true
let check name f =
    try f () ;
        Printf.printf "%s: Ok\n" name
    with _ ->
        ok := false ;
        Printf.printf "%s: Fail:\n" name ;
        Printexc.print_backtrace stdout ;
        Printf.printf "----------\n"

let () =
    check "datatools" check_datatools ;
    check "ints" check_ints ;
    check "inet_addr" check_inet_addr ;
    check "cidr" check_cidr ;
    check "mac" check_mac ;
    check "timestamp" check_timestamp ;
    check "interval" check_interval ;
    check "option" check_option ;
    check "distribution" check_distribution ;
    check "serial" check_serial ;
    check "disp_numbers" check_disp_numbers ;
    check "prefs" check_prefs ;
    check "expressions" check_expressions ;
    check "sample values" check_samples ;
    exit (if !ok then 0 else 1)
