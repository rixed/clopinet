open Batteries
open Datatype

let () = Prefs.set_base "./conf-check"

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
    assert_exc Peg.Parse_error of_string "42m"
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
    assert_exc Peg.Parse_error of_string "" ;
    assert (of_string "2012-10-25 11:23:02" = 1351156982000L) ;
    assert (of_string "2012-10-25 11:23" = 1351156980000L) ;
    assert (of_string "2012-10-25" = 1351116000000L) ;
    assert (to_string 1351156980000L = "2012-10-25 11:23") ;
    assert_exc Peg.Parse_error of_string "2012-10-25 11:23:02 glop" ;
    (* check time+interval format *)
    assert (of_string "2012-10-25 +2months" = of_string "2012-12-25") ;
    assert (of_string "2012-10-25 -3 w -4 d" = of_string "2012-09-30") ;
    assert (of_string "2012-10-25 -3w-4d " = of_string "2012-09-30") ;
    assert (compare (of_string "now-1d") (of_string "now +2 w") = -1) ;
    assert (compare (of_string "now-1min") (of_string "now -1 min") = 0) ;
    (* when no units nor sign are given, it's a timestamp *)
    assert (compare (of_string "1323765999.42") (of_string "2011-12-13 09:46:39.42") = 0) ;
    (* parser must handle junkie's format *)
    assert (parzer (s2l "1323766045s 962156us") = Peg.Res (1323766045962L, []))

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
    assert (string_of_number 0.004 = "4m") ;
    assert (string_of_number ~-.0.004 = "-4m") ;
    assert (string_of_number ~-.4000. = "-4k") ;
    assert (string_of_number 0. = "0")

let check_prefs () =
    let open Prefs in
    List.enum [ "over1", "value1" ; "over2", "42" ] |>
    Hashtbl.of_enum |>
    overwrite_many ;
    assert (get_string "over1" "" = "value1") ;
    assert (get_int "over2" 0 = 42) ;
    assert (get_float "over2" 0. = 42.) ;
    assert (get_int "not_defined" 1 = 1) ;
    assert (get_int "foo/bar" 0 = 42) ;
    assert (get_bool "boolean" false = true) ;
    assert (get_string "comment" "" = "") ;
    assert_exc (Scanf.Scan_failure "scanf: bad input at char number 0: ``float_of_string''") (get_int "boolean") 0 ;
    assert (get_int "big" 0 = 1_500_000) ;
    overwrite_single "not_defined = 42" ;
    assert (get_int "not_defined" 0 = 42)

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
    exit (if !ok then 0 else 1)
