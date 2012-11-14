open Bricabrac
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

let check_datatools () =
    assert (string_of_list ['a'; 'b'; 'c'] = "abc") ;
    assert (string_of_list [] = "")
    (* TODO: read_txt_until *)

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
    assert_exc Overflow of_string "128"
    (* TODO: more int checks *)

let check_cidr () =
    assert (in_cidr (InetAddr.of_string "192.168.1.2") (Cidr.of_string "192.168.0.0/16")) ;
    (* CIDR must not be well formed, ie masked bits can have any value *)
    assert (in_cidr (InetAddr.of_string "192.168.1.2") (Cidr.of_string "192.168.1.2/16")) ;
    assert (in_cidr (InetAddr.of_string "192.168.1.2") (Cidr.of_string "192.168.1.2/32")) ;
    assert (in_cidr (InetAddr.of_string "192.168.0.0") (Cidr.of_string "192.168.0.0/16")) ;
    assert (in_cidr (InetAddr.of_string "192.168.255.255") (Cidr.of_string "192.168.0.0/16")) ;
    assert (Cidr.of_string "1.2.3.4" = (InetAddr.of_string "1.2.3.4", 32))

let check_mac () =
    let open EthAddr in
    assert (of_string "01:23:45:67:89:ab" =
        Char.(chr 1, chr 0x23, chr 0x45, chr 0x67, chr 0x89, chr 0xab))

let check_timestamp () =
    let open Timestamp in
    assert_exc End_of_file of_string "" ;
    assert (of_string "2012-10-25 11:23:02" = 1351156982000L) ;
    assert (of_string "2012-10-25 11:23" = 1351156980000L) ;
    assert (of_string "2012-10-25 11" = 1351155600000L) ;
    assert (of_string "2012-10-25" = 1351116000000L) ;
    assert (to_string 1351156980000L = "2012-10-25 11:23:00.000") ;
    assert_exc (Failure "Cannot consume all input") of_string "2012-10-25 11:23:02 glop" ;
    (* check time+interval format *)
    assert (of_string "2012-10-25 +2months" = of_string "2012-12-25") ;
    assert (of_string "2012-10-25 -3w 4d" = of_string "2012-09-30") ;
    assert (of_string "2012-10-25 - 3w 4d " = of_string "2012-09-30") ;
    assert (of_string "2012-10-25 -3w4d" = of_string "2012-09-30")

let check_interval () =
    let open Interval in
    (* nothing specified -> seconds *)
    assert (of_string "42" = { zero_interval with secs = 42 }) ;
    (* check various units *)
    assert (of_string "42years" = { zero_interval with years = 42 }) ;
    assert (of_string "42 year" = { zero_interval with years = 42 }) ;
    assert (of_string "42y" = { zero_interval with years = 42 }) ;
    assert (of_string "42 y" = { zero_interval with years = 42 }) ;
    assert (of_string "42 weeks" = { zero_interval with weeks = 42 }) ;
    assert (of_string "42s" = { zero_interval with secs = 42 }) ;
    assert (of_string "42 ms" = { zero_interval with msecs = 42 }) ;
    assert (of_string "42 msec" = { zero_interval with msecs = 42 }) ;
    assert (of_string "42 msecs" = { zero_interval with msecs = 42 }) ;
    assert (of_string "6y 4months 42 secs" =
        { zero_interval with years = 6 ; months = 4 ; secs = 42 })

module TestOption = Option (Integer8)
let check_option () =
    let open TestOption in
    assert (of_string "None" = None) ;
    assert (of_string "Some 1" = Some 1) ;
    assert (of_string "Some -42" = Some ~-42)

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

let _ =
    check_datatools () ;
    check_ints () ;
    check_cidr () ;
    check_mac () ;
    check_timestamp () ;
    check_interval () ;
    check_option () ;
    check_serial () ;
    print_string "Ok\n"
