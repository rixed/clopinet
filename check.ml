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
    assert (of_string "123s 456ms"  = 123456L) ;
    assert (of_string "123 456ms"   = 123456L) ;
    assert (of_string "123s 456us"  = 123000L) ;
    assert (of_string "123s 1456us" = 123001L) ;
    assert (of_string "123s 1678us" = 123002L) ;
    assert (of_string "123 456"     = 123456L) ;
    assert (of_string "123s 456"    = 123456L) ;
    assert (of_string "123s"        = 123000L) ;
    assert (of_string "123"         = 123000L) ;
    assert (of_string "123."        = 123000L) ;
    assert (of_string "123.456"     = 123456L) ;
    assert (of_string "123.4569"    = 123457L) ;
    assert (of_string "123.1"       = 123100L) ;
    assert_exc (Failure "Cannot consume all input") of_string "-1" ;
    assert_exc End_of_file of_string ""

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
        assert (deser_string ibuf 9 = "glop glop") ;
        assert (Cidr.read ibuf |> Cidr.to_string = "192.168.1.0/30") ;
        assert (Integer16.read ibuf = 5) ;
        assert (deser_string ibuf 8 = "pas glop")) ;
    Unix.unlink fname

let _ =
    check_datatools () ;
    check_ints () ;
    check_cidr () ;
    check_mac () ;
    check_timestamp () ;
    check_option () ;
    check_serial () ;
    print_string "Ok\n"
