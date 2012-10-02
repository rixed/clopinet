open Bricabrac
open Datatype

let assert_exc exn f x =
    try ignore (f x) ; assert false
    with exn' ->
        if exn <> exn' then
            Printf.printf "Got exn \"%s\" instead of \"%s\"\n" (Printexc.to_string exn') (Printexc.to_string exn) ;
        assert (exn = exn')

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
    assert (of_string "123s 456us" = (123L, 456)) ;
    assert (of_string "123 456us"  = (123L, 456)) ;
    assert (of_string "123 456"    = (123L, 456)) ;
    assert (of_string "123s 456"   = (123L, 456)) ;
    assert (of_string "123s"       = (123L, 0)) ;
    assert (of_string "123"        = (123L, 0)) ;
    assert_exc End_of_file of_string ""

module TestOption = Option (Integer8)
let check_option () =
    let open TestOption in
    assert (of_string "None" = None) ;
    assert (of_string "Some 1" = Some 1) ;
    assert (of_string "Some -42" = Some ~-42)

let _ =
    check_datatools () ;
    check_ints () ;
    check_cidr () ;
    check_mac () ;
    check_timestamp () ;
    check_option () ;
    print_string "Ok\n"
