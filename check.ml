open Bricabrac
open Datatype

let check_datatools () =
    assert (string_of_list ['a'; 'b'; 'c'] = "abc") ;
    assert (string_of_list [] = "")
    (* TODO: read_txt_until *)

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
    assert (of_string "123"        = (123L, 0))

let _ =
    check_datatools () ;
    check_cidr () ;
    check_mac () ;
    check_timestamp () ;
    print_string "Ok\n"
