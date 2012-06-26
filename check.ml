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

let check_timestamp () =
    assert (Timestamp.of_string "123s 456us" = (123L, 456)) ;
    assert (Timestamp.of_string "123 456us"  = (123L, 456)) ;
    assert (Timestamp.of_string "123 456"    = (123L, 456)) ;
    assert (Timestamp.of_string "123s 456"   = (123L, 456)) ;
    assert (Timestamp.of_string "123s"       = (123L, 0)) ;
    assert (Timestamp.of_string "123"        = (123L, 0))

let _ =
    check_datatools () ;
    check_cidr () ;
    check_timestamp () ;
    print_string "Ok\n"
