open Bricabrac
open Datatype

let check_datatools () =
    assert (string_of_list ['a'; 'b'; 'c'] = "abc") ;
    assert (string_of_list [] = "")
    (* TODO: read_txt_until *)

let check_cidr () =
    assert (in_cidr (ip_of_string "192.168.1.2") (cidr_of_string "192.168.0.0/16")) ;
    assert (in_cidr (ip_of_string "192.168.0.0") (cidr_of_string "192.168.0.0/16")) ;
    assert (in_cidr (ip_of_string "192.168.255.255") (cidr_of_string "192.168.0.0/16"))

let _ =
    check_datatools () ;
    check_cidr () ;
    print_string "Ok\n"
