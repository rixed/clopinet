open Datatype
open Tcp

let main =
    let dbdir = ref "./" and start = ref None and stop = ref None
    and dur_min = ref None and dur_max = ref None
    and client = ref None and server = ref None
    and peer = ref None and create = ref false in
    Arg.(parse [
        "-dir", Set_string dbdir, "database directory (or './')" ;
        "-create", Set create, "create db if it does not exist yet" ;
        "-load", String (fun s -> load !dbdir !create s), "load a CSV file" ;
        "-verbose", Unit (fun () -> verbose := true; Metric.verbose := true), "verbose" ;
        "-j", Set_int Table.ncores, "number of cores (default: 1)" ;
        "-dump", String (function tbname -> Tcp.(iter ?start:!start ?stop:!stop
                                                      ?dur_min:!dur_min ?dur_max:!dur_max
                                                      ?client:!client ?server:!server ?peer:!peer
                                                      !dbdir tbname
                                                      (fun x -> write_txt Output.stdout x ; print_newline ()))), "dump this table" ;
        "-start", String (fun s -> start := Some (Timestamp.of_string s)), "limit queries to timestamps after this" ;
        "-stop",  String (fun s -> stop  := Some (Timestamp.of_string s)), "limit queries to timestamps before this" ;
        "-min-duration", String (fun s -> dur_min := Some (Float.of_string s)), "limit queries to sockets longer than this" ;
        "-max-duration", String (fun s -> dur_max := Some (Float.of_string s)), "limit queries to sockets shorter than this" ;
        "-client", String (fun s -> client := Some (Cidr.of_string s)), "limit to these clients" ;
        "-server", String (fun s -> server := Some (Cidr.of_string s)), "limit to these servers" ;
        "-peer", String (fun s -> peer := Some (Cidr.of_string s)), "limit to these clients or servers" ]
        (fun x -> raise (Bad x))
        "Operate the TCP sockets DB")

