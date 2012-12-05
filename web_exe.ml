open Datatype
open Web

let main =
    let dbdir = ref "./" and start = ref None and stop = ref None
    and rt_min = ref None and url = ref None and status = ref None
    and client = ref None and server = ref None and host = ref None
    and peer = ref None and methd = ref None and create = ref false in
    Arg.(parse [
        "-dir", Set_string dbdir, "database directory (or './')" ;
        "-create", Set create, "create db if it does not exist yet" ;
        "-load", String (fun s -> load !dbdir !create s), "load a CSV file" ;
        "-verbose", Unit (fun () -> verbose := true; Metric.verbose := true), "verbose" ;
        "-c", String Prefs.overwrite_single, "overwrite conf" ;
        "-dump", String (function tbname -> Web.(iter ?start:!start ?stop:!stop ?rt_min:!rt_min
                                                      ?client:!client ?server:!server ?peer:!peer
                                                      ?methd:!methd ?host:!host
                                                      ?url:!url ?status:!status !dbdir tbname
                                                      (fun x -> write_txt Output.stdout x ; print_newline ()))), "dump this table" ;
        "-start", String (fun s -> start := Some (Timestamp.of_string s)), "limit queries to timestamps after this" ;
        "-stop",  String (fun s -> stop  := Some (Timestamp.of_string s)), "limit queries to timestamps before this" ;
        "-rt-min", String (fun s -> rt_min := Some (Float.of_string s)), "limit queries to resptimes greater than this" ;
        "-url", String (fun s -> url := Some s), "limit queries to those which URL starts with this" ;
        "-host", String (fun s -> host := Some s), "limit queries to those which host ends with this" ;
        "-status", Int (fun i -> status := Some i), "select only queries with this status code" ;
        "-method", Int (fun i -> methd := Some i), "select only queries with this method code" ;
        "-client", String (fun s -> client := Some (Cidr.of_string s)), "limit to these clients" ;
        "-server", String (fun s -> server := Some (Cidr.of_string s)), "limit to these servers" ;
        "-peer", String (fun s -> peer := Some (Cidr.of_string s)), "limit to these clients or servers" ]
        (fun x -> raise (Bad x))
        "Operate the HTTP response times DB")

