open Datatype
open Dns

let main =
    let dbdir = ref "./" and start = ref None and stop = ref None
    and rt_min = ref None and qname = ref None and error = ref None
    and client = ref None and server = ref None and peer =ref None
    and create = ref false and tx_min = ref None in
    Arg.(parse [
        "-dir", Set_string dbdir, "database directory (or './')" ;
        "-create", Set create, "create db if it does not exist yet" ;
        "-load", String (fun s -> load !dbdir !create s), "load a CSV file" ;
        "-verbose", Unit (fun () -> verbose := true; Metric.verbose := true), "verbose" ;
        "-c", String Prefs.overwrite_single, "overwrite conf" ;
        "-dump", String (function tbname -> Dns.(iter ?start:!start ?stop:!stop ?rt_min:!rt_min
                                                      ?client:!client ?server:!server ?peer:!peer
                                                      ?qname:!qname ?error:!error ?tx_min:!tx_min
                                                      !dbdir tbname
                                                      (fun x -> write_txt Output.stdout x ; print_newline ()))), "dump content of this table" ;
        "-start", String (fun s -> start := Some (Timestamp.of_string s)), "limit queries to timestamps after this" ;
        "-stop",  String (fun s -> stop  := Some (Timestamp.of_string s)), "limit queries to timestamps before this" ;
        "-rt-min", String (fun s -> rt_min := Some (Float.of_string s)), "limit queries to resptimes greater than this" ;
        "-tx-min", Int (fun n -> tx_min := Some n), "limit results to servers that answered at least this number of queries" ;
        "-qname", String (fun s -> qname := Some s), "limit queries to those ending with this" ;
        "-error", Int (fun i -> error := Some i), "select only queries with this error code" ;
        "-client", String (fun s -> client := Some (Cidr.of_string s)), "limit to these clients" ;
        "-server", String (fun s -> server := Some (Cidr.of_string s)), "limit to these servers" ;
        "-peer", String (fun s -> peer := Some (Cidr.of_string s)), "limit to these clients or servers" ]
        (fun x -> raise (Bad x))
        "Operate the DNS response times DB")

