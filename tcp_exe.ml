open Datatype
open Tcp

let main =
    let start = ref None and stop = ref None
    and client = ref None and server = ref None
    and peer = ref None in
    Arg.(parse [
        "-load", String (fun s -> load s), "load a CSV file" ;
        "-dump", String (function tbname -> Tcp.(iter ?start:!start ?stop:!stop
                                                      ?client:!client ?server:!server ?peer:!peer
                                                      tbname
                                                      (fun x -> write_txt Output.stdout x ; print_newline ()))), "dump this table" ;
        "-dbck", Unit (fun () -> Metric.dbck "tcp" lods Tcp.read Tcp.meta_read), "scan the DB and try to repair it" ;
        "-purge", Unit (fun () -> Metric.purge "tcp" lods), "purge old datafiles" ;
        "-start", String (fun s -> start := Some (Timestamp.of_string s)), "limit queries to timestamps after this" ;
        "-stop",  String (fun s -> stop  := Some (Timestamp.of_string s)), "limit queries to timestamps before this" ;
        "-client", String (fun s -> client := Some (Cidr.of_string s)), "limit to these clients" ;
        "-server", String (fun s -> server := Some (Cidr.of_string s)), "limit to these servers" ;
        "-peer", String (fun s -> peer := Some (Cidr.of_string s)), "limit to these clients or servers" ]
        (fun x -> raise (Bad x))
        "Operate the TCP sockets DB")

