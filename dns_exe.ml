open Datatype
open Dns

let main =
    let start = ref None and stop = ref None
    and rt_min = ref None and rt_max = ref None and qname = ref None and error = ref None
    and ip_clt = ref None and ip_srv = ref None and peer =ref None
    and tx_min = ref None in
    Arg.(parse [
        "-load", String (fun s -> load s), "load a CSV file" ;
        "-dump", String (function tbname -> Dns.(iter ?start:!start ?stop:!stop ?rt_min:!rt_min ?rt_max:!rt_max
                                                      ?ip_clt:!ip_clt ?ip_srv:!ip_srv ?peer:!peer
                                                      ?qname:!qname ?error:!error ?tx_min:!tx_min
                                                      tbname
                                                      (fun x -> write_txt Output.stdout x ; print_newline ()))), "dump content of this table" ;
        "-dbck", Unit (fun () -> Metric.dbck "dns" lods Dns.read Dns.meta_read), "scan the DB and try to repair it" ;
        "-purge", Unit (fun () -> Metric.purge "dns" lods), "purge old datafiles" ;
        "-start", String (fun s -> start := Some (Timestamp.of_string s)), "limit queries to timestamps after this" ;
        "-stop",  String (fun s -> stop  := Some (Timestamp.of_string s)), "limit queries to timestamps before this" ;
        "-rt-min", String (fun s -> rt_min := Some (Float.of_string s)), "limit queries to resptimes above this" ;
        "-rt-max", String (fun s -> rt_max := Some (Float.of_string s)), "limit queries to resptimes below this" ;
        "-tx-min", Int (fun n -> tx_min := Some n), "limit results to servers that answered at least this number of queries" ;
        "-qname", String (fun s -> qname := Some s), "limit queries to those ending with this" ;
        "-error", Int (fun i -> error := Some i), "select only queries with this error code" ;
        "-client", String (fun s -> ip_clt := Some (Cidr.of_string s)), "limit to these clients" ;
        "-server", String (fun s -> ip_srv := Some (Cidr.of_string s)), "limit to these servers" ;
        "-peer", String (fun s -> peer := Some (Cidr.of_string s)), "limit to these clients or servers" ]
        (fun x -> raise (Bad x))
        "Operate the DNS response times DB")

