open Datatype
open Web

let main =
    let start = ref None and stop = ref None
    and rt_min = ref None and url = ref None and status = ref None
    and ip_clt = ref None and ip_srv = ref None and host = ref None
    and peer = ref None and methd = ref None in
    Arg.(parse [
        "-load", String (fun s -> load s), "load a CSV file" ;
        "-dump", String (function tbname -> Web.(iter ?start:!start ?stop:!stop ?rt_min:!rt_min
                                                      ?ip_clt:!ip_clt ?ip_srv:!ip_srv ?peer:!peer
                                                      ?methd:!methd ?host:!host
                                                      ?url:!url ?status:!status tbname
                                                      (fun x -> write_txt Output.stdout x ; print_newline ()))), "dump this table" ;
        "-dbck", Unit (fun () -> Metric.dbck "web" lods Web.read Web.meta_read), "scan the DB and try to repair it" ;
        "-purge", Unit (fun () -> Metric.purge "web" lods), "purge old datafiles" ;
        "-start", String (fun s -> start := Some (Timestamp.of_string s)), "limit queries to timestamps after this" ;
        "-stop",  String (fun s -> stop  := Some (Timestamp.of_string s)), "limit queries to timestamps before this" ;
        "-rt-min", String (fun s -> rt_min := Some (Float.of_string s)), "limit queries to resptimes greater than this" ;
        "-url", String (fun s -> url := Some s), "limit queries to those which URL starts with this" ;
        "-host", String (fun s -> host := Some s), "limit queries to those which host ends with this" ;
        "-status", Int (fun i -> status := Some i), "select only queries with this status code" ;
        "-method", Int (fun i -> methd := Some i), "select only queries with this method code" ;
        "-client", String (fun s -> ip_clt := Some (Cidr.of_string s)), "limit to these clients" ;
        "-server", String (fun s -> ip_srv := Some (Cidr.of_string s)), "limit to these servers" ;
        "-peer", String (fun s -> peer := Some (Cidr.of_string s)), "limit to these clients or servers" ]
        (fun x -> raise (Bad x))
        "Operate the HTTP response times DB")

