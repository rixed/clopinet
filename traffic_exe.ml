open Datatype
open Traffic

let main =
    let start = ref None and stop = ref None
    and mac_src = ref None and mac_dst = ref None and vlan = ref None
    and eth_proto = ref None and ip_src = ref None and ip_dst = ref None
    and ip = ref None and port = ref None
    and ip_proto = ref None and step = ref 60
    and usr_filter = ref None and sort_by = ref "mac_pld"
    and keys = ref [] and aggrs = ref [] and max_graphs = ref 20 in
    Arg.(parse [
        "-load", String (fun s -> load s), "load a CSV file" ;
        "-verbose", Unit (fun () -> verbose := true; Metric.verbose := true), "verbose" ;
        "-step", Set_int step, "time step for plots (default: 60)" ;
        "-sort_by", Set_string sort_by, "(DEBUG) sort_by field" ;
        "-key", String (fun s -> keys := s :: !keys), "(DEBUG) Add a key for top query" ;
        "-field", String (fun s -> aggrs := s :: !aggrs), "(DEBUG) Add a field for top query" ;
        "-max-graphs", Set_int max_graphs, "(DEBUG) Max number of graphs/tops" ;
        "-top", String (function tbname ->
                            get_top ?start:!start ?stop:!stop
                                    ?ip_src:!ip_src ?usr_filter:!usr_filter
                                    ~max_graphs:!max_graphs
                                    !sort_by !keys !aggrs
                                    tbname |> Plot.print_tops), "(DEBUG) run `top` on this table" ;
        "-dump", String (function tbname -> Traffic.(iter ?start:!start ?stop:!stop ?eth_proto:!eth_proto ?ip_proto:!ip_proto
                                                          ?vlan:!vlan ?mac_src:!mac_src ?mac_dst:!mac_dst
                                                          ?ip_src:!ip_src ?ip_dst:!ip_dst ?usr_filter:!usr_filter
                                                          ?ip:!ip ?port:!port
                                                          tbname
                                                          (fun x -> write_txt Output.stdout x ; print_newline ()))), "dump this table" ;
        "-dumpf", String (function fname -> Traffic.(iter_fname fname
                                                          (fun x -> write_txt Output.stdout x ; print_newline ()))), "dump this file" ;
        "-meta", String (function fname -> Traffic.(with_meta fname
                                                          (fun (start, stop) ->
                                                                Printf.printf "%s - %s\n"
                                                                    (Timestamp.to_string start)
                                                                    (Timestamp.to_string stop)))),
                                                            "dump this meta file" ;
        "-dbck", Unit (fun () -> Metric.dbck lods Traffic.read Traffic.meta_read), "scan the DB and try to repair it" ;
        "-purge", Unit (fun () -> Metric.purge "traffic" lods), "purge old datafiles" ;
        "-start", String (fun s -> start := Some (Timestamp.of_string s)), "limit queries to timestamps after this" ;
        "-stop",  String (fun s -> stop  := Some (Timestamp.of_string s)), "limit queries to timestamps before this" ;
        "-vlan", String (fun s -> vlan := Some (VLan.of_string s)), "limit queries to this VLAN" ;
        "-eth-proto", String (fun s -> eth_proto := Some (Integer16.of_string s)), "select only queries with this Eth protocol" ;
        "-ip-proto", String (fun s -> ip_proto := Some (Integer16.of_string s)), "select only queries with this IP protocol" ;
        "-mac-src", String (fun s -> mac_src := Some (EthAddr.of_string s)), "limit to these sources" ;
        "-mac-dst", String (fun s -> mac_dst := Some (EthAddr.of_string s)), "limit to these dests" ;
        "-ip-src", String (fun s -> ip_src := Some (Cidr.of_string s)), "limit to these sources" ;
        "-ip-dst", String (fun s -> ip_dst := Some (Cidr.of_string s)), "limit to these dests" ;
        "-ip", String (fun s -> ip := Some (Cidr.of_string s)), "limit to these IPs" ;
        "-port", String (fun s -> port := Some (UInteger16.of_string s)), "limit to these ports" ;
        "-filter", String (fun s -> usr_filter := Some User_filter.(expression TBool Traffic.fields s)), "Additional filter, as free expression" ]
        (fun x -> raise (Bad x))
        "Operate the traffic DB")
