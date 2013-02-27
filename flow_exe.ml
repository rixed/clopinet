open Batteries
open Datatype
open Flow

let main =
    let dbdir = ref "./" and start = ref None and stop = ref None
    and mac_src = ref None and mac_dst = ref None and vlan = ref None
    and ip_src = ref None and ip_dst = ref None
    and ip_proto = ref None and port_src = ref None and port_dst = ref None
    and create = ref false in
    Arg.(parse [
        "-dir", Set_string dbdir, "database directory (or './')" ;
        "-create", Set create, "create db if it does not exist yet" ;
        "-load", String (fun s -> load !dbdir !create s), "load a CSV file" ;
        "-verbose", Unit (fun () -> verbose := true; Metric.verbose := true), "verbose" ;
        "-c", String Prefs.overwrite_single, "overwrite conf" ;
        "-dump", String (function tbname -> Flow.(iter ?start:!start ?stop:!stop ?ip_proto:!ip_proto
                                                       ?vlan:!vlan ?mac_src:!mac_src ?mac_dst:!mac_dst
                                                       ?ip_src:!ip_src ?ip_dst:!ip_dst
                                                       ?port_src:!port_src ?port_dst:!port_dst
                                                       !dbdir tbname
                                                       (fun x -> write_txt Output.stdout x ; print_newline ()))), "dump this table" ;
        "-dumpf", String (function fname -> Flow.(iter_fname fname
                                                       (fun x -> write_txt Output.stdout x ; print_newline ()))), "dump this file" ;
        "-meta", String (function fname -> Flow.(with_meta fname
                                                       (fun (start, stop) ->
                                                             Printf.printf "%s - %s\n"
                                                                 (Timestamp.to_string start)
                                                                 (Timestamp.to_string stop)))),
                                                         "dump this meta file" ;
        "-dbck",  Unit (fun () -> Metric.dbck !dbdir lods Flow.read Flow.meta_read), "scan the DB and try to repair it" ;
        "-start", String (fun s -> start := Some (Timestamp.of_string s)), "limit queries to timestamps after this" ;
        "-stop",  String (fun s -> stop  := Some (Timestamp.of_string s)), "limit queries to timestamps before this" ;
        "-vlan", String (fun s -> vlan := Some (VLan.of_string s)), "limit queries to this VLAN" ;
        "-ip-proto", String (fun s -> ip_proto := Some (Integer16.of_string s)), "select only queries with this IP protocol" ;
        "-mac-src", String (fun s -> mac_src := Some (EthAddr.of_string s)), "limit to these sources" ;
        "-mac-dst", String (fun s -> mac_dst := Some (EthAddr.of_string s)), "limit to these dests" ;
        "-ip-src", String (fun s -> ip_src := Some (Cidr.of_string s)), "limit to these sources" ;
        "-ip-dst", String (fun s -> ip_dst := Some (Cidr.of_string s)), "limit to these dests" ;
        "-port-src", String (fun s -> port_src := Some (UInteger16.of_string s)), "limit to these source ports" ;
        "-port-dst", String (fun s -> port_dst := Some (UInteger16.of_string s)), "limit to these dest ports" ;
        "-query", String (fun s -> get_callflow (BatOption.get !start) (BatOption.get !stop)
                                          ?vlan:!vlan (InetAddr.of_string s)
                                          ?ip_proto:!ip_proto ?port_src:!port_src ?port_dst:!port_dst !dbdir |>
                                   List.iter (fun (ts1, ts2, ip1, ip2, descr, group, _spec) ->
                                           Printf.printf "%s->%s @ [%s:%s], %s (%s)\n"
                                               ip1 ip2
                                               (Timestamp.to_string ts1) (Timestamp.to_string ts2)
                                               descr group)), "List flows from this IP" ]
        (fun x -> raise (Bad x))
        "Operate the traffic DB")
