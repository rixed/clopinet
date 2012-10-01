open Bricabrac
open Datatype
open Traffic

let main =
    let dbdir = ref "./" and start = ref None and stop = ref None 
    and mac_src = ref None and mac_dst = ref None and vlan = ref None
    and eth_proto = ref None and ip_proto = ref None
    and create = ref false and step = ref 60 in
    Arg.(parse [
        "-dir", Set_string dbdir, "database directory (or './')" ;
        "-create", Set create, "create db if it does not exist yet" ;
        "-load", String (fun s -> load !dbdir !create s), "load a CSV file" ;
        "-verbose", Set verbose, "output some msgs on stderr" ;
        "-j", Set_int Table.ncores, "number of cores (default: 1)" ;
        "-step", Set_int step, "time step for plots (default: 60)" ;
        "-dump", String (function tbname -> Traffic.(iter ?start:!start ?stop:!stop ?eth_proto:!eth_proto ?ip_proto:!ip_proto
                                                          ?vlan:!vlan ?mac_src:!mac_src ?mac_dst:!mac_dst !dbdir tbname
                                                          (fun x -> write_txt Output.stdout x ; print_newline ()))), "dump this table" ;
        "-plot", String (function tbname -> ip_plot_vol_time ?start:!start ?stop:!stop ?eth_proto:!eth_proto ?ip_proto:!ip_proto
                                                             ?vlan:!vlan ?mac_src:!mac_src ?mac_dst:!mac_dst !step !dbdir tbname |> Plot.stacked_area), "plot this table" ;
        "-plot2", String (function tbname -> eth_plot_vol_time ?start:!start ?stop:!stop ?eth_proto:!eth_proto ?ip_proto:!ip_proto
                                                               ?vlan:!vlan ?mac_src:!mac_src ?mac_dst:!mac_dst !step !dbdir tbname |> Plot.stacked_area), "plot this table" ;
        "-plot3", String (function tbname -> app_plot_vol_time ?start:!start ?stop:!stop ?eth_proto:!eth_proto ?ip_proto:!ip_proto
                                                               ?vlan:!vlan ?mac_src:!mac_src ?mac_dst:!mac_dst !step !dbdir tbname |> Plot.stacked_area), "plot this table" ;
        "-start", String (fun s -> start := Some (Timestamp.of_string s)), "limit queries to timestamps after this" ;
        "-stop",  String (fun s -> stop  := Some (Timestamp.of_string s)), "limit queries to timestamps before this" ;
        "-vlan", String (fun s -> vlan := Some (Integer16.of_string s)), "limit queries to this VLAN" ;
        "-eth-proto", String (fun s -> eth_proto := Some (Integer16.of_string s)), "select only queries with this Eth protocol" ;
        "-ip-proto", String (fun s -> ip_proto := Some (Integer16.of_string s)), "select only queries with this IP protocol" ;
        "-mac-src", String (fun s -> mac_src := Some (EthAddr.of_string s)), "limit to these sources" ;
        "-mac-dst", String (fun s -> mac_dst := Some (EthAddr.of_string s)), "limit to these dests" ]
        (fun x -> raise (Bad x))
        "Operate the traffic DB")
