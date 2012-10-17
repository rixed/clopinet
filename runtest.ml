open Traffic
open Batteries

let main =
    let dbdir = ref "./" (*and start = ref None and stop = ref None *) in
    Arg.(parse [
        "-dir", Set_string dbdir, "database directory (or './')" ;
(*        "-start", String (fun s -> start := Some (Timestamp.of_string s)), "limit queries to timestamps after this" ;
        "-stop",  String (fun s -> stop  := Some (Timestamp.of_string s)), "limit queries to timestamps before this" ;*)
        "-iter", String (fun tbl ->
            Traffic.(iter !dbdir tbl ignore)), "scan this table" ;
        "-batread_i64", String (fun f ->
            let ic = File.open_in f in
            try forever IO.read_i64 ic
            with _ -> ignore (IO.close_in ic)), "read a whole file as int64s using batteries" ;
        "-deser_i64", String (fun f ->
            let open Serial in
            let ic = make_ibuf f in
            try forever deser64 ic
            with _ -> close_ibuf ic), "read a while file as int64s using deser" ]
        (fun x -> raise (Bad x))
        "Run some perf test")

