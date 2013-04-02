open Batteries
open Html

let pages = Hashtbl.create 7
let () =
    Hashtbl.add pages "Traffic" (
        [
            p [ CData "This metric displays informations about exchanged volumes, with no other protocolar \
                       informations than what's readily available at layer 4 (ie. individual packets are \
                       considered in isolation)." ] ;
            p [ CData "This is adapted for bandwidth monitoring, and many ways to observe bandwidth and \
                       network peers are offered." ] ;
            p [ CData "Notice that the 'CallFlow' chart is peculiar as it also makes use of other metrics." ]
        ],
        Traffic.Traffic.fields
    ) ;
    Hashtbl.add pages "DNS" (
        [
            p [ CData "This table stores informations about each DNS queries, associating answers to \
                       queries in order to compute response times." ] ;
            p [ CData "Monitoring DNS queries is important since it's used prevalently in so many other services. \
                       Beyond performance, you should check DNS errors, queried servers, etc..." ] ;
        ],
        Dns.Dns.fields
    ) ;
    Hashtbl.add pages "Web" (
        [
            p [ CData "This table stores informations about each HTTP queries, associating answers to \
                       queries in order to compute response times." ] ;
            p [ CData "Monitoring HTTP queries is important since so many services are based on this protocol." ]
        ],
        Dns.Dns.fields
    ) ;
    Hashtbl.add pages "Config" (
        [
            p [ CData "Here you can change some of the settings. New values will be stored as cookies and will \
                       take precedence over settings from the configuration file." ]
        ],
        []
    )


let field_help (name, desc) =
    let open Datatype in
    tr [
        th [ CData name ] ;
        td [ CData desc.help ] ;
        td (
            if desc.aggrs <> [] then
                List.map fst desc.aggrs |> String.concat ", " |> fun fs -> [ CData fs ]
            else []
        ) ;
        td (
            if desc.sortable <> "" then [ CData "X" ] else []
        ) ;
        td (
            if desc.keyable then [ CData "X" ] else []
        )
    ]

let chart_help chart =
    Block (
        h3 chart.My_ctrl.ChartDescr.title :: chart.My_ctrl.ChartDescr.help
    )

let page name _params =
    let descr, fields =
        try Hashtbl.find pages name
        with Not_found -> [ p [ cdata "No help for page " ; cdata name ] ], [] in
    let fields_help =
        Block (
            if fields <> [] then [
                h2 "Fields" ;
                table (
                    tr [ th [ CData "name" ] ;
                         th [ CData "description" ] ;
                         th [ CData "possible aggregation functions" ] ;
                         th [ CData "can be sorted by" ] ;
                         th [ CData "can be grouped by" ] ] ::
                    List.map field_help fields
                )
            ] else [])
    and charts_help =
        let charts = List.filter (fun c -> c.My_ctrl.ChartDescr.category = name) My_ctrl.chart_descrs in
        Block (
            if charts <> [] then
                h2 "Charts" :: List.map chart_help charts
            else []) in
    [ h1 name ;
      div ~cls:"help" [
        span descr ;
        fields_help ;
        charts_help
    ] ]

