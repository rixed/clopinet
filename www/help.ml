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
    ) (*;
    Hashtbl.add pages "DNS" (
        [
            p [ CData "This metric displays informations about all DNS queries, associating answers to\
                       queries in order to compute response times." ]
        ],
        Dns.Dns.fields
    )*)


let field_help (name, desc) =
    let open Metric in
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
        h3 chart.My_ctrl.ChartDescr.title :: chart.My_ctrl.ChartDescr.descr
    )

let page name _params =
    let descr, fields = Hashtbl.find pages name in
    let fields_help =
        List.map field_help fields
    and chart_help =
        List.filter (fun c -> c.My_ctrl.ChartDescr.category = name) My_ctrl.chart_descrs |>
        List.map chart_help in
    [ div ~cls:"help" [
        h2 name ;
        span descr ;
        h3 "Fields" ;
        table (
            tr [ th [ CData "name" ] ;
                 th [ CData "description" ] ;
                 th [ CData "possible aggregation functions" ] ;
                 th [ CData "can be sorted by" ] ;
                 th [ CData "can be grouped by" ] ] ::
            fields_help) ;
        h3 "Charts" ;
        (* TODO: Also repeat this help in the chart page * (not in reports, or in a popup window) *)
        Block chart_help
    ] ]

