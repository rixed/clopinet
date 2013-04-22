open Batteries
open Datatype
open Html
open Input.Ops

module InputOfDatatype (D : DATATYPE) :
    TYPE with type t = D.t =
struct
    module String = Input.String (struct let min = Some 1 let max = None end)
    type t = D.t
    let to_edit name getter =
        [ input [ "name", name ;
                  "value", input_text_of name getter ] ]
    let from name getter =
        let s = String.from name getter in
        try D.of_string s
        with End_of_file -> input_error "Not enough data"
           | Overflow    -> input_error "Integer overflow"
           | exn         -> input_error (Printexc.to_string exn)
end

open Input

module StartField = struct
    module Type = Optional (My_time)
    let display_name = "start"
    let uniq_name = "start"
    let persistant = true
end
module StopField = struct
    module Type = Optional (My_time)
    let display_name = "stop"
    let uniq_name = "stop"
    let persistant = true
end
module VlanField = struct
    module Type = Optional (InputOfDatatype (VLan))
    let display_name = "vlan"
    let uniq_name = "vlan"
    let persistant = false
end
module MacSrcField = struct
    module Type = Optional (InputOfDatatype(EthAddr))
    let display_name = "Eth src"
    let uniq_name = "eth-src"
    let persistant = false
end
module MacDstField = struct
    module Type = Optional (InputOfDatatype(EthAddr))
    let display_name = "Eth dest"
    let uniq_name = "eth-dest"
    let persistant = false
end
module EthProtoField = struct
    module Type = Optional (InputOfDatatype(Integer16))
    let display_name = "Eth proto"
    let uniq_name = "eth-proto"
    let persistant = false
end
module IpCltField = struct
    module Type = Optional (InputOfDatatype(Cidr))
    let display_name = "IP clt"
    let uniq_name = "ip-clt"
    let persistant = true
end
module IpSrvField = struct
    module Type = Optional (InputOfDatatype(Cidr))
    let display_name = "IP srv"
    let uniq_name = "ip-srv"
    let persistant = true
end
module IpSrcField = struct
    module Type = Optional (InputOfDatatype(Cidr))
    let display_name = "IP src"
    let uniq_name = "ip-src"
    let persistant = false
end
module IpDstField = struct
    module Type = Optional (InputOfDatatype(Cidr))
    let display_name = "IP dst"
    let uniq_name = "ip-dst"
    let persistant = false
end
module IpField = struct
    module Type = Optional (InputOfDatatype(Cidr))
    let display_name = "IP src/dst"
    let uniq_name = "ip"
    let persistant = false
end
module IpStartField = struct
    module Type = InputOfDatatype(InetAddr)
    let display_name = "Starting IP"
    let uniq_name = "ip-start"
    let persistant = true
end
module IpProtoField = struct
    module Type = Optional (InputOfDatatype(Integer8))
    let display_name = "IP proto"
    let uniq_name = "ip-proto"
    let persistant = false
end
module L4SrcPortField = struct
    module Type = Optional (InputOfDatatype(Integer16))
    let display_name = "src port"
    let uniq_name = "src-port"
    let persistant = true
end
module L4DstPortField = struct
    module Type = Optional (InputOfDatatype(Integer16))
    let display_name = "dst port"
    let uniq_name = "dst-port"
    let persistant = true
end
module L4PortField = struct
    module Type = Optional (InputOfDatatype(Integer16))
    let display_name = "port"
    let uniq_name = "port"
    let persistant = true
end
module TimeStepField = struct
    module Type = Optional (InputOfDatatype (Interval))
    let display_name = "time step"
    let uniq_name = "tstep"
    let persistant = true
end
module GroupByField = struct
    module Type = Enum (struct
        let name = "key"
        let options = [| "port";"src-mac";"dst-mac";"src-ip";"dst-ip" |]
    end)
    let display_name = "group by"
    let uniq_name = "traffic-groupby"
    let persistant = true
end
module GroupByPeerField = struct
    module Type = Enum (struct let name = "key"
                               let options = [| "mac";"ip" |] end)
    let display_name = "group by"
    let uniq_name = "peers-groupby"
    let persistant = true
end
module GroupByGraphField = struct
    module Type = Enum (struct let name = "key"
                               let options = [| "mac+ip";"ip";"mac" |] end)
    let display_name = "show"
    let uniq_name = "graph-groupby"
    let persistant = true
end
module GroupByTopField = struct
    module Type = Enum (struct
        let name = "key"
        let options = [| "port";"src-mac";"dst-mac";"mac (both)";"src-ip";"dst-ip";"ip (both)" |]
    end)
    let display_name = "group by"
    let uniq_name = "top-groupby"
    let persistant = true
end
module PlotWhat = struct
    module Type = Enum (struct let name = "y"
                               let options = [| "volume";"packets" |] end)
    let display_name = "value"
    let uniq_name = "vol-or-count"
    let persistant = true
end
module MaxGraphsField = struct
    module Type = Optional (Integer (struct let min = Some 1 let max = Some 10000 end))
    let display_name = "#series"
    let uniq_name = "series"
    let persistant = true
end
module TxMin = struct
    module Type = Optional (Integer (struct let min = Some 1 let max = None end))
    let display_name = "#tx min"
    let uniq_name = "txmin"
    let persistant = true
end
module MinRespTime = struct
    module Type = Optional (InputOfDatatype (Interval))
    let display_name = "min resp time"
    let uniq_name = "minrt"
    let persistant = true
end
module MaxRespTime = struct
    module Type = Optional (InputOfDatatype (Interval))
    let display_name = "max resp time"
    let uniq_name = "maxrt"
    let persistant = true
end
module SortOrders = struct
    let name = "selection"
    let options = [| "min";"max" |]
end
module SortOrder = struct
    module Type = Enum (SortOrders)
    let display_name = "Selection"
    let uniq_name = "sort-order"
    let persistant = true
end
module DistPrecField = struct
    module Type = Optional (InputOfDatatype (Interval))
    let display_name = "Precision"
    let uniq_name = "distr-prec"
    let persistant = true
end
module SinglePass = struct
    module Type = Optional (Boolean)
    let display_name = "single pass"
    let uniq_name = "single-pass"
    let persistant = true
end

module Traffic = struct
    module TblNames = struct
        let name = "db-tables"
        let options = [| "1min";"10mins";"1hour" |]
    end
    module TblNameField = struct
        module Type = OptEnum (TblNames)
        let display_name = "DB table"
        let uniq_name = "vol-table"
        let persistant = true
    end
    module MinTraffic = struct
        module Type = Optional (InputOfDatatype (ULeast63))
        let display_name = "volume min"
        let uniq_name = "volume-min"
        let persistant = false
    end
    module LayoutType = struct
        let name = "layout"
        let options = [| "neato";"twopi";"circo";"fdp";"sfdp";"dot" |]
    end
    module Layout = struct
        module Type = Enum (LayoutType)
        let display_name = "layout"
        let uniq_name = "layout"
        let persistant = true
    end
    module UsrFilter = struct
        module Type = Optional (Filter_expr.Make (Traffic.Traffic))
        let display_name = "free filter"
        let uniq_name = "usr-filter"
        let persistant = true
    end
    module KeySelector = struct
        module Type = Selector.MakeKey (Traffic.Traffic)
        let display_name = "key"
        let uniq_name = "traffic-tops-key"
        let persistant = true
    end
    module AggrSelector = struct
        module Type = Selector.MakeAggr (Traffic.Traffic)
        let display_name = "fields"
        let uniq_name = "traffic-tops-fields"
        let persistant = true
    end
    module SortSelector = struct
        module Type = Selector.MakeSort (Traffic.Traffic)
        let display_name = "sort by"
        let uniq_name = "traffic-tops-sort"
        let persistant = true
    end

    module Bandwidth = RecordOf (ConsOf (FieldOf (StartField))
                                (ConsOf (FieldOf (StopField))
                                (ConsOf (FieldOf (VlanField))
                                (ConsOf (FieldOf (MacSrcField))
                                (ConsOf (FieldOf (MacDstField))
                                (ConsOf (FieldOf (EthProtoField))
                                (ConsOf (FieldOf (IpSrcField))
                                (ConsOf (FieldOf (IpDstField))
                                (ConsOf (FieldOf (IpField))
                                (ConsOf (FieldOf (IpProtoField))
                                (ConsOf (FieldOf (L4PortField))
                                (ConsOf (FieldOf (UsrFilter))
                                (ConsOf (FieldOf (TimeStepField))
                                (ConsOf (FieldOf (TblNameField))
                                (ConsOf (FieldOf (PlotWhat))
                                (ConsOf (FieldOf (GroupByField))
                                (ConsOf (FieldOf (MaxGraphsField))
                                        (NulType))))))))))))))))))

    module Peers = RecordOf (ConsOf (FieldOf (StartField))
                            (ConsOf (FieldOf (StopField))
                            (ConsOf (FieldOf (VlanField))
                            (ConsOf (FieldOf (MacSrcField))
                            (ConsOf (FieldOf (MacDstField))
                            (ConsOf (FieldOf (EthProtoField))
                            (ConsOf (FieldOf (IpSrcField))
                            (ConsOf (FieldOf (IpDstField))
                            (ConsOf (FieldOf (IpField))
                            (ConsOf (FieldOf (IpProtoField))
                            (ConsOf (FieldOf (L4PortField))
                            (ConsOf (FieldOf (UsrFilter))
                            (ConsOf (FieldOf (TblNameField))
                            (ConsOf (FieldOf (PlotWhat))
                            (ConsOf (FieldOf (GroupByPeerField))
                            (ConsOf (FieldOf (MaxGraphsField))
                                    (NulType)))))))))))))))))

    module Graph = RecordOf (ConsOf (FieldOf (StartField))
                            (ConsOf (FieldOf (StopField))
                            (ConsOf (FieldOf (VlanField))
                            (ConsOf (FieldOf (EthProtoField))
                            (ConsOf (FieldOf (IpProtoField))
                            (ConsOf (FieldOf (L4PortField))
                            (ConsOf (FieldOf (MinTraffic))
                            (ConsOf (FieldOf (UsrFilter))
                            (ConsOf (FieldOf (Layout))
                            (ConsOf (FieldOf (TblNameField))
                            (ConsOf (FieldOf (GroupByGraphField))
                                    (NulType))))))))))))

     module Map = RecordOf (ConsOf (FieldOf (StartField))
                           (ConsOf (FieldOf (StopField))
                           (ConsOf (FieldOf (VlanField))
                           (ConsOf (FieldOf (EthProtoField))
                           (ConsOf (FieldOf (IpProtoField))
                           (ConsOf (FieldOf (L4PortField))
                           (ConsOf (FieldOf (MinTraffic))
                           (ConsOf (FieldOf (UsrFilter))
                           (ConsOf (FieldOf (TblNameField))
                                   (NulType))))))))))

    module Tops = RecordOf (ConsOf (FieldOf (StartField))
                           (ConsOf (FieldOf (StopField))
                           (ConsOf (FieldOf (VlanField))
                           (ConsOf (FieldOf (MacSrcField))
                           (ConsOf (FieldOf (MacDstField))
                           (ConsOf (FieldOf (EthProtoField))
                           (ConsOf (FieldOf (IpSrcField))
                           (ConsOf (FieldOf (IpDstField))
                           (ConsOf (FieldOf (IpField))
                           (ConsOf (FieldOf (IpProtoField))
                           (ConsOf (FieldOf (L4PortField))
                           (ConsOf (FieldOf (UsrFilter))
                           (ConsOf (FieldOf (TblNameField))
                           (ConsOf (FieldOf (KeySelector))
                           (ConsOf (FieldOf (AggrSelector))
                           (ConsOf (FieldOf (SortSelector))
                           (ConsOf (FieldOf (MaxGraphsField))
                           (ConsOf (FieldOf (SinglePass))
                                   (NulType)))))))))))))))))))

end

module Web = struct
    module TblNames = struct
        let name = "web-tables"
        let options = [| "queries";"1min";"10mins";"1hour" |]
    end
    module TblNameField = struct
        module Type = OptEnum (TblNames)
        let display_name = "DB table"
        let uniq_name = "web-table"
        let persistant = true
    end
    module HttpStatus = struct
        module Type = Optional (Integer (struct let min = Some 100 let max = Some 999 end))
        let display_name = "status"
        let uniq_name = "status"
        let persistant = false
    end
    module HttpHost = struct
        module Type = Optional (String (NoLimit))
        let display_name = "host"
        let uniq_name = "host"
        let persistant = false
    end
    module HttpURL = struct
        module Type = Optional (String (NoLimit))
        let display_name = "URL"
        let uniq_name = "url"
        let persistant = false
    end
    module HttpMethod = struct
        module Type = OptEnum (struct let name = "method" let options = Web.http_methods end)
        let display_name = "method"
        let uniq_name = "method"
        let persistant = false
    end
    module UsrFilter = struct
        module Type = Optional (Filter_expr.Make (Web.Web))
        let display_name = "free filter"
        let uniq_name = "usr-filter"
        let persistant = true
    end
    module KeySelector = struct
        module Type = Selector.MakeKey (Web.Web)
        let display_name = "key"
        let uniq_name = "dns-tops-key"
        let persistant = true
    end
    module AggrSelector = struct
        module Type = Selector.MakeAggr (Web.Web)
        let display_name = "fields"
        let uniq_name = "dns-tops-fields"
        let persistant = true
    end
    module SortSelector = struct
        module Type = Selector.MakeSort (Web.Web)
        let display_name = "sort by"
        let uniq_name = "dns-tops-sort"
        let persistant = true
    end

    module RespTime = RecordOf (ConsOf (FieldOf (StartField))
                               (ConsOf (FieldOf (StopField))
                               (ConsOf (FieldOf (VlanField))
                               (ConsOf (FieldOf (MacSrcField))
                               (ConsOf (FieldOf (MacDstField))
                               (ConsOf (FieldOf (IpCltField))
                               (ConsOf (FieldOf (IpSrvField))
                               (ConsOf (FieldOf (HttpMethod))
                               (ConsOf (FieldOf (HttpStatus))
                               (ConsOf (FieldOf (HttpHost))
                               (ConsOf (FieldOf (HttpURL))
                               (ConsOf (FieldOf (MinRespTime))
                               (ConsOf (FieldOf (MaxRespTime))
                               (ConsOf (FieldOf (TimeStepField))
                               (ConsOf (FieldOf (TblNameField))
                                       (NulType))))))))))))))))

    module Queries = RecordOf (ConsOf (FieldOf (StartField))
                              (ConsOf (FieldOf (StopField))
                              (ConsOf (FieldOf (VlanField))
                              (ConsOf (FieldOf (MacSrcField))
                              (ConsOf (FieldOf (MacDstField))
                              (ConsOf (FieldOf (IpCltField))
                              (ConsOf (FieldOf (IpSrvField))
                              (ConsOf (FieldOf (HttpMethod))
                              (ConsOf (FieldOf (HttpStatus))
                              (ConsOf (FieldOf (HttpHost))
                              (ConsOf (FieldOf (HttpURL))
                              (ConsOf (FieldOf (MinRespTime))
                              (ConsOf (FieldOf (MaxRespTime))
                              (ConsOf (FieldOf (MaxGraphsField))
                              (ConsOf (FieldOf (SortOrder))
                                      (NulType))))))))))))))))

    module Distrib = RecordOf (ConsOf (FieldOf (StartField))
                              (ConsOf (FieldOf (StopField))
                              (ConsOf (FieldOf (VlanField))
                              (ConsOf (FieldOf (MacSrcField))
                              (ConsOf (FieldOf (MacDstField))
                              (ConsOf (FieldOf (IpCltField))
                              (ConsOf (FieldOf (IpSrvField))
                              (ConsOf (FieldOf (HttpMethod))
                              (ConsOf (FieldOf (HttpStatus))
                              (ConsOf (FieldOf (HttpHost))
                              (ConsOf (FieldOf (HttpURL))
                              (ConsOf (FieldOf (MinRespTime))
                              (ConsOf (FieldOf (MaxRespTime))
                              (ConsOf (FieldOf (DistPrecField))
                              (ConsOf (FieldOf (MaxGraphsField))
                              (ConsOf (FieldOf (TblNameField))
                                      (NulType)))))))))))))))))

    module Tops = RecordOf (ConsOf (FieldOf (StartField))
                           (ConsOf (FieldOf (StopField))
                           (ConsOf (FieldOf (IpSrvField))
                           (ConsOf (FieldOf (UsrFilter))
                           (ConsOf (FieldOf (TblNameField))
                           (ConsOf (FieldOf (KeySelector))
                           (ConsOf (FieldOf (AggrSelector))
                           (ConsOf (FieldOf (SortSelector))
                           (ConsOf (FieldOf (MaxGraphsField))
                           (ConsOf (FieldOf (SinglePass))
                                   (NulType)))))))))))

end

module Dns = struct
    module TblNames = struct
        let name = "db-tables"
        let options = [| "queries";"1min";"10mins";"1hour" |]
    end
    module TblNameField = struct
        module Type = OptEnum (TblNames)
        let display_name = "DB table"
        let uniq_name = "dns-table"
        let persistant = true
    end
    module Error = struct
        module Type = Optional (Integer (struct let min = Some 0 let max = Some 255 end))
        let display_name = "Error code"
        let uniq_name = "err-code"
        let persistant = false
    end
    module QueryName = struct
        module Type = Optional (String (NoLimit))
        let display_name = "Query Name"
        let uniq_name = "qname"
        let persistant = false
    end
    module UsrFilter = struct
        module Type = Optional (Filter_expr.Make (Dns.Dns))
        let display_name = "free filter"
        let uniq_name = "usr-filter"
        let persistant = true
    end
    module KeySelector = struct
        module Type = Selector.MakeKey (Dns.Dns)
        let display_name = "key"
        let uniq_name = "dns-tops-key"
        let persistant = true
    end
    module AggrSelector = struct
        module Type = Selector.MakeAggr (Dns.Dns)
        let display_name = "fields"
        let uniq_name = "dns-tops-fields"
        let persistant = true
    end
    module SortSelector = struct
        module Type = Selector.MakeSort (Dns.Dns)
        let display_name = "sort by"
        let uniq_name = "dns-tops-sort"
        let persistant = true
    end
    module RespTime = RecordOf (ConsOf (FieldOf (StartField))
                               (ConsOf (FieldOf (StopField))
                               (ConsOf (FieldOf (VlanField))
                               (ConsOf (FieldOf (MacSrcField))
                               (ConsOf (FieldOf (MacDstField))
                               (ConsOf (FieldOf (IpCltField))
                               (ConsOf (FieldOf (IpSrvField))
                               (ConsOf (FieldOf (TxMin))
                               (ConsOf (FieldOf (MinRespTime))
                               (ConsOf (FieldOf (MaxRespTime))
                               (ConsOf (FieldOf (TimeStepField))
                               (ConsOf (FieldOf (TblNameField))
                                       (NulType)))))))))))))

    module Queries = RecordOf (ConsOf (FieldOf (StartField))
                              (ConsOf (FieldOf (StopField))
                              (ConsOf (FieldOf (VlanField))
                              (ConsOf (FieldOf (MacSrcField))
                              (ConsOf (FieldOf (MacDstField))
                              (ConsOf (FieldOf (IpCltField))
                              (ConsOf (FieldOf (IpSrvField))
                              (ConsOf (FieldOf (MinRespTime))
                              (ConsOf (FieldOf (MaxRespTime))
                              (ConsOf (FieldOf (Error))
                              (ConsOf (FieldOf (QueryName))
                              (ConsOf (FieldOf (MaxGraphsField))
                              (ConsOf (FieldOf (SortOrder))
                                      (NulType))))))))))))))

    module Distrib = RecordOf (ConsOf (FieldOf (StartField))
                              (ConsOf (FieldOf (StopField))
                              (ConsOf (FieldOf (VlanField))
                              (ConsOf (FieldOf (MacSrcField))
                              (ConsOf (FieldOf (MacDstField))
                              (ConsOf (FieldOf (IpCltField))
                              (ConsOf (FieldOf (IpSrvField))
                              (ConsOf (FieldOf (MinRespTime))
                              (ConsOf (FieldOf (MaxRespTime))
                              (ConsOf (FieldOf (DistPrecField))
                              (ConsOf (FieldOf (MaxGraphsField))
                              (ConsOf (FieldOf (TblNameField))
                                      (NulType)))))))))))))

    module Tops = RecordOf (ConsOf (FieldOf (StartField))
                           (ConsOf (FieldOf (StopField))
                           (ConsOf (FieldOf (IpSrvField))
                           (ConsOf (FieldOf (UsrFilter))
                           (ConsOf (FieldOf (TblNameField))
                           (ConsOf (FieldOf (KeySelector))
                           (ConsOf (FieldOf (AggrSelector))
                           (ConsOf (FieldOf (SortSelector))
                           (ConsOf (FieldOf (MaxGraphsField))
                           (ConsOf (FieldOf (SinglePass))
                                   (NulType)))))))))))
end

module Flow = struct
    module Callflow = RecordOf (ConsOf (FieldOf (StartField))
                               (ConsOf (FieldOf (StopField))
                               (ConsOf (FieldOf (VlanField))
                               (ConsOf (FieldOf (IpStartField))
                               (ConsOf (FieldOf (IpDstField))
                               (ConsOf (FieldOf (IpProtoField))
                               (ConsOf (FieldOf (L4SrcPortField))
                               (ConsOf (FieldOf (L4DstPortField))
                                       (NulType)))))))))
end

module Config = struct
    module SVGWidth = struct
        module Type = Optional (Float (NoLimit_float))
        let display_name = "graph width"
        let uniq_name = "CPN_GUI_SVG_WIDTH"
        let persistant = true
    end
    module SVGHeight = struct
        module Type = Optional (Float (NoLimit_float))
        let display_name = "graph height"
        let uniq_name = "CPN_GUI_SVG_HEIGHT"
        let persistant = true
    end
    module ResolveIp = struct
        module Type = Optional (Boolean)
        let display_name = "ip as names"
        let uniq_name = "CPN_RESOLVER_IP"
        let persistant = true
    end
    module ResolveMac = struct
        module Type = Optional (Boolean)
        let display_name = "MAC as names"
        let uniq_name = "CPN_RESOLVER_MAC"
        let persistant = true
    end
    module NCores = struct
        module Type = Optional (Integer (struct let min = Some 1 let max = Some 1000 end))
        let display_name = "#CPU cores"
        let uniq_name = "CPN_DB_NB_CORES"
        let persistant = true
    end
    module Preferences = RecordOf (ConsOf (FieldOf (SVGWidth))
                                  (ConsOf (FieldOf (SVGHeight))
                                  (ConsOf (FieldOf (ResolveIp))
                                  (ConsOf (FieldOf (ResolveMac))
                                  (* This one will not be available eventualy *)
                                  (ConsOf (FieldOf (NCores))
                                          (NulType))))))
end
