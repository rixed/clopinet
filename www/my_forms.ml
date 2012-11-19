open Batteries
open Datatype
open Html
open Input.Ops

module InputOfDatatype (D : DATATYPE) :
    TYPE with type t = D.t =
struct
    module String = Input.String (struct let min = Some 1 let max = None end)
    type t = D.t
    let name = D.name
    let to_html v = [ cdata (html_of_user_value D.to_string v) ]
    let edit name v =
        [ input [ "name", name ;
                  "value", input_of_user_value D.to_string v ] ] @
        err_msg_of v
    let from_args name args =
        match String.from_args name args with
        | Error _ as x -> x
        | Value s  ->
            (try Value (D.of_string s)
            with End_of_file -> Error ("Not enough data", s)
               | Overflow    -> Error ("Integer overflow", s)
               | exn         -> Error (Printexc.to_string exn, s))
end

module OptInputOfDatatype (D : DATATYPE) :
    TYPE with type t = D.t option =
struct
    module String = Input.OptString (struct let min = Some 0 let max = None end)
    type t = D.t option
    let name = D.name
    let to_html v = [ cdata (html_of_user_value (function None -> "<i>unset</i>"
                                                        | Some s -> D.to_string s) v) ]
    let edit name v =
        [ input [ "name", name ;
                  "value", input_of_user_value (function None -> ""
                                                       | Some s -> D.to_string s) v ] ] @
        err_msg_of v
    let from_args name args =
        match String.from_args name args with
        | Error _ | Value None as x -> x
        | Value (Some s) -> 
            (try Value (Some (D.of_string s))
            with End_of_file -> Error ("Not enough data", s)
               | Overflow    -> Error ("Integer overflow", s)
               | exn         -> Error (Printexc.to_string exn, s))
end

open Input

module NoLimit = struct
    let min = None
    let max = None
end

module LoginField = struct
    module Type = Input.String (struct let min = Some 1 let max = Some 100 end)
    let display_name = "name"
    let uniq_name = "name"
    let persistant = false
end
module PasswdField = struct
    module Type = Input.Password (struct let min = Some 3 let max = Some 100 end)
    let display_name = "password"
    let uniq_name = "password"
    let persistant = false
end

module Login = RecordOf (ConsOf (FieldOf (LoginField))
                        (ConsOf (FieldOf (PasswdField))
                                (NulType)))

module StartField = struct
    module Type = My_time.Mandatory
    let display_name = "start"
    let uniq_name = "start"
    let persistant = true
end
module StopField = struct
    module Type = My_time.Mandatory
    let display_name = "stop"
    let uniq_name = "stop"
    let persistant = true
end
module VlanField = struct
    module Type = OptInputOfDatatype(Integer16)
    let display_name = "vlan"
    let uniq_name = "vlan"
    let persistant = false
end
module MacSrcField = struct
    module Type = OptInputOfDatatype(EthAddr)
    let display_name = "Eth src"
    let uniq_name = "eth-src"
    let persistant = false
end
module MacDstField = struct
    module Type = OptInputOfDatatype(EthAddr)
    let display_name = "Eth dest"
    let uniq_name = "eth-dest"
    let persistant = false
end
module EthProtoField = struct
    module Type = OptInputOfDatatype(Integer16)
    let display_name = "Eth proto"
    let uniq_name = "eth-proto"
    let persistant = false
end
module IpSrcField = struct
    module Type = OptInputOfDatatype(Cidr)
    let display_name = "IP src"
    let uniq_name = "ip-src"
    let persistant = false
end
module IpDstField = struct
    module Type = OptInputOfDatatype(Cidr)
    let display_name = "IP dst"
    let uniq_name = "ip-dst"
    let persistant = false
end
module IpField = struct
    module Type = OptInputOfDatatype(Cidr)
    let display_name = "IP src/dst"
    let uniq_name = "ip"
    let persistant = false
end
module IpProtoField = struct
    module Type = OptInputOfDatatype(Integer8)
    let display_name = "IP proto"
    let uniq_name = "ip-proto"
    let persistant = false
end
module L4PortField = struct
    module Type = OptInputOfDatatype(Integer16)
    let display_name = "port"
    let uniq_name = "port"
    let persistant = true
end
module TimeStepField = struct
    module Type = InputOfDatatype (Interval)
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
    let uniq_name = "groupby"
    let persistant = false
end
module GroupByPeerField = struct
    module Type = Enum (struct let name = "key"
                               let options = [| "mac";"ip" |] end)
    let display_name = "group by"
    let uniq_name = "groupby"
    let persistant = false
end
module GroupByGraphField = struct
    module Type = Enum (struct let name = "key"
                               let options = [| "mac+ip";"ip";"mac" |] end)
    let display_name = "show"
    let uniq_name = "show_in_network"
    let persistant = false
end
module GroupByTopField = struct
    module Type = Enum (struct
        let name = "key"
        let options = [| "port";"src-mac";"dst-mac";"mac (both)";"src-ip";"dst-ip";"ip (both)" |]
    end)
    let display_name = "group by"
    let uniq_name = "groupby"
    let persistant = false
end
module PlotWhat = struct
    module Type = Enum (struct let name = "y"
                               let options = [| "volume";"packets" |] end)
    let display_name = "value"
    let uniq_name = "Y"
    let persistant = false
end
module MaxGraphsField = struct
    module Type = OptInteger (struct let min = Some 1 let max = Some 10000 end)
    let display_name = "#series"
    let uniq_name = "series"
    let persistant = false
end
module TxMin = struct
    module Type = OptInteger (struct let min = Some 1 let max = None end)
    let display_name = "#tx min"
    let uniq_name = "txmin"
    let persistant = false
end
module MinRespTime = struct
    module Type = OptFloat (struct let min = Some 0. let max = None end)
    let display_name = "min resp time (s)"
    let uniq_name = "minrt"
    let persistant = false
end
module MaxRespTime = struct
    module Type = OptFloat (struct let min = Some 0. let max = None end)
    let display_name = "max resp time (s)"
    let uniq_name = "maxrt"
    let persistant = false
end
module SortOrders = struct
    let name = "selection"
    let options = [| "min";"max" |]
end
module SortOrder = struct
    module Type = Enum (SortOrders)
    let display_name = "Selection"
    let uniq_name = "sort-order"
    let persistant = false
end

module Traffic = struct
    module TblNames = struct
        let name = "db-tables"
        let options = [| "1min";"10mins";"1hour" |]
    end
    module TblNameField = struct
        module Type = Enum (TblNames)
        let display_name = "DB table"
        let uniq_name = "vol-table"
        let persistant = true
    end
    module MinTraffic = struct
        module Type = OptInteger (NoLimit)
        let display_name = "volume min"
        let uniq_name = "volume_min"
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
                                (ConsOf (FieldOf (TimeStepField))
                                (ConsOf (FieldOf (TblNameField))
                                (ConsOf (FieldOf (PlotWhat))
                                (ConsOf (FieldOf (GroupByField))
                                (ConsOf (FieldOf (MaxGraphsField))
                                        (NulType)))))))))))))))))

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
                            (ConsOf (FieldOf (TblNameField))
                            (ConsOf (FieldOf (PlotWhat))
                            (ConsOf (FieldOf (GroupByPeerField))
                            (ConsOf (FieldOf (MaxGraphsField))
                                    (NulType))))))))))))))))

    module Graph = RecordOf (ConsOf (FieldOf (StartField))
                            (ConsOf (FieldOf (StopField))
                            (ConsOf (FieldOf (VlanField))
                            (ConsOf (FieldOf (EthProtoField))
                            (ConsOf (FieldOf (IpProtoField))
                            (ConsOf (FieldOf (L4PortField))
                            (ConsOf (FieldOf (MinTraffic))
                            (ConsOf (FieldOf (Layout))
                            (ConsOf (FieldOf (TblNameField))
                            (ConsOf (FieldOf (GroupByGraphField))
                                    (NulType)))))))))))

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
                           (ConsOf (FieldOf (TblNameField))
                           (ConsOf (FieldOf (PlotWhat))
                           (ConsOf (FieldOf (GroupByTopField))
                           (ConsOf (FieldOf (MaxGraphsField))
                                   (NulType))))))))))))))))

end

module Web = struct
    module TblNames = struct
        let name = "web-tables"
        let options = [| "queries";"1min";"10mins";"1hour" |]
    end
    module TblNameField = struct
        module Type = Enum (TblNames)
        let display_name = "DB table"
        let uniq_name = "web-table"
        let persistant = true
    end
    module HttpStatus = struct
        module Type = OptInteger (struct let min = Some 100 let max = Some 999 end)
        let display_name = "status"
        let uniq_name = "status"
        let persistant = false
    end
    module HttpHost = struct
        module Type = OptString (NoLimit)
        let display_name = "host"
        let uniq_name = "host"
        let persistant = false
    end
    module HttpURL = struct
        module Type = OptString (NoLimit)
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

    (* TODO: Add HttpMethod *)
    module RespTime = RecordOf (ConsOf (FieldOf (StartField))
                               (ConsOf (FieldOf (StopField))
                               (ConsOf (FieldOf (VlanField))
                               (ConsOf (FieldOf (MacSrcField))
                               (ConsOf (FieldOf (MacDstField))
                               (ConsOf (FieldOf (IpSrcField))
                               (ConsOf (FieldOf (IpDstField))
                               (ConsOf (FieldOf (HttpMethod))
                               (ConsOf (FieldOf (HttpStatus))
                               (ConsOf (FieldOf (HttpHost))
                               (ConsOf (FieldOf (HttpURL))
                               (ConsOf (FieldOf (MinRespTime))
                               (ConsOf (FieldOf (MaxRespTime))
                               (ConsOf (FieldOf (TimeStepField))
                               (ConsOf (FieldOf (TblNameField))
                                       (NulType))))))))))))))))

    (* TODO: Add HttpMethod *)
    module Top = RecordOf (ConsOf (FieldOf (StartField))
                          (ConsOf (FieldOf (StopField))
                          (ConsOf (FieldOf (VlanField))
                          (ConsOf (FieldOf (MacSrcField))
                          (ConsOf (FieldOf (MacDstField))
                          (ConsOf (FieldOf (IpSrcField))
                          (ConsOf (FieldOf (IpDstField))
                          (ConsOf (FieldOf (HttpMethod))
                          (ConsOf (FieldOf (HttpStatus))
                          (ConsOf (FieldOf (HttpHost))
                          (ConsOf (FieldOf (HttpURL))
                          (ConsOf (FieldOf (MinRespTime))
                          (ConsOf (FieldOf (MaxRespTime))
                          (ConsOf (FieldOf (MaxGraphsField))
                          (ConsOf (FieldOf (SortOrder))
                                  (NulType))))))))))))))))

end

module Dns = struct
    module TblNames = struct
        let name = "db-tables"
        let options = [| "queries";"1min";"10mins";"1hour" |]
    end
    module TblNameField = struct
        module Type = Enum (TblNames)
        let display_name = "DB table"
        let uniq_name = "dns-table"
        let persistant = true
    end
    module Error = struct
        module Type = OptInteger (struct let min = Some 0 let max = Some 255 end)
        let display_name = "Error code"
        let uniq_name = "err-code"
        let persistant = false
    end
    module QueryName = struct
        module Type = OptString (NoLimit)
        let display_name = "Query Name"
        let uniq_name = "qname"
        let persistant = false
    end
    module RespTime = RecordOf (ConsOf (FieldOf (StartField))
                               (ConsOf (FieldOf (StopField))
                               (ConsOf (FieldOf (VlanField))
                               (ConsOf (FieldOf (MacSrcField))
                               (ConsOf (FieldOf (MacDstField))
                               (ConsOf (FieldOf (IpSrcField))
                               (ConsOf (FieldOf (IpDstField))
                               (ConsOf (FieldOf (TxMin))
                               (ConsOf (FieldOf (MinRespTime))
                               (ConsOf (FieldOf (MaxRespTime))
                               (ConsOf (FieldOf (TimeStepField))
                               (ConsOf (FieldOf (TblNameField))
                                       (NulType)))))))))))))

    module Top = RecordOf (ConsOf (FieldOf (StartField))
                          (ConsOf (FieldOf (StopField))
                          (ConsOf (FieldOf (VlanField))
                          (ConsOf (FieldOf (MacSrcField))
                          (ConsOf (FieldOf (MacDstField))
                          (ConsOf (FieldOf (IpSrcField))
                          (ConsOf (FieldOf (IpDstField))
                          (ConsOf (FieldOf (MinRespTime))
                          (ConsOf (FieldOf (MaxRespTime))
                          (ConsOf (FieldOf (Error))
                          (ConsOf (FieldOf (QueryName))
                          (ConsOf (FieldOf (MaxGraphsField))
                          (ConsOf (FieldOf (SortOrder))
                                  (NulType))))))))))))))
end
