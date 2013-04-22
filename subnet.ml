(* Classify IP addresses into subnets.
 * Used by all metrics. *)

module SL = Datatype.ListOf (Datatype.Cidr)

let subnets =
    (* don't crash here if syntax is not clean since it's in module initialization code (we may not need subnets) *)
    try SL.of_pref "CPN_SUBNETS" []
    with Peg.Parse_error err ->
        Printf.fprintf stderr "Cannot parse subnets: %s. Continuing without subnets..." (Peg.string_of_error err) ;
        []

