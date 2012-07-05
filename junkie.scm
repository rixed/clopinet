#!/usr/local/bin/junkie -c
; vim:syntax=scheme filetype=scheme expandtab
Or just run: junkie -c this_file
!#

(use-modules ((junkie netmatch nettrack) :renamer (symbol-prefix-proc 'nt:))
			 (junkie runtime))

(define nt-http (nt:compile "http-response-time"
  '(
    [(err-code uint)
     (client-ip ip)
     (server-ip ip)
     (client-mac mac)
     (server-mac mac)
     (eth-vlan uint)
     (client-port uint)
     (server-port uint)
     (qry-start timestamp)
     (qry-stop timestamp)
     (qry-method uint)
     (qry-name str)
     (qry-host str)]
    [(http-answer
       (on-entry (pass "printf(\"WEB\\t%d\\t%s\\t%s\\t%\"PRIuPTR\"\\t%s\\t%s\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\t%s\\t%\"PRIuPTR\"\\t%s\\t%s\\n\",
                        (int)" eth-vlan ",
                        eth_addr_2_str(" client-mac "), ip_addr_2_str(" client-ip "), " client-port ",
                        eth_addr_2_str(" server-mac "), ip_addr_2_str(" server-ip "), " server-port ",
                        " qry-method ", " err-code ", timeval_2_str(" qry-start "), " (timestamp-sub qry-stop qry-start) ", " qry-host ", " qry-name ");\n")))
     (web-qry
       (index-size 1024))]
    ; edges
    [(root web-qry
        (match (cap eth ip tcp http) (do
                                       (client-ip := ip.src)
                                       (client-mac := eth.src)
                                       (client-port := tcp.src-port)
                                       (server-ip := ip.dst)
                                       (server-mac := eth.dst)
                                       (server-port := tcp.dst-port)
                                       (qry-start := cap.ts)
                                       (qry-method := http.method)
                                       (qry-host := http.host)
                                       (qry-name := http.url)
                                       (set? http.method)))
        (dst-index-on () client-port)
        spawn)
     (web-qry http-answer
        (match (cap ip tcp http) (do
                                   (qry-stop := cap.ts)
                                   (err-code := http.status)
                                   (and (ip.src == server-ip)
                                        (ip.dst == client-ip)
                                        (tcp.dst-port == client-port)
                                        (tcp.src-port == server-port)
                                        (set? http.status))))
        (src-index-on (tcp) tcp.dst-port))]))) 

(define nt-dns (nt:compile "dns-response-time"
  '(
    [(err-code uint)
     (client-ip ip)
     (server-ip ip)
     (client-mac mac)
     (server-mac mac)
     (eth-vlan uint)
     (qry-start timestamp)
     (qry-stop timestamp)
     (qry-name str)]
    [(dns-answer
       (on-entry (pass "printf(\"DNS\\t%d\\t%s\\t%s\\t%s\\t%s\\t%\"PRIuPTR\"\\t%s\\t%\"PRIuPTR\"\\t%s\\n\",
                        (int)" eth-vlan ",
                        eth_addr_2_str(" client-mac "), ip_addr_2_str(" client-ip "),
                        eth_addr_2_str(" server-mac "), ip_addr_2_str(" server-ip "),
                        " err-code ", timeval_2_str(" qry-start "), " (timestamp-sub qry-stop qry-start) ", " qry-name ");\n")))
     (dns-query
       (index-size 1024))]
    ; edges
    [(root dns-query
        (match (cap eth ip dns) (do
                                  (client-ip := ip.src)
                                  (client-mac := eth.src)
                                  (server-ip := ip.dst)
                                  (server-mac := eth.dst)
                                  (txid := dns.txid)
                                  (qry-name := dns.name)
                                  (qry-start := cap.ts)
                                  dns.query))
        (dst-index-on () txid)
        spawn)
	 (dns-query dns-answer
        (match (cap ip dns) (do
                              (qry-stop := cap.ts)
                              (err-code := dns.err-code)
                              (and
                                (ip.src == server-ip)
                                (ip.dst == client-ip)
                                (dns.txid == txid))))
        (src-index-on (dns) dns.txid))]))) 

(define nt-eth (nt:compile "eth-traffic"
  '(
    [(client-mac mac)
     (server-mac mac)
     (eth-vlan uint)
     (qry-start timestamp)
     (qry-stop timestamp)
     (eth-proto uint)
     (count uint)
     (eth-pld uint)
     (eth-mtu uint)]
    [(traffic-eth-end
       (on-entry (pass "printf(\"ETH\\t%d\\t%s\\t%s\\t%\"PRIuPTR\"\\t%s\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\n\",
                        (int)" eth-vlan ",
                        eth_addr_2_str(" client-mac "), eth_addr_2_str(" server-mac "), " eth-proto ",
                        timeval_2_str(" qry-start "), " (timestamp-sub qry-stop qry-start) ",
                        " count ", " eth-pld ", " eth-mtu ");\n")))
     (traffic-eth
       (index-size 1024)
       (timeout 0))]
    ; edges
    [(root traffic-eth ; we place this one first so that it's checked last
        (match (cap eth) (do
                           (client-mac := eth.src) ; of course it's not necessary "client" here
                           (server-mac := eth.dst)
                           (eth-vlan := eth.vlan)
                           (eth-proto := eth.protocol)
                           (qry-start := cap.ts)
                           (qry-stop := cap.ts)
                           (eth-pld := eth.payload)
                           (eth-mtu := eth.payload)
                           (count := 1)
                           #t))
        (dst-index-on () (hash client-mac))
        spawn
        grab) ; no need to match traffic-eth->traffic-eth
     (traffic-eth traffic-eth
        (match (cap eth) (do
                           (count := (count + 1))
                           (eth-pld := (eth-pld + eth.payload))
                           (eth-mtu := (max eth-mtu eth.payload))
                           (qry-stop := cap.ts)
                           (and (client-mac == eth.src)
                                (server-mac == eth.dst)
                                (eth-vlan == eth.vlan)
                                (eth-proto == eth.protocol))))
        (dst-index-on () (hash client-mac)) ; an index value must be of type uint!
        (src-index-on (eth) (hash eth.src))
        grab) ; prevent root->traffic-eth
     (traffic-eth traffic-eth-end
        (older 30000000)) ; do not prevent root->traffic-eth edge to match as well (thus reporting this packet payload)
     ]))) 

(nettrack-start nt-http)
(nettrack-start nt-dns)
(nettrack-start nt-eth)

