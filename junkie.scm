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
     (vlan uint)
     (client-port uint)
     (server-port uint)
     (qry-start timestamp)
     (qry-stop timestamp)
     (qry-method uint)
     (url str)
     (qry-host str)]
    [(http-answer
       (on-entry (pass "printf(\"WEB\\t%s\\t%s\\t%s\\t%s\\t%s\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\t%s\\t1\\t%\"PRId64\"\\t%\"PRId64\"\\t%\"PRId64\"\\t0\\t%s\\t%s\\n\",
                        (int)" vlan " == -1 ? \"None\" : tempstr_printf(\"Some %d\", (int)" vlan "),
                        eth_addr_2_str(" client-mac "), ip_addr_2_str(" client-ip "),
                        eth_addr_2_str(" server-mac "), ip_addr_2_str(" server-ip "), " server-port ",
                        " qry-method ", " err-code ", timeval_2_str(" qry-start "),
                        " (timestamp-sub qry-stop qry-start) ", " (timestamp-sub qry-stop qry-start) ", " (timestamp-sub qry-stop qry-start) ",
                        " qry-host ", " url ");\n")))
     (web-qry
       (index-size 1024))]
    ; edges
    [(root web-qry
        (match (cap eth ip tcp http) (if
                                       (set? http.method)
                                       (do
                                         (client-ip := ip.src)
                                         (client-mac := eth.src)
                                         (client-port := tcp.src-port)
                                         (server-ip := ip.dst)
                                         (server-mac := eth.dst)
                                         (server-port := tcp.dst-port)
                                         (qry-start := cap.ts)
                                         (qry-method := http.method)
                                         (qry-host := http.host)
                                         (url := http.url)
                                         #t)))
        (dst-index-on () client-port)
        spawn)
     (web-qry http-answer
        (match (cap ip tcp http) (if
                                   (and (ip.src == server-ip)
                                        (ip.dst == client-ip)
                                        (tcp.dst-port == client-port)
                                        (tcp.src-port == server-port)
                                        (set? http.status)) 
                                   (do
                                     (qry-stop := cap.ts)
                                     (err-code := http.status)
                                     #t)))
        (src-index-on (tcp) tcp.dst-port))]))) 

(define nt-dns (nt:compile "dns-response-time"
  '(
    [(err-code uint)
     (client-ip ip)
     (server-ip ip)
     (client-mac mac)
     (server-mac mac)
     (vlan uint)
     (qry-start timestamp)
     (qry-stop timestamp)
     (qry-name str)]
    [(dns-answer
       (on-entry (pass "printf(\"DNS\\t%s\\t%s\\t%s\\t%s\\t%s\\t%\"PRIuPTR\"\\t%s\\t1\\t%\"PRId64\"\\t%\"PRId64\"\\t%\"PRId64\"\\t0\\t%s\\n\",
                        (int)" vlan " == -1 ? \"None\" : tempstr_printf(\"Some %d\", (int)" vlan "),
                        eth_addr_2_str(" client-mac "), ip_addr_2_str(" client-ip "),
                        eth_addr_2_str(" server-mac "), ip_addr_2_str(" server-ip "),
                        " err-code ", timeval_2_str(" qry-start "),
                        " (timestamp-sub qry-stop qry-start) ", " (timestamp-sub qry-stop qry-start) ", " (timestamp-sub qry-stop qry-start) ",
                        " qry-name ");\n")))
     (dns-query
       (index-size 1024))]
    ; edges
    [(root dns-query
        (match (cap eth ip dns) (if
                                  dns.query
                                  (do
                                    (client-ip := ip.src)
                                    (client-mac := eth.src)
                                    (server-ip := ip.dst)
                                    (server-mac := eth.dst)
                                    (txid := dns.txid)
                                    (qry-name := dns.name)
                                    (qry-start := cap.ts)
                                    #t)))
        (dst-index-on () txid)
        spawn)
	 (dns-query dns-answer
        (match (cap ip dns) (if
                              (and
                                (ip.src == server-ip)
                                (ip.dst == client-ip)
                                (dns.txid == txid))
                              (do
                                (qry-stop := cap.ts)
                                (err-code := dns.err-code)
                                #t)))
        (src-index-on (dns) dns.txid))]))) 

(define nt-eth (nt:compile "eth-traffic"
  '([(vlan uint)
     (eth-src mac)
     (eth-dst mac)
     (eth-proto uint)
     (eth-pld uint)
     (ip-src ip)
     (ip-dst ip)
     (ip-proto uint)
     (ip-pld uint)
     (port-src uint)
     (port-dst uint)
     (l4-pld uint)
     (ts-start timestamp)
     (ts-stop timestamp)
     (count uint)
     (eth-mtu uint)]
    [(traffic-eth-end
       (on-entry (pass "printf(\"TRF\\t%s\\t%s\\t%\"PRIuPTR\"\\t%s\\t%s\\t%s\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\t0.0.0.0\\t0.0.0.0\\t0\\t0\\t0\\t0\\t0\\n\",
                        timeval_2_str(" ts-start "), timeval_2_str(" ts-stop "), " count ",
                        (int)" vlan " == -1 ? \"None\" : tempstr_printf(\"Some %d\", (int)" vlan "),
                        eth_addr_2_str(" eth-src "), eth_addr_2_str(" eth-dst "), " eth-proto ",
                        " eth-pld ", " eth-mtu ");\n")))
     (traffic-eth
       (index-size 1024)
       (timeout 0))
     (traffic-ip-end
       (on-entry (pass "printf(\"TRF\\t%s\\t%s\\t%\"PRIuPTR\"\\t%s\\t%s\\t%s\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\t%s\\t%s\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\t0\\t0\\t0\\n\",
                        timeval_2_str(" ts-start "), timeval_2_str(" ts-stop "), " count ",
                        (int)" vlan " == -1 ? \"None\" : tempstr_printf(\"Some %d\", (int)" vlan "),
                        eth_addr_2_str(" eth-src "), eth_addr_2_str(" eth-dst "), " eth-proto ",
                        " eth-pld ", " eth-mtu ",
                        ip_addr_2_str(" ip-src "), ip_addr_2_str(" ip-dst "), " ip-proto ", " ip-pld ");\n")))
     (traffic-ip
       (index-size 1024)
       (timeout 0))
     (traffic-l4-end
       (on-entry (pass "printf(\"TRF\\t%s\\t%s\\t%\"PRIuPTR\"\\t%s\\t%s\\t%s\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\t%s\\t%s\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\n\",
                        timeval_2_str(" ts-start "), timeval_2_str(" ts-stop "), " count ",
                        (int)" vlan " == -1 ? \"None\" : tempstr_printf(\"Some %d\", (int)" vlan "),
                        eth_addr_2_str(" eth-src "), eth_addr_2_str(" eth-dst "), " eth-proto ",
                        " eth-pld ", " eth-mtu ",
                        ip_addr_2_str(" ip-src "), ip_addr_2_str(" ip-dst "), " ip-proto ", " ip-pld ",
                        " port-src ", " port-dst ", " l4-pld ");\n")))
     (traffic-tcp
       (index-size 8192)
       (timeout 0))
     (traffic-udp
       (index-size 8192)
       (timeout 0))]
    ; edges
    [; ETH
     (root traffic-eth ; we place this one first so that it's checked last
        (on full-parse)
        (match (cap eth) (do
                           (eth-src := eth.src)
                           (eth-dst := eth.dst)
                           (vlan := eth.vlan)
                           (eth-proto := eth.protocol)
                           (ts-start := cap.ts)
                           (ts-stop := cap.ts)
                           (eth-pld := eth.payload)
                           (eth-mtu := eth.payload)
                           (count := 1)
                           #t))
        (dst-index-on () (hash eth-src))
        spawn
        grab) ; no need to match traffic-eth->traffic-eth
     (traffic-eth traffic-eth
        (on full-parse)
        (match (cap eth) (if
                           (and (eth-src == eth.src)
                                (eth-dst == eth.dst)
                                (vlan == eth.vlan)
                                (eth-proto == eth.protocol))
                           (do
                             (count := (count + 1))
                             (eth-pld := (eth-pld + eth.payload))
                             (eth-mtu := (max eth-mtu eth.payload))
                             (ts-stop := cap.ts)
                             #t)))
        (dst-index-on () (hash eth-src)) ; an index value must be of type uint!
        (src-index-on (eth) (hash eth.src))
        grab) ; prevent root->traffic-eth
     (traffic-eth traffic-eth-end
        (on full-parse)
        (older 30000000)) ; do not prevent root->traffic-eth edge to match as well (thus reporting this packet payload)
     ; other IP
     (root traffic-ip ; for other ip traffic
        (on full-parse)
        (match (cap eth ip) (do
                              (eth-src := eth.src)
                              (eth-dst := eth.dst)
                              (vlan := eth.vlan)
                              (eth-proto := eth.protocol)
                              (ts-start := cap.ts)
                              (ts-stop := cap.ts)
                              (eth-pld := eth.payload)
                              (eth-mtu := eth.payload)
                              (ip-pld := ip.payload)
                              (ip-src := ip.src)
                              (ip-dst := ip.dst)
                              (ip-proto := ip.proto)
                              (count := 1)
                              #t))
        (dst-index-on () (hash ip-src))
        spawn
        grab)
     (traffic-ip traffic-ip
        (on full-parse)
        (match (cap eth ip) (if
                              (and (eth-src == eth.src)
                                   (eth-dst == eth.dst)
                                   (vlan == eth.vlan)
                                   (eth-proto == eth.protocol)
                                   (ip-src == ip.src)
                                   (ip-dst == ip.dst)
                                   (ip-proto == ip.proto))
                              (do
                                (count := (count + 1))
                                (eth-pld := (eth-pld + eth.payload))
                                (eth-mtu := (max eth-mtu eth.payload))
                                (ts-stop := cap.ts)
                                (ip-pld := (ip-pld + ip.payload))
                                #t)))
        (dst-index-on () (hash ip-src))
        (src-index-on (ip) (hash ip.src))
        grab)
     (traffic-ip traffic-ip-end
        (on full-parse)
        (older 30000000))
     ; TCP
     (root traffic-tcp
        (on full-parse)
        (match (cap eth ip tcp) (do
                                  (eth-src := eth.src)
                                  (eth-dst := eth.dst)
                                  (vlan := eth.vlan)
                                  (eth-proto := eth.protocol)
                                  (ts-start := cap.ts)
                                  (ts-stop := cap.ts)
                                  (eth-pld := eth.payload)
                                  (eth-mtu := eth.payload)
                                  (ip-pld := ip.payload)
                                  (ip-src := ip.src)
                                  (ip-dst := ip.dst)
                                  (ip-proto := ip.proto)
                                  (port-src := tcp.src-port)
                                  (port-dst := tcp.dst-port)
                                  (l4-pld := tcp.payload)
                                  (count := 1)
                                  #t))
        (dst-index-on () (hash ip-src))
        spawn
        grab)
     (traffic-tcp traffic-tcp
        (on full-parse)
        (match (cap eth ip tcp) (if
                                  (and (eth-src == eth.src)
                                       (eth-dst == eth.dst)
                                       (vlan == eth.vlan)
                                       (eth-proto == eth.protocol)
                                       (ip-src == ip.src)
                                       (ip-dst == ip.dst)
                                       (ip-proto == ip.proto)
                                       (port-src == tcp.src-port)
                                       (port-dst == tcp.dst-port))
                                  (do
                                    (count := (count + 1))
                                    (eth-pld := (eth-pld + eth.payload))
                                    (eth-mtu := (max eth-mtu eth.payload))
                                    (ts-stop := cap.ts)
                                    (ip-pld := (ip-pld + ip.payload))
                                    (l4-pld := (l4-pld + tcp.payload))
                                    #t)))
        (dst-index-on () (hash ip-src))
        (src-index-on (ip) (hash ip.src))
        grab)
     (traffic-tcp traffic-l4-end
        (on full-parse)
        (older 30000000))
     ; UDP
     (root traffic-udp
        (on full-parse)
        (match (cap eth ip udp) (do
                                  (eth-src := eth.src)
                                  (eth-dst := eth.dst)
                                  (vlan := eth.vlan)
                                  (eth-proto := eth.protocol)
                                  (ts-start := cap.ts)
                                  (ts-stop := cap.ts)
                                  (eth-pld := eth.payload)
                                  (eth-mtu := eth.payload)
                                  (ip-pld := ip.payload)
                                  (ip-src := ip.src)
                                  (ip-dst := ip.dst)
                                  (ip-proto := ip.proto)
                                  (port-src := udp.src-port)
                                  (port-dst := udp.dst-port)
                                  (l4-pld := udp.payload)
                                  (count := 1)
                                  #t))
        (dst-index-on () (hash ip-src))
        spawn
        grab)
     (traffic-udp traffic-udp
        (on full-parse)
        (match (cap eth ip udp) (if
                                  (and (eth-src == eth.src)
                                       (eth-dst == eth.dst)
                                       (vlan == eth.vlan)
                                       (eth-proto == eth.protocol)
                                       (ip-src == ip.src)
                                       (ip-dst == ip.dst)
                                       (ip-proto == ip.proto)
                                       (port-src == udp.src-port)
                                       (port-dst == udp.dst-port))
                                  (do
                                    (count := (count + 1))
                                    (eth-pld := (eth-pld + eth.payload))
                                    (eth-mtu := (max eth-mtu eth.payload))
                                    (ts-stop := cap.ts)
                                    (ip-pld := (ip-pld + ip.payload))
                                    (l4-pld := (l4-pld + udp.payload))
                                    #t)))
        (dst-index-on () (hash ip-src))
        (src-index-on (ip) (hash ip.src))
        grab)
     (traffic-udp traffic-l4-end
        (on full-parse)
        (older 30000000))]))) 

(nettrack-start nt-http)
(nettrack-start nt-dns)
(nettrack-start nt-eth)

