#!/usr/local/bin/junkie -c
; vim:syntax=scheme filetype=scheme expandtab
Or just run: junkie -c this_file
!#

(use-modules ((junkie netmatch nettrack) :renamer (symbol-prefix-proc 'nt:))
			 (junkie runtime))

(define nt (nt:compile "http-response-time"
  '(
    [(err-code uint)
     (client-ip ip)
     (server-ip ip)
     (client-mac mac)
     (server-mac mac)
     (client-port uint)
     (qry-ts timestamp)
     (duration uint)
     (qry-method uint)
     (qry-name str)
     (qry-host str)]
    [(http-answer
       (on-entry (pass "printf(\"WEB\\t%s\\t%s\\t%s\\t%s\\t%\"PRIuPTR\"\\t%\"PRIuPTR\"\\t%s\\t%\"PRIuPTR\"\\t%s\\t%s\\n\",
                        eth_addr_2_str(" client-mac "), ip_addr_2_str(" client-ip "),
                        eth_addr_2_str(" server-mac "), ip_addr_2_str(" server-ip "),
                        " qry-method ", " err-code ", timeval_2_str(" qry-ts "), " duration ", " qry-host ", " qry-name ");\n")))
     (web-syn
       (index-size 1024))
     (dns-answer
       (on-entry (pass "printf(\"DNS\\t%s\\t%s\\t%s\\t%s\\t%\"PRIuPTR\"\\t%s\\t%\"PRIuPTR\"\\t%s\\n\",
                        eth_addr_2_str(" client-mac "), ip_addr_2_str(" client-ip "),
                        eth_addr_2_str(" server-mac "), ip_addr_2_str(" server-ip "),
                        " err-code ", timeval_2_str(" qry-ts "), " duration ", " qry-name ");\n")))
     (dns-query
       (index-size 1024))]
    ; edges
    [(root web-qry
              (match (cap eth ip tcp http) (do
                                         (client-ip := ip.src)
                                         (client-mac := eth.src)
                                         (server-ip := ip.dst)
                                         (server-mac := eth.dst)
                                         (client-port := tcp.src-port)
                                         (qry-ts := cap.ts)
                                         (qry-method := http.method)
                                         (qry-host := http.host)
                                         (qry-name := http.url)
                                         (set? http.method)))
              (dst-index-on () client-port)
              spawn)
     (web-qry http-answer
              (match (cap ip tcp http) (do
                                         (duration := (timestamp-sub cap.ts qry-ts))
                                         (err-code := http.status)
                                         (and (ip.src == server-ip)
                                              (ip.dst == client-ip)
                                              (tcp.dst-port == client-port)
                                              (set? http.status))))
              (src-index-on (tcp) tcp.dst-port))
     (root dns-query
           (match (cap eth ip dns) (do
                                 (client-ip := ip.src)
                                 (client-mac := eth.src)
                                 (server-ip := ip.dst)
                                 (server-mac := eth.dst)
                                 (txid := dns.txid)
                                 (qry-name := dns.name)
                                 (qry-ts := cap.ts)
                                 (eth.src == #{b4:a4:e3:4d:5c:01}#)))
           (dst-index-on () txid)
           spawn)
	 (dns-query dns-answer
                (match (cap ip dns) (do
                                      (duration := (timestamp-sub cap.ts qry-ts))
                                      (err-code := dns.err-code)
                                      (and
                                        (ip.src == server-ip)
                                        (ip.dst == client-ip)
                                        (dns.txid == txid))))
                (src-index-on (dns) dns.txid) ; note that one CAN NOT use any register in a src-index-on expression (since it's used precisely to find the regfile)
	   ; TODO: kill parent
       )]))) 

(nettrack-start nt)

