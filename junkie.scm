#!/usr/local/bin/junkie -c
; vim:syntax=scheme filetype=scheme expandtab
Or just run: junkie -c this_file
!#

(define-module (clopinet))
(use-modules ((junkie netmatch nettrack) :renamer (symbol-prefix-proc 'nt:))
             (junkie defs)
             (junkie tools)
             (junkie runtime)
             (ice-9 threads))

(set-log-file (string-append (getenv "CPN_LOG_DIR") "/junkie.log"))
;(set-log-level log-debug)
;(set-log-level log-err "mutex")
;(set-log-level log-err "proto")

; find all the FIFOs in which we are supposed to write
(define fifodir (getenv "CPN_DB_BASE_DIR"))
(define (get-fifo name)
  (let ((fname (string-append fifodir "/" name ".fifo")))
    (open-file fname "alb"))) ; append, line buffered, binary

(define tcp-fifo (get-fifo "tcp"))
(define dns-fifo (get-fifo "dns"))
(define flow-fifo (get-fifo "flow"))
(define web-fifo (get-fifo "web"))
(define traffic-fifo (get-fifo "traffic"))

(define (ll-dump out next . rest)
  (display next out)
  (if (null? rest) (write-char #\newline out)
      (begin (write-char #\tab out)
             (apply ll-dump out rest))))

; damn ports still not thread safe
(define dump-lock (make-mutex))
(define (dump . args)
  (with-mutex dump-lock
              (apply ll-dump args)))

(define (iface device)
  (string-append "iface " (number->string device)))
(define (vlan vlan-id)
  (if (> vlan-id 4095)
      "None"
      (string-append "Some " (number->string vlan-id))))

(define-public (print-web device vlan-id client-mac client-ip server-mac server-ip server-port qry-method err-code qry-start dt qry-host url)
  (let ((dt (/ dt 1000000.))) ; convert microsecs to secs
    (dump web-fifo
          (iface device)
          (vlan vlan-id)
          (eth->string client-mac) (ip->string client-ip)
          (eth->string server-mac) (ip->string server-ip) server-port
          qry-method err-code
          (timestamp->string qry-start) 1 dt dt dt 0
          qry-host url)))

(define-public (print-dns device vlan-id client-mac client-ip server-mac server-ip err-code qry-start dt qry-name)
  (let ((dt (/ dt 1000000.))) ; convert microsecs to secs
    (dump dns-fifo
          (iface device)
          (vlan vlan-id)
          (eth->string client-mac) (ip->string client-ip)
          (eth->string server-mac) (ip->string server-ip)
          err-code
          (timestamp->string qry-start) 1 dt dt dt 0
          qry-name)))

(define-public (print-traffic device vlan-id ts-start ts-stop count eth-src eth-dst eth-proto eth-pld eth-mtu ip-src ip-dst ip-proto ip-pld port-src port-dst l4-pld)
  (dump traffic-fifo
        (iface device)
        (timestamp->string ts-start) (timestamp->string ts-stop) count
        (vlan vlan-id)
        (eth->string eth-src) (eth->string eth-dst) eth-proto eth-pld eth-mtu
        (if ip-src (ip->string ip-src) "0.0.0.0")
        (if ip-dst (ip->string ip-dst) "0.0.0.0")
        ip-proto ip-pld
        port-src port-dst l4-pld))

(define-public (print-tcp device vlan-id client-mac client-ip server-mac server-ip client-port server-port sock-syn nb-syns dt)
  (let ((dt (/ dt 1000000.))) ; convert microsecs to secs
    (dump tcp-fifo
          (iface device)
          (vlan vlan-id)
          (eth->string client-mac) (ip->string client-ip)
          (eth->string server-mac) (ip->string server-ip)
          client-port server-port
          (timestamp->string sock-syn) nb-syns 1 dt dt dt 0)))

(define-public (print-flow device vlan-id eth-src ip-src eth-dst ip-dst ip-proto port-src port-dst ts-start ts-stop count pld)
  (dump flow-fifo
        (iface device)
        (vlan vlan-id)
        (eth->string eth-src) (ip->string ip-src)
        (eth->string eth-dst) (ip->string ip-dst)
        ip-proto port-src port-dst
        (timestamp->string ts-start) (timestamp->string ts-stop)
        count pld))

(define nt-http (nt:compile "http-response-time"
  '([(err-code uint)
     (client-ip ip)
     (server-ip ip)
     (client-mac mac)
     (server-mac mac)
     (device uint)
     (vlan uint)
     (client-port uint)
     (server-port uint)
     (qry-start timestamp)
     (qry-stop timestamp)
     (qry-method uint)
     (url str)
     (qry-host str)]
    [(http-answer
       (on-entry (apply (clopinet) print-web device vlan client-mac client-ip server-mac server-ip server-port qry-method err-code qry-start (- qry-stop qry-start) qry-host url)))
     (web-qry
       (index-size 5024)
       (timeout 60000000))]
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
                                         (device := cap.dev)
                                         (vlan := eth.vlan)
                                         (server-port := tcp.dst-port)
                                         (qry-start := cap.ts)
                                         (qry-method := http.method)
                                         (if (set? http.host)
                                             (qry-host := http.host)
                                             (qry-host := ""))
                                         (if (set? http.url)
                                             (url := http.url)
                                             (url := ""))
                                         #t)))
        (dst-index-on () client-port)
        spawn)
     (web-qry http-answer
        (match (cap ip tcp http) (if
                                   (and (ip.src == server-ip)
                                        (ip.dst == client-ip)
                                        (tcp.dst-port == client-port)
                                        (tcp.src-port == server-port)
                                        (set? http.status)
                                        (cap.ts >= qry-start))
                                   (do
                                     (qry-stop := cap.ts)
                                     (err-code := http.status)
                                     #t)))
        (src-index-on (tcp) tcp.dst-port))])))

(define nt-dns (nt:compile "dns-response-time"
  '([(err-code uint)
     (client-ip ip)
     (server-ip ip)
     (client-mac mac)
     (server-mac mac)
     (device uint)
     (vlan uint)
     (qry-start timestamp)
     (qry-stop timestamp)
     (qry-name str)]
    [(dns-answer
       (on-entry (apply (clopinet) print-dns device vlan client-mac client-ip server-mac server-ip err-code qry-start (- qry-stop qry-start) qry-name)))
     (dns-query
       (index-size 5024)
       (timeout 10000000))]
    ; edges
    [(root dns-query
        (match (cap eth ip dns) (if
                                  dns.query
                                  (do
                                    (client-ip := ip.src)
                                    (client-mac := eth.src)
                                    (server-ip := ip.dst)
                                    (server-mac := eth.dst)
                                    (device := cap.dev)
                                    (vlan := eth.vlan)
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
                                (dns.txid == txid)
                                (cap.ts >= qry-start))
                              (do
                                (qry-stop := cap.ts)
                                (err-code := dns.err-code)
                                #t)))
        (src-index-on (dns) dns.txid))])))

(define nt-eth (nt:compile "eth-traffic"
  '([(device uint)
     (vlan uint)
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
       (on-entry (apply (clopinet) print-traffic device vlan ts-start ts-stop count eth-src eth-dst eth-proto eth-pld eth-mtu #f #f 0 0 0 0 0)))
     (traffic-eth
       (index-size 5024)
       (timeout 0))
     (traffic-ip-end
       (on-entry (apply (clopinet) print-traffic device vlan ts-start ts-stop count eth-src eth-dst eth-proto eth-pld eth-mtu ip-src ip-dst ip-proto ip-pld 0 0 0)))
     (traffic-ip
       (index-size 5024)
       (timeout 0))
     (traffic-l4-end
       (on-entry (apply (clopinet) print-traffic device vlan ts-start ts-stop count eth-src eth-dst eth-proto eth-pld eth-mtu ip-src ip-dst ip-proto ip-pld port-src port-dst l4-pld)))
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
                           (device := cap.dev)
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
                                (device == cap.dev)
                                (vlan == eth.vlan)
                                (eth-proto == eth.protocol))
                           (do
                             (count := (count + 1))
                             (eth-pld := (eth-pld + eth.payload))
                             (eth-mtu := (max eth-mtu eth.payload))
                             (ts-stop := cap.ts)
                             #t)))
        (src-index-on (eth) (hash eth.src))
        grab) ; prevent root->traffic-eth
     (traffic-eth traffic-eth-end
        (on full-parse)
        (older 60000000)) ; do not prevent root->traffic-eth edge to match as well (thus reporting this packet payload)
     ; other IP
     (root traffic-ip ; for other ip traffic
        (on full-parse)
        (match (cap eth ip) (do
                              (eth-src := eth.src)
                              (eth-dst := eth.dst)
                              (device := cap.dev)
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
        (dst-index-on () (hash ip-src ip-dst))
        spawn
        grab)
     (traffic-ip traffic-ip
        (on full-parse)
        (match (cap eth ip) (if
                              (and (eth-src == eth.src)
                                   (eth-dst == eth.dst)
                                   (device == cap.dev)
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
        (src-index-on (ip) (hash ip.src ip.dst))
        grab)
     (traffic-ip traffic-ip-end
        (on full-parse)
        (older 60000000))
     ; TCP
     (root traffic-tcp
        (on full-parse)
        (match (cap eth ip tcp) (do
                                  (eth-src := eth.src)
                                  (eth-dst := eth.dst)
                                  (device := cap.dev)
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
        (dst-index-on () (hash ip-src ip-dst port-src port-dst))
        spawn
        grab)
     (traffic-tcp traffic-tcp
        (on full-parse)
        (match (cap eth ip tcp) (if
                                  (and (eth-src == eth.src)
                                       (eth-dst == eth.dst)
                                       (device == cap.dev)
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
        (src-index-on (ip tcp) (hash ip.src ip.dst tcp.src-port tcp.dst-port))
        grab)
     (traffic-tcp traffic-l4-end
        (on full-parse)
        (older 60000000))
     ; UDP
     (root traffic-udp
        (on full-parse)
        (match (cap eth ip udp) (do
                                  (eth-src := eth.src)
                                  (eth-dst := eth.dst)
                                  (device := cap.dev)
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
        (dst-index-on () (hash ip-src ip-dst port-src port-dst))
        spawn
        grab)
     (traffic-udp traffic-udp
        (on full-parse)
        (match (cap eth ip udp) (if
                                  (and (eth-src == eth.src)
                                       (eth-dst == eth.dst)
                                       (device == cap.dev)
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
        (src-index-on (ip udp) (hash ip.src ip.dst udp.src-port udp.dst-port))
        grab)
     (traffic-udp traffic-l4-end
        (on full-parse)
        (older 60000000))])))

; To report all TCP cnx establishment
(define nt-tcp (nt:compile "tcp-cnxs"
  '([(client-ip ip)
     (server-ip ip)
     (client-mac mac)
     (server-mac mac)
     (client-port uint)
     (server-port uint)
     (device uint)
     (vlan uint)
     (sock-syn timestamp)
     (sock-ack timestamp)
     (nb-syns uint)]
    [(tcp-opened
       (on-entry (apply (clopinet) print-tcp device vlan client-mac client-ip server-mac server-ip client-port server-port sock-syn nb-syns (sock-ack - sock-syn))))
     (tcp-connecting
       (on-timeout (apply (clopinet) print-tcp device vlan client-mac client-ip server-mac server-ip client-port server-port sock-syn nb-syns (sock-ack - sock-syn)))
       (index-size 20000)
       ; timeout an outstanding SYN after 80s
       (timeout 80000000))]
    ; edges
    [(root tcp-connecting
        (match (cap eth ip tcp) (if
                                  tcp.syn
                                  (do
                                    (client-port := tcp.src-port)
                                    (client-ip := ip.src)
                                    (client-mac := eth.src)
                                    (server-port := tcp.dst-port)
                                    (server-ip := ip.dst)
                                    (server-mac := eth.dst)
                                    (device := cap.dev)
                                    (vlan := eth.vlan)
                                    (sock-syn := cap.ts)
                                    (sock-ack := cap.ts) ; FIXME: 0 instead
                                    (nb-syns := 1)
                                    #t)))
        (dst-index-on () (hash client-ip client-port server-ip server-port))
        spawn)
     (tcp-connecting tcp-connecting ; other syns are merely counted
        (match (cap ip tcp) (if
                          (and
                            tcp.syn
                            (ip.src == client-ip)
                            (ip.dst == server-ip)
                            (tcp.src-port == client-port)
                            (tcp.dst-port == server-port))
                          (do
                            (nb-syns := (nb-syns + 1))
                            #t)))
        (src-index-on (ip tcp) (hash ip.src tcp.src-port ip.dst tcp.dst-port))
        grab)
     (tcp-connecting tcp-connecting ; and the other way around
        (match (cap ip tcp) (if
                          (and
                            tcp.syn
                            (ip.src == server-ip)
                            (ip.dst == client-ip)
                            (tcp.src-port == server-port)
                            (tcp.dst-port == client-port))
                          (do
                            (nb-syns := (nb-syns + 1))
                            #t)))
        (src-index-on (ip tcp) (hash ip.dst tcp.dst-port ip.src tcp.src-port))
        grab)
     (tcp-connecting tcp-opened ; when the client ack something (ie. the other syn)
        (match (cap ip tcp) (if
                          (and
                            tcp.ack
                            (ip.src == client-ip)
                            (ip.dst == server-ip)
                            (tcp.src-port == client-port)
                            (tcp.dst-port == server-port))
                          (do
                            (sock-ack := cap.ts)
                            #t)))
        (src-index-on (ip tcp) (hash ip.src tcp.src-port ip.dst tcp.dst-port))
        (dst-index-on () (hash client-ip client-port server-ip server-port))
        grab)])))

; To report all TCP/UDP data transfers (for callflow)
(define nt-flow (nt:compile "l4-data"
  '([(src-ip ip)
     (dst-ip ip)
     (src-mac mac)
     (dst-mac mac)
     (src-port uint)
     (dst-port uint)
     (ip-proto uint)
     (device uint)
     (vlan uint)
     (start timestamp)
     (stop timestamp)
     (nb-pkts uint)
     (pld uint)]
    [(tcp-tx
       (on-timeout (apply (clopinet) print-flow device vlan src-mac src-ip dst-mac dst-ip ip-proto src-port dst-port start stop nb-pkts pld))
       (index-size 20000)
       ; timeout an outstanding tcp tx after 500ms
       (timeout 500000))
     (udp-tx
       (on-timeout (apply (clopinet) print-flow device vlan src-mac src-ip dst-mac dst-ip ip-proto src-port dst-port start stop nb-pkts pld))
       (index-size 20000)
       ; timeout an outstanding udp tx after 500ms
       (timeout 500000))
     (dt-end
       (on-entry (apply (clopinet) print-flow device vlan src-mac src-ip dst-mac dst-ip ip-proto src-port dst-port start stop nb-pkts pld)))]
    ; edges
    [(root tcp-tx
        (match (cap eth ip tcp) (if
                                  (tcp.payload > 0)
                                  (do
                                    (src-port := tcp.src-port)
                                    (src-ip := ip.src)
                                    (src-mac := eth.src)
                                    (dst-port := tcp.dst-port)
                                    (dst-ip := ip.dst)
                                    (dst-mac := eth.dst)
                                    (device := cap.dev)
                                    (vlan := eth.vlan)
                                    (ip-proto := ip.proto)
                                    (start := cap.ts)
                                    (stop := cap.ts)
                                    (nb-pkts := 1)
                                    (pld := tcp.payload)
                                    #t)))
        (dst-index-on () (hash src-ip src-port dst-ip dst-port))
        spawn)
     (tcp-tx tcp-tx ; new packet in same direction
        (match (cap ip tcp) (if
                              (and
                                (tcp.payload > 0)
                                (ip.src == src-ip)
                                (ip.dst == dst-ip)
                                (tcp.src-port == src-port)
                                (tcp.dst-port == dst-port))
                              (do
                                (stop := cap.ts)
                                (nb-pkts := (nb-pkts + 1))
                                (pld := (pld + tcp.payload))
                                #t)))
        (src-index-on (ip tcp) (hash ip.src tcp.src-port ip.dst tcp.dst-port))
        grab)
     (tcp-tx dt-end ; packet in the other direction
        (match (ip tcp) (and
                          (tcp.payload > 0)
                          (ip.src == dst-ip)
                          (ip.dst == src-ip)
                          (tcp.src-port == dst-port)
                          (tcp.dst-port == src-port)))
        (src-index-on (ip tcp) (hash ip.dst tcp.dst-port ip.src tcp.src-port))
        ; no grab
        )
     (root udp-tx
        (match (cap eth ip udp) (if
                                  (udp.payload > 0)
                                  (do
                                    (src-port := udp.src-port)
                                    (src-ip := ip.src)
                                    (src-mac := eth.src)
                                    (dst-port := udp.dst-port)
                                    (dst-ip := ip.dst)
                                    (dst-mac := eth.dst)
                                    (device := cap.dev)
                                    (vlan := eth.vlan)
                                    (ip-proto := ip.proto)
                                    (start := cap.ts)
                                    (stop := cap.ts)
                                    (nb-pkts := 1)
                                    (pld := udp.payload)
                                    #t)))
        (dst-index-on () (hash src-ip src-port dst-ip dst-port))
        spawn)
     (udp-tx udp-tx ; new packet in same direction
        (match (cap ip udp) (if
                              (and
                                (udp.payload > 0)
                                (ip.src == src-ip)
                                (ip.dst == dst-ip)
                                (udp.src-port == src-port)
                                (udp.dst-port == dst-port))
                              (do
                                (stop := cap.ts)
                                (nb-pkts := (nb-pkts + 1))
                                (pld := (pld + udp.payload))
                                #t)))
        (src-index-on (ip udp) (hash ip.src udp.src-port ip.dst udp.dst-port))
        grab)
     (udp-tx dt-end ; packet in the other direction
        (match (ip udp) (and
                          (udp.payload > 0)
                          (ip.src == dst-ip)
                          (ip.dst == src-ip)
                          (udp.src-port == dst-port)
                          (udp.dst-port == src-port)))
        (src-index-on (ip udp) (hash ip.dst udp.dst-port ip.src udp.src-port))
        ; no grab
        )])))

(set-collapse-vlans #f)
(set-collapse-ifaces #f)

(nettrack-start nt-http)
(nettrack-start nt-dns)
(nettrack-start nt-eth)
(nettrack-start nt-tcp)
(nettrack-start nt-flow)

(start-repl-server)

(let ((ifaces (getenv "SNIFF_IFACES"))
      (capfilter (getenv "SNIFF_CAPTURE_FILTER")))
  (if ifaces
      (set-ifaces ifaces #:capfilter (or capfilter ""))
      (slog log-crit "SNIFF_IFACES not defined, don't know what to listen"))
  (if (null? (iface-names))
      (slog log-crit "It seams SNIFF_IFACES (~a) matches no ifaces" ifaces)))

