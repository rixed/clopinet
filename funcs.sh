web_resp_times () {
	./web.opt $* -dump 0 | sort -k 8 | gnuplot -p gnuplot.templates/web_resp_times
}

dns_resp_times () {
	./dns.opt $* -dump 0 | sort -k 2 | gnuplot -p gnuplot.templates/dns_resp_times
}

