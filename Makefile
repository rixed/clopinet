top_srcdir = .
PKG_NAME = clopinet
SOURCES  = \
	log.ml \
	prefs.ml \
	cluster.ml \
	serial.ml \
	geoip.ml \
	output.ml \
	peg.ml \
	datatype.ml \
	user_filter.ml \
	dynlinker.ml \
	tuple2.ml \
	tuple3.ml \
	tuple4.ml \
	tuple5.ml \
	tuple6.ml \
	tuple7.ml \
	tuple8.ml \
	tuple9.ml \
	tuple10.ml \
	tuple11.ml \
	tuple12.ml \
	tuple16.ml \
	distribution.ml \
	aggregator.ml \
	dbfile.ml \
	table.ml \
	plot.ml \
	subnet.ml \
    metric.ml \
	traffic.ml \
	tcp.ml \
	web.ml \
	dns.ml \
	flow.ml

C_SOURCES = ll_serial.c ll_geoip.c
LIBS = -cclib -lpcap -cclib -lGeoIP

EXAMPLES_BYTE = \
	dns_exe.byte web_exe.byte tcp_exe.byte \
	traffic_exe.byte flow_exe.byte runtest.byte \
	cluster_test.byte

EXAMPLES_OPT = $(EXAMPLES_BYTE:.byte=.opt)
EXAMPLES = $(EXAMPLES_BYTE) $(EXAMPLES_OPT)

REQUIRES = batteries bitstring bitstring.syntax parmap dynlink graphics
SYNTAX=-syntax camlp4o

.PHONY: all loc
all: $(ARCHIVE) examples www
#clopinet.top

include $(top_srcdir)/make.common

opt: $(XARCHIVE)

examples: $(ARCHIVE) $(XARCHIVE) $(EXAMPLES)
$(EXAMPLES_BYTE): $(ARCHIVE) libclopinet.a
$(EXAMPLES_OPT): $(XARCHIVE) libclopinet.a

check.byte: $(ARCHIVE)
check.opt: $(XARCHIVE)
check-spec: check.byte
	@./check.byte || echo "custom checks FAILED"

clopinet.top: $(ARCHIVE) $(CLIB)
	# for some reason we must give here -ccopt -L. in order for ocamlmktop to find libclopinet.a
	# passing this through -passopt is not particularly elegant.
	$(OCAMLMKTOP) -o $@ $(SYNTAX) -package "findlib $(REQUIRES)" -custom -linkpkg -passopt -ccopt -passopt -L. $(ARCHIVE)

# maketuple.opt does not depend on $(ARCHIVE)
maketuple.opt: maketuple.cmx
	$(OCAMLOPT) -o $@ $(SYNTAX) -package "$(REQUIRES)" -linkpkg $(OCAMLOPTFLAGS) $^

# Dependancies on tuples can't be guessed by ocamldep
TUPLES=tuple1.ml tuple2.ml tuple3.ml tuple4.ml tuple5.ml tuple6.ml tuple7.ml tuple8.ml tuple9.ml tuple10.ml tuple11.ml tuple12.ml tuple13.ml tuple14.ml tuple15.ml tuple16.ml
$(TUPLES): maketuple.opt
$(TUPLES:.ml=.cmo): peg.cmo datatype.cmo
$(TUPLES:.ml=.cmx): peg.cmx datatype.cmx

tuple%.ml: maketuple.opt
	@n=$$(echo $@ | sed -e 's/^tuple\([0-9]*\).ml$$/\1/') ;\
	 echo "Building Tuple functor for N=$$n" ;\
	 ./maketuple.opt $$n > $@

distribution.cmo: tuple5.cmo
metric.cmo: tuple2.cmo
traffic.cmo: tuple2.cmo tuple16.cmo tuple3.cmo
distribution.cmx: tuple5.cmx
metric.cmx: tuple2.cmx
traffic.cmx: tuple2.cmx tuple16.cmx tuple3.cmx

clean-spec:
	$(MAKE) -C www clean
	rm -f clopinet.top tuple*.ml

dep-spec:
	$(MAKE) -C www dep

loc:
	@ls *.ml *.mli www/*.ml -1 | grep -v tuple | xargs cat | wc -l

.PHONY: www
www:
	$(MAKE) -C $(top_srcdir)/www index.cgi

