top_srcdir = .
PKG_NAME = mlrrd
SOURCES  = \
	txtInput.ml \
	binInput.ml \
	output.ml \
	datatype.ml \
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
	tuple13.ml \
	distribution.ml \
	aggregator.ml \
	dbfile.ml \
	table.ml \

EXAMPLES_BYTE = \
	dns.byte web.byte eth.byte \

EXAMPLES_OPT = $(EXAMPLES_BYTE:.byte=.opt)
EXAMPLES = $(EXAMPLES_BYTE) $(EXAMPLES_OPT)

REQUIRES = bricabrac pfds bitstring bitstring.syntax zip parmap
SYNTAX=-syntax camlp4o

.PHONY: all
all: $(ARCHIVE) mlrrd.top examples

include $(top_srcdir)/make.common

opt: $(XARCHIVE)

examples: $(ARCHIVE) $(XARCHIVE) $(EXAMPLES)

check.byte: $(ARCHIVE)
check.opt: $(XARCHIVE)
check: check.byte
	@./check.byte || echo "FAILED"

mlrrd.top: $(ARCHIVE)
	$(OCAMLMKTOP)  -o $@ $(SYNTAX) -package "findlib,$(REQUIRES)" -linkpkg $(ARCHIVE)

tuple1.ml tuple2.ml tuple3.ml tuple4.ml tuple5.ml tuple6.ml tuple7.ml tuple8.ml tuple9.ml tuple10.ml tuple11.ml tuple12.ml tuple13.ml: maketuple.opt
	@n=$$(echo $@ | sed -e 's/^tuple\([0-9]*\).ml$$/\1/') ;\
	 echo "Building Tuple functor for N=$$n" ;\
	 ./maketuple.opt $$n > $@

distribution.cmo: tuple5.cmo

clean-spec:
	rm -f mlrrd.top

