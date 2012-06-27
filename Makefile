top_srcdir = .
PKG_NAME = mlrrd
SOURCES  = \
	txtInput.ml \
	binInput.ml \
	output.ml \
	datatype.ml \
	aggregator.ml \
	dbfile.ml \
	table.ml \

EXAMPLES_BYTE = \
	dns.byte web.byte \

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

clean-spec:
	rm -f mlrrd.top

