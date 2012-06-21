top_srcdir = .
PKG_NAME = mlrrd
SOURCES  = \
	txtInput.ml \
	datatype.ml \
	aggregator.ml \
	dbfile.ml \
	table.ml \

EXAMPLES_BYTE = \
	dns.byte \

EXAMPLES_OPT = $(EXAMPLES_BYTE:.byte=.opt)
EXAMPLES = $(EXAMPLES_BYTE) $(EXAMPLES_OPT)

REQUIRES = bricabrac pfds zip

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
	$(OCAMLMKTOP)  -o $@ -package "$(REQUIRES)" -custom $(ARCHIVE)

clean-spec:
	rm -f mlrrd.top

