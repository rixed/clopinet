top_srcdir = ..

SOURCES  = \
	color.ml \
	chart.ml \
	my_view.ml \
	my_time.ml \
	filter_expr.ml \
	selector.ml \
	my_forms.ml \
	my_ctrl.ml \
	help.ml \
	index.ml \

REQUIRES = owww batteries bitstring bitstring.syntax dynlink parmap
EXTRALIBS = $(top_srcdir)/clopinet.cma

all: index.cgi

include $(top_srcdir)/make.common

$(XOBJECTS): $(top_srcdir)/clopinet.cmxa

# for some reason the same rules from make.common reverse the elemtns of %^ (!??)
index.opt: $(XOBJECTS) $(top_srcdir)/clopinet.cmxa
	$(OCAMLOPT) -o $@ $(SYNTAX) -package "$(REQUIRES)" -ccopt -L$(top_srcdir) clopinet.cmxa $(EXTRALIBS:.cma=.cmxa) -linkpkg $(OCAMLOPTFLAGS) $(XOBJECTS)

index.cgi: index.opt
	@echo '#!/bin/sh' > $@
	@echo 'export LD_LIBRARY_PATH="$(libdir)"' >> $@
	@echo '. "$(sysconfdir)/clopinet.conf"' >> $@
	@echo 'exec "$(bindir)/index.opt" 2>/dev/null' >> $@
	@chmod a+x $@

install: index.opt index.cgi
	install -d "$(wwwdir)"
	install index.opt "$(bindir)"
	install index.cgi "$(wwwdir)"
	cp -r static "$(wwwdir)/"

uninstall:
	rm -f "$(bindir)/index.opt"
	rm -f "$(wwwdir)/index.cgi"
	rm -rf "$(wwwdir)/static"

clean-spec:
	rm -f index.cgi
