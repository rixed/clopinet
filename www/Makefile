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

all: index.opt

include $(top_srcdir)/make.common

$(XOBJECTS): $(top_srcdir)/clopinet.cmxa

# for some reason the same rules from make.common reverse the elemtns of %^ (!??)
index.opt: $(XOBJECTS) $(top_srcdir)/clopinet.cmxa
	$(OCAMLOPT) -o $@ $(SYNTAX) -package "$(REQUIRES)" -ccopt -L$(top_srcdir) clopinet.cmxa $(EXTRALIBS:.cma=.cmxa) -linkpkg $(OCAMLOPTFLAGS) $(XOBJECTS)

.PHONY: httpd
httpd:
	/usr/local/sbin/mini_httpd -p 8080 -c '*.cgi'
