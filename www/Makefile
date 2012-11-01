top_srcdir = ..

SOURCES  = color.ml index.ml

REQUIRES = owww mlrrd

all: index.cgi

include $(top_srcdir)/make.common

index.cgi: index.opt
	ln -f $< $@

# for some reason the same rules from make.common reverse the elemtns of %^ (!??)
index.opt: color.cmx index.cmx
	$(OCAMLOPT) -o $@ $(SYNTAX) -package "$(REQUIRES) $(PKG_NAME)" $(EXTRALIBS:.cma=.cmxa) -linkpkg $(OCAMLOPTFLAGS) $^

.PHONY: httpd
httpd:
	/usr/local/sbin/mini_httpd -p 8080 -c '*.cgi'
