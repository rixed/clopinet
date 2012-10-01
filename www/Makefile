top_srcdir = ..

SOURCES  = index.ml

REQUIRES = owww mlrrd

all: index.cgi

include $(top_srcdir)/make.common

index.cgi: index.opt
	ln -f $< $@

index.opt: index.cmx

.PHONY: httpd
httpd:
	/usr/local/sbin/mini_httpd -p 8080 -c '*.cgi'
