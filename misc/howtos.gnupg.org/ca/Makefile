#
# $Id: Makefile,v 1.2 2004/10/12 21:06:13 lolo Exp $
#
# Translation by:
# 2003  Josep Monés i Teixidor <jmones@puntbarra.com>
#

LINUXDOC := linuxdoc

ALL_FORMATS := GPGMiniHowto_big.html \
	       GPGMiniHowto.html     \
	       GPGMiniHowto.txt      \
	       GPGMiniHowto.rtf      \
	       GPGMiniHowto.dvi      \
	       GPGMiniHowto.pdf      \
	       GPGMiniHowto.ps

ALL_TARBALL := $(ALL_FORMATS)	     \
	       GPGMiniHowto.sgml

all: $(ALL_FORMATS) GPGMiniHowto.tar.gz


# One HTML big file
GPGMiniHowto_big.html : GPGMiniHowto.sgml
	$(LINUXDOC) -B html -s 0 $<
	mv GPGMiniHowto.html $@

# txt format
GPGMiniHowto.txt: GPGMiniHowto.sgml
	$(LINUXDOC) -B txt --charset=latin $<

# Other formats
GPGMiniHowto.html \
GPGMiniHowto.rtf: \
GPGMiniHowto.% : GPGMiniHowto.sgml
	linuxdoc -B $* $<

GPGMiniHowto.dvi \
GPGMiniHowto.pdf \
GPGMiniHowto.ps: \
GPGMiniHowto.% : GPGMiniHowto.sgml
	linuxdoc -B latex -o $* $<

GPGMiniHowto.tar.gz: $(ALL_TARBALL)
	rm -Rf GPGMiniHowto 2> /dev/null
	mkdir GPGMiniHowto
	for file in "$(ALL_TARBALL)"; do cp $$file GPGMiniHowto/. ; done 
	tar cvfz GPGMiniHowto.tar.gz GPGMiniHowto/

clean:
	rm -Rf GPGMiniHowto 2> /dev/null
	rm $(ALL_FORMATS) 2> /dev/null
	rm GPGMiniHowto.tar.gz


