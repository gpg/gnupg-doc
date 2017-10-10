
all: swdb.lst.sig

swdb.lst: swdb.mac
	awk '                                \
	     ! /^#\+macro:/ {next}           \
	     $$2 ~ /ftp_.*/ {next}           \
	                    {print $$2, $$3} \
	    ' swdb.mac >swdb.lst

swdb.lst.sig: swdb.lst
	gpg -sbu 0x249B39D24F25E3B6 swdb.lst

upload: swdb.lst.sig
	scp swdb.lst.sig swdb.lst playfair.gnupg.org:/var/www/git/versions.gnupg.org/htdocs/
	scp swdb.lst.sig swdb.lst webbuilder@trithemius.gnupg.org:/var/www/www/www.gnupg.org/htdocs/

.PHONY: upload all
