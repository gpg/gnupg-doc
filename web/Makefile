
all: swdb.lst.sig

swdb.lst: swdb.mac
	awk '                                \
	     ! /^#\+macro:/ {next}           \
	     $$2 ~ /ftp_.*/ {next}           \
	                    {print $$2, $$3} \
	    ' swdb.mac >swdb.lst

swdb.lst.sig: swdb.lst signcheck
	gpg -sbu 0x249B39D24F25E3B6 swdb.lst

signcheck: swdb.lst
	@set -e; \
        tmp="$$(awk '$$1~/^.*_ver/{print $$1}' swdb.lst \
            |sort|uniq -c|sort -nr|head -1|cut -b 1-7)"; \
	if [ "$$tmp" -ne 1 ]; then \
	  echo "ERROR: Duplicate version numbers found" >&2; \
          exit 1; \
        fi

upload: swdb.lst.sig
	scp swdb.lst.sig swdb.lst playfair.gnupg.org:/var/www/git/versions.gnupg.org/htdocs/
	scp swdb.lst.sig swdb.lst webbuilder@trithemius.gnupg.org:/var/www/www/www.gnupg.org/htdocs/

.PHONY: upload all signcheck
