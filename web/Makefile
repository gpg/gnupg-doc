# Makefile for swdb
#
#
# You need to have in ~/.gnupg-autogen.rc lines like this:
#
# VERSIONSGNUPG_CMD=scp
# VERSIONSGNUPG_LOC=al-kindi.gnupg.org:/var/www/versions.gnupg.org/
#
#

all: swdb.lst.sig

swdb.lst: swdb.mac
	awk '                                \
	     ! /^#\+macro:/ {next}           \
	     $$2 ~ /ftp_.*/ {next}           \
	                    {print $$2, $$3} \
	    ' swdb.mac >swdb.lst

swdb.lst.sig: swdb.lst
	+(set -e; \
	  key=$$(grep '^SWDB_SIGNKEY=' $$HOME/.gnupg-autogen.rc|cut -d= -f2);\
          if [ -z "$$key" ]; then \
             echo "error: SWDB_SIGNKEY missing in ~/.gnupg-autogen.rc">&2; \
             exit 2;\
          fi;\
	  gpg -sbu "$$key" swdb.lst;\
	 )


signcheck: swdb.lst
	@set -e; \
        tmp="$$(awk '$$1~/^.*_ver/{print $$1}' swdb.lst \
            |sort|uniq -c|sort -nr|head -1|cut -b 1-7)"; \
	if [ "$$tmp" -ne 1 ]; then \
	  echo "ERROR: Duplicate version numbers found" >&2; \
          exit 1; \
        fi


upload: swdb.lst.sig  signcheck
	+(set -e;\
	 x=$$(grep '^VERSIONSGNUPG_CMD=' $$HOME/.gnupg-autogen.rc|cut -d= -f2);\
         if [ -z "$$x" ]; then \
           echo "error: VERSIONSGNUPG_CMD missing in ~/.gnupg-autogen.rc">&2;\
	   exit 2;\
         fi;\
	 mycmd="$$x";\
	 x=$$(grep '^VERSIONSGNUPG_LOC=' $$HOME/.gnupg-autogen.rc|cut -d= -f2);\
         if [ -z "$$x" ]; then \
           echo "error: VERSIONSGNUPG_LOC missing in ~/.gnupg-autogen.rc">&2; \
           exit 2;\
         fi;\
	 myloc="$$x";\
	 $$mycmd swdb.lst.sig swdb.lst $$myloc \
	)

.PHONY: upload all signcheck
