
srcdir = .

# Note that you need a recent version of emacs23 with org-mode 7.01h
#
# Note that due to a problem with hyperref we are currently not able
# to build a pdf version
#
#	  --visit "$(srcdir)/gnupg-faq.org" \
#         --eval "(org-latex-export-to-pdf)"\
#

gnupg-faq.txt gnupg-faq.html: gnupg-faq.org
	@set -e; \
	 emacs  --batch \
	  --eval "(require 'org)" \
	  --visit "$(srcdir)/gnupg-faq.org" \
          --eval "(org-ascii-export-to-ascii)"

faq-online: gnupg-faq.txt gnupg-faq.html
	set -e; \
        user=werner ; \
	echo "Uploading current FAQ to ftp.gnupg.org ..."; \
	scp gnupg-faq.txt  $${user}@ftp.gnupg.org:gcrypt/gnupg/GnuPG-FAQ.txt; \
	echo "...ready"
