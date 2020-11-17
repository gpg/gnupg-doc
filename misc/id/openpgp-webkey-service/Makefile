
MD_EXAMPLE_FIX = '(defun org-md-example-block (example-block contents info) \
"with fixed indentation" \
(replace-regexp-in-string \
"^" "      " \
(org-remove-indentation \
 (org-export-format-code-default example-block info))))'


# Take care:
# You may need to modify pandoc2rfc so that is used
#  -t docbook4
# because the "-t docbook" seems to be mapped to "-t docbook5" by
# current pandoc versions.  Also the quote tag needs to be replaced
# by adding
#  sed -e 's,<quote>,",g' -e 's,</quote>,",g'
# to the output of docbook.

draft.txt, draft.xml: draft.org
	emacs --batch \
           --eval "(require 'org)" \
           --eval "(require 'ox-md)" \
           --eval $(MD_EXAMPLE_FIX)  \
           --visit "draft.org" \
           --eval "(org-md-export-to-markdown)"
	sed -i 's/\*\*\(.*\):\*\*/"\1":/' draft.md
	sed <draft.md  -n '/^# Abstract/q;1,$$ p'|sed 's/^      //' >template.xml
	sed <draft.md  -n '/^# Abstract/,/^# Middle/p' | \
		sed  '1 d;$$ d' >tmp-abstract.md
	sed <draft.md  -n '/^# Middle/,/^# Back/p' | \
		sed  '1 d;$$ d' >tmp-middle.md
	sed <draft.md  -n '/^# Back/,$$ p' | \
		sed  '1 d'     >tmp-back.md
	pandoc2rfc tmp-abstract.md tmp-middle.md tmp-back.md
	pandoc2rfc -X tmp-abstract.md tmp-middle.md tmp-back.md
	rm template.xml tmp-abstract.md tmp-middle.md tmp-back.md draft.md
