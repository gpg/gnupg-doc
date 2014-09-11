;;; ox-gpgweb.el --- HTML for gnupg.org Back-End for Org Export Engine

;; Copyright (C) 2011-2014 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;;      Jambunathan K <kjambunathan at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a HTML back-end for Org generic exporter.
;; See Org manual for more information.  This is a simplified version
;; of the standard HTML backend to be used for the gnupg.org website.

;;; Code:

;;; Dependencies

(require 'ox)
(require 'ox-publish)
(require 'format-spec)
(eval-when-compile (require 'cl) (require 'table nil 'noerror))


;;; Function Declarations

(declare-function htmlize-region "ext:htmlize" (beg end))

;;; Define Back-End

(org-export-define-backend 'gpgweb
  '((bold . org-gpgweb-bold)
    (center-block . org-gpgweb-center-block)
    (clock . org-gpgweb-clock)
    (code . org-gpgweb-code)
    (drawer . org-gpgweb-drawer)
    (dynamic-block . org-gpgweb-dynamic-block)
    (entity . org-gpgweb-entity)
    (example-block . org-gpgweb-example-block)
    (export-block . org-gpgweb-export-block)
    (export-snippet . org-gpgweb-export-snippet)
    (fixed-width . org-gpgweb-fixed-width)
    (footnote-definition . org-gpgweb-footnote-definition)
    (footnote-reference . org-gpgweb-footnote-reference)
    (headline . org-gpgweb-headline)
    (horizontal-rule . org-gpgweb-horizontal-rule)
    (inline-src-block . org-gpgweb-inline-src-block)
    (inlinetask . org-gpgweb-inlinetask)
    (inner-template . org-gpgweb-inner-template)
    (italic . org-gpgweb-italic)
    (item . org-gpgweb-item)
    (keyword . org-gpgweb-keyword)
    (latex-environment . org-gpgweb-latex-environment)
    (latex-fragment . org-gpgweb-latex-fragment)
    (line-break . org-gpgweb-line-break)
    (link . org-gpgweb-link)
    (node-property . org-gpgweb-node-property)
    (paragraph . org-gpgweb-paragraph)
    (plain-list . org-gpgweb-plain-list)
    (plain-text . org-gpgweb-plain-text)
    (planning . org-gpgweb-planning)
    (property-drawer . org-gpgweb-property-drawer)
    (quote-block . org-gpgweb-quote-block)
    (radio-target . org-gpgweb-radio-target)
    (section . org-gpgweb-section)
    (special-block . org-gpgweb-special-block)
    (src-block . org-gpgweb-src-block)
    (statistics-cookie . org-gpgweb-statistics-cookie)
    (strike-through . org-gpgweb-strike-through)
    (subscript . org-gpgweb-subscript)
    (superscript . org-gpgweb-superscript)
    (table . org-gpgweb-table)
    (table-cell . org-gpgweb-table-cell)
    (table-row . org-gpgweb-table-row)
    (target . org-gpgweb-target)
    (timestamp . org-gpgweb-timestamp)
    (underline . org-gpgweb-underline)
    (verbatim . org-gpgweb-verbatim)
    (verse-block . org-gpgweb-verse-block))
  :export-block "HTML"
  :filters-alist '((:filter-final-output . org-gpgweb-final-function))
  :menu-entry
  '(?G "Export for http://gnupg.org"
       ((?H "As HTML buffer" org-gpgweb-export-as-html)
	(?h "As HTML file" org-gpgweb-export-to-html)
	(?o "As HTML file and open"
	    (lambda (a s v b)
	      (if a (org-gpgweb-export-to-html t s v b)
		(org-open-file (org-gpgweb-export-to-html nil s v b)))))))
  :options-alist
  '(
    (:html-html5-fancy nil "html5-fancy" org-gpgweb-html5-fancy)
    (:html-link-home "HTML_LINK_HOME" nil org-gpgweb-link-home)
    (:html-link-up "HTML_LINK_UP" nil org-gpgweb-link-up)
    (:html-mathjax "HTML_MATHJAX" nil "" space)
    (:html-html5-fancy nil "html5-fancy" org-gpgweb-html5-fancy)
    (:html-checkbox-type nil nil org-gpgweb-checkbox-type)
    (:html-extension nil nil org-gpgweb-extension)
    (:html-home/up-format nil nil org-gpgweb-home/up-format)
    (:html-inline-image-rules nil nil org-gpgweb-inline-image-rules)
    (:html-link-org-files-as-html nil nil org-gpgweb-link-org-files-as-html)
    (:html-mathjax-options nil nil org-gpgweb-mathjax-options)
    (:html-mathjax-template nil nil org-gpgweb-mathjax-template)
    (:html-table-align-individual-fields
     nil nil org-gpgweb-table-align-individual-fields)
    (:html-table-caption-above nil nil org-gpgweb-table-caption-above)
    (:html-table-data-tags nil nil org-gpgweb-table-data-tags)
    (:html-table-header-tags nil nil org-gpgweb-table-header-tags)
    (:html-table-use-header-tags-for-first-column
     nil nil org-gpgweb-table-use-header-tags-for-first-column)
    (:html-tag-class-prefix nil nil org-gpgweb-tag-class-prefix)
    (:html-todo-kwd-class-prefix nil nil org-gpgweb-todo-kwd-class-prefix)
    (:html-toplevel-hlevel nil nil org-gpgweb-toplevel-hlevel)
    (:html-inline-images nil nil org-gpgweb-inline-images)
    (:html-table-attributes nil nil org-gpgweb-table-default-attributes)
    (:html-table-row-tags nil nil org-gpgweb-table-row-tags)
    (:html-xml-declaration nil nil org-gpgweb-xml-declaration)
    ;; Redefine regular options.
    (:with-latex nil "tex" org-gpgweb-with-latex)
    ;; Retrieve LaTeX header for fragments.
    (:latex-header "LATEX_HEADER" nil nil newline)))


;;; Internal Variables

(defvar org-gpgweb-format-table-no-css)
(defvar htmlize-buffer-places)  ; from htmlize.el

(defvar org-gpgweb--pre/postamble-class "status"
  "CSS class used for pre/postamble")

(defconst org-gpgweb-html5-elements
  '("article" "aside" "audio" "canvas" "details" "figcaption"
    "figure" "footer" "header" "menu" "meter" "nav" "output"
    "progress" "section" "video")
  "New elements in html5.

For blocks that should contain headlines, use the HTML_CONTAINER
property on the headline itself.")

(defconst org-gpgweb-special-string-regexps
  '(("\\\\-" . "&#x00ad;")		; shy
    ("---\\([^-]\\)" . "&#x2014;\\1")	; mdash
    ("--\\([^-]\\)" . "&#x2013;\\1")	; ndash
    ("\\.\\.\\." . "&#x2026;"))		; hellip
  "Regular expressions for special string conversion.")



;;; User Configuration Variables

;;;; Headline

(defconst org-gpgweb-toplevel-hlevel 2
  "The <H> level for level 1 headings in HTML export.
This is also important for the classes that will be wrapped around headlines
and outline structure.  If this variable is 1, the top-level headlines will
be <h1>, and the corresponding classes will be outline-1, section-number-1,
and outline-text-1.  If this is 2, all of these will get a 2 instead.
The default for this variable is 2, because we use <h1> for formatting the
document title.")

;;;; LaTeX

(defconst org-gpgweb-with-latex org-export-with-latex
  "Non-nil means process LaTeX math snippets.

When set, the exporter will process LaTeX environments and
fragments.

This option can also be set with the +OPTIONS line,
e.g. \"tex:mathjax\".  Allowed values are:

nil            Ignore math snippets.
`verbatim'     Keep everything in verbatim
`dvipng'       Process the LaTeX fragments to images.  This will also
               include processing of non-math environments.
`imagemagick'  Convert the LaTeX fragments to pdf files and use
               imagemagick to convert pdf files to png files.
`mathjax'      Do MathJax preprocessing and arrange for MathJax.js to
               be loaded.
t              Synonym for `mathjax'.")

;;;; Links :: Generic

(defconst org-gpgweb-link-org-files-as-html t
  "Non-nil means make file links to `file.org' point to `file.html'.
When `org-mode' is exporting an `org-mode' file to HTML, links to
non-html files are directly put into a href tag in HTML.
However, links to other Org-mode files (recognized by the
extension `.org.) should become links to the corresponding html
file, assuming that the linked `org-mode' file will also be
converted to HTML.
When nil, the links still point to the plain `.org' file.")

;;;; Links :: Inline images

(defconst org-gpgweb-inline-images t
  "Non-nil means inline images into exported HTML pages.
This is done using an <img> tag.  When nil, an anchor with href is used to
link to the image.")

(defconst org-gpgweb-inline-image-rules
  '(("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")
    ("http" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")
    ("https" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'"))
  "Rules characterizing image files that can be inlined into HTML.
A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path.")

;;;; Plain Text

(defvar org-gpgweb-protect-char-alist
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;"))
  "Alist of characters to be converted by `org-gpgweb-protect'.")

;;;; Src Block

(defconst org-gpgweb-htmlize-output-type nil
  "Output type to be used by htmlize when formatting code snippets.
Choices are `css' to export the CSS selectors only,`inline-css'
to export the CSS attribute values inline in the HTML or `nil' to
export plain text.")

(defconst org-gpgweb-htmlize-font-prefix "org-"
  "The prefix for CSS class names for htmlize font specifications.")

;;;; Table

(defconst org-gpgweb-table-default-attributes
  '(:border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides")
  "Default attributes and values which will be used in table tags.
This is a plist where attributes are symbols, starting with
colons, and values are strings.

When exporting to HTML5, these values will be disregarded.")

(defconst org-gpgweb-table-header-tags '("<th scope=\"%s\"%s>" . "</th>")
  "The opening and ending tags for table header fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `org-gpgweb-table-use-header-tags-for-first-column'.
See also the variable `org-gpgweb-table-align-individual-fields'.")

(defconst org-gpgweb-table-data-tags '("<td%s>" . "</td>")
  "The opening and ending tags for table data fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `org-gpgweb-table-align-individual-fields'.")

(defconst org-gpgweb-table-row-tags '("<tr>" . "</tr>")
  "The opening and ending tags for table rows.
This is customizable so that alignment options can be specified.
Instead of strings, these can be Lisp forms that will be
evaluated for each row in order to construct the table row tags.

During evaluation, these variables will be dynamically bound so that
you can reuse them:

       `row-number': row number (0 is the first row)
  `rowgroup-number': group number of current row
 `start-rowgroup-p': non-nil means the row starts a group
   `end-rowgroup-p': non-nil means the row ends a group
        `top-row-p': non-nil means this is the top row
     `bottom-row-p': non-nil means this is the bottom row

For example:

\(setq org-gpgweb-table-row-tags
      (cons '(cond (top-row-p \"<tr class=\\\"tr-top\\\">\")
                   (bottom-row-p \"<tr class=\\\"tr-bottom\\\">\")
                   (t (if (= (mod row-number 2) 1)
			  \"<tr class=\\\"tr-odd\\\">\"
			\"<tr class=\\\"tr-even\\\">\")))
	    \"</tr>\"))

will use the \"tr-top\" and \"tr-bottom\" classes for the top row
and the bottom row, and otherwise alternate between \"tr-odd\" and
\"tr-even\" for odd and even rows.")

(defconst org-gpgweb-table-align-individual-fields t
  "Non-nil means attach style attributes for alignment to each table field.
When nil, alignment will only be specified in the column tags, but this
is ignored by some browsers (like Firefox, Safari).  Opera does it right
though.")

(defconst org-gpgweb-table-use-header-tags-for-first-column nil
  "Non-nil means format column one in tables with header tags.
When nil, also column one will use data tags.")

(defconst org-gpgweb-table-caption-above t
  "When non-nil, place caption string at the beginning of the table.
Otherwise, place it near the end.")

;;;; Tags

(defconst org-gpgweb-tag-class-prefix ""
  "Prefix to class names for TODO keywords.
Each tag gets a class given by the tag itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful.")

;;;; Template :: Generic

(defconst org-gpgweb-extension "html"
  "The extension for exported HTML files.")

(defconst org-gpgweb-xml-declaration
  '(("html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
    ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))
  "The extension for exported HTML files.
%s will be replaced with the charset of the exported file.
This may be a string, or an alist with export extensions
and corresponding declarations.

This declaration only applies when exporting to XHTML.")

(defconst org-gpgweb-doctype "xhtml-strict"
  "Document type definition to use for exported HTML files.")

(defconst org-gpgweb-html5-fancy nil
  "Non-nil means using new HTML5 elements.
This variable is ignored for anything other than HTML5 export.

For compatibility with Internet Explorer, it's probably a good
idea to download some form of the html5shiv (for instance
https://code.google.com/p/html5shiv/) and add it to your
HTML_HEAD_EXTRA, so that your pages don't break for users of IE
versions 8 and below.")

(defconst org-gpgweb-container-element "div"
  "HTML element to use for wrapping top level sections.")

(defconst org-gpgweb-checkbox-types
  '((unicode .
     ((on . "&#x2611;") (off . "&#x2610;") (trans . "&#x2610;")))
    (ascii .
     ((on . "<code>[X]</code>")
      (off . "<code>[&#xa0;]</code>")
      (trans . "<code>[-]</code>")))
    (html .
	  ((on . "<input type='checkbox' checked='checked' />")
	  (off . "<input type='checkbox' />")
	  (trans . "<input type='checkbox' />"))))
  "Alist of checkbox types.
The cdr of each entry is an alist list three checkbox types for
HTML export: `on', `off' and `trans'.

The choices are:
  `unicode' Unicode characters (HTML entities)
  `ascii'   ASCII characters
  `html'    HTML checkboxes

Note that only the ascii characters implement tri-state
checkboxes. The other two use the `off' checkbox for `trans'.")

(defconst org-gpgweb-checkbox-type 'ascii
  "The type of checkboxes to use for HTML export.
See `org-gpgweb-checkbox-types' for for the values used for each
option.")


;;;; Template :: Mathjax

(defconst org-gpgweb-mathjax-options
  '((path  "http://orgmode.org/mathjax/MathJax.js")
    (scale "100")
    (align "center")
    (indent "2em")
    (mathml nil))
  "Options for MathJax setup.

path        The path where to find MathJax
scale       Scaling for the HTML-CSS backend, usually between 100 and 133
align       How to align display math: left, center, or right
indent      If align is not center, how far from the left/right side?
mathml      Should a MathML player be used if available?
            This is faster and reduces bandwidth use, but currently
            sometimes has lower spacing quality.  Therefore, the default is
            nil.  When browsers get better, this switch can be flipped.

You can also customize this for each buffer, using something like

#+MATHJAX: scale:\"133\" align:\"right\" mathml:t path:\"/MathJax/\"")

(defconst org-gpgweb-mathjax-template
  "<script type=\"text/javascript\" src=\"%PATH\"></script>
<script type=\"text/javascript\">
<!--/*--><![CDATA[/*><!--*/
    MathJax.Hub.Config({
        // Only one of the two following lines, depending on user settings
        // First allows browser-native MathML display, second forces HTML/CSS
        :MMLYES: config: [\"MMLorHTML.js\"], jax: [\"input/TeX\"],
        :MMLNO: jax: [\"input/TeX\", \"output/HTML-CSS\"],
        extensions: [\"tex2jax.js\",\"TeX/AMSmath.js\",\"TeX/AMSsymbols.js\",
                     \"TeX/noUndefined.js\"],
        tex2jax: {
            inlineMath: [ [\"\\\\(\",\"\\\\)\"] ],
            displayMath: [ ['$$','$$'], [\"\\\\[\",\"\\\\]\"], [\"\\\\begin{displaymath}\",\"\\\\end{displaymath}\"] ],
            skipTags: [\"script\",\"noscript\",\"style\",\"textarea\",\"pre\",\"code\"],
            ignoreClass: \"tex2jax_ignore\",
            processEscapes: false,
            processEnvironments: true,
            preview: \"TeX\"
        },
        showProcessingMessages: true,
        displayAlign: \"%ALIGN\",
        displayIndent: \"%INDENT\",

        \"HTML-CSS\": {
             scale: %SCALE,
             availableFonts: [\"STIX\",\"TeX\"],
             preferredFont: \"TeX\",
             webFont: \"TeX\",
             imageFont: \"TeX\",
             showMathMenu: true,
        },
        MMLorHTML: {
             prefer: {
                 MSIE:    \"MML\",
                 Firefox: \"MML\",
                 Opera:   \"HTML\",
                 other:   \"HTML\"
             }
        }
    });
/*]]>*///-->
</script>"
  "The MathJax setup for XHTML files.")

(defconst org-gpgweb-link-up ""
  "Where should the \"UP\" link of exported HTML pages lead?")

(defconst org-gpgweb-link-home ""
  "Where should the \"HOME\" link of exported HTML pages lead?")

(defconst org-gpgweb-home/up-format
  "<div id=\"org-div-home-and-up\">
 <a accesskey=\"h\" href=\"%s\"> UP </a>
 |
 <a accesskey=\"H\" href=\"%s\"> HOME </a>
</div>"
  "Snippet used to insert the HOME and UP links.
This is a format string, the first %s will receive the UP link,
the second the HOME link.  If both `org-gpgweb-link-up' and
`org-gpgweb-link-home' are empty, the entire snippet will be
ignored.")

;;;; Todos

(defconst org-gpgweb-todo-kwd-class-prefix ""
  "Prefix to class names for TODO keywords.
Each TODO keyword gets a class given by the keyword itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful.")


;;; Internal Functions

(defun org-gpgweb-xhtml-p (info)
  (member org-gpgweb-doctype '("xhtml-strict" "xhtml" "xhtml5")))

(defun org-gpgweb-html5-p (info)
  (member org-gpgweb-doctype '("html5" "xhtml5" "<!doctype html>")))

(defun org-gpgweb-close-tag (tag attr info)
  (concat "<" tag " " attr
	  (if (org-gpgweb-xhtml-p info) " />" ">")))

(defun org-gpgweb--make-attribute-string (attributes)
  "Return a list of attributes, as a string.
ATTRIBUTES is a plist where values are either strings or nil. An
attributes with a nil value will be omitted from the result."
  (let (output)
    (dolist (item attributes (mapconcat 'identity (nreverse output) " "))
      (cond ((null item) (pop output))
            ((symbolp item) (push (substring (symbol-name item) 1) output))
            (t (let ((key (car output))
                     (value (replace-regexp-in-string
                             "\"" "&quot;" (org-gpgweb-encode-plain-text item))))
                 (setcar output (format "%s=\"%s\"" key value))))))))

(defun org-gpgweb--wrap-image (contents info &optional caption label)
  "Wrap CONTENTS string within an appropriate environment for images.
INFO is a plist used as a communication channel.  When optional
arguments CAPTION and LABEL are given, use them for caption and
\"id\" attribute."
  (let ((html5-fancy (and (org-gpgweb-html5-p info)
			  (plist-get info :html-html5-fancy))))
    (format (if html5-fancy "\n<figure%s>%s%s\n</figure>"
	      "\n<div%s class=\"figure\">%s%s\n</div>")
	    ;; ID.
	    (if (not (org-string-nw-p label)) ""
	      (format " id=\"%s\"" (org-export-solidify-link-text label)))
	    ;; Contents.
	    (format "\n<p>%s</p>" contents)
	    ;; Caption.
	    (if (not (org-string-nw-p caption)) ""
	      (format (if html5-fancy "\n<figcaption>%s</figcaption>"
			"\n<p>%s</p>")
		      caption)))))

(defun org-gpgweb--format-image (source attributes info)
  "Return \"img\" tag with given SOURCE and ATTRIBUTES.
SOURCE is a string specifying the location of the image.
ATTRIBUTES is a plist, as returned by
`org-export-read-attribute'.  INFO is a plist used as
a communication channel."
  (if (string= "svg" (file-name-extension source))
      (org-gpgweb--svg-image source attributes info)
    (org-gpgweb-close-tag
     "img"
     (org-gpgweb--make-attribute-string
      (org-combine-plists
       (list :src source
	     :alt (if (string-match-p "^ltxpng/" source)
		      (org-gpgweb-encode-plain-text
		       (org-find-text-property-in-string 'org-latex-src source))
		    (file-name-nondirectory source)))
       attributes))
     info)))

(defun org-gpgweb--svg-image (source attributes info)
  "Return \"object\" appropriate for embedding svg file SOURCE
with assoicated ATTRIBUTES. INFO is a plist used as a
communication channel.

The special attribute \"fallback\" can be used to specify a fallback
image file to use if the object embedding is not supported."
  (let ((fallback (plist-get attributes :fallback))
	(attrs (org-gpgweb--make-attribute-string
		(plist-put attributes :fallback nil))))
  (format "<object type=\"image/svg+xml\" data=\"%s\" %s>\n%s</object>"
	  source attrs
	  (if fallback
	      (org-gpgweb-close-tag
	       "img" (format "src=\"%s\" %s" fallback attrs) info)
	    "Sorry, your browser does not support SVG."))))

(defun org-gpgweb--textarea-block (element)
  "Transcode ELEMENT into a textarea block.
ELEMENT is either a src block or an example block."
  (let* ((code (car (org-export-unravel-code element)))
	 (attr (org-export-read-attribute :attr_html element)))
    (format "<p>\n<textarea cols=\"%s\" rows=\"%s\">\n%s</textarea>\n</p>"
	    (or (plist-get attr :width) 80)
	    (or (plist-get attr :height) (org-count-lines code))
	    code)))

(defun org-gpgweb--has-caption-p (element &optional info)
  "Non-nil when ELEMENT has a caption affiliated keyword.
INFO is a plist used as a communication channel.  This function
is meant to be used as a predicate for `org-export-get-ordinal' or
a value to `org-gpgweb-standalone-image-predicate'."
  (org-element-property :caption element))

;;;; Table

(defun org-gpgweb-htmlize-region-for-paste (beg end)
  "Convert the region between BEG and END to HTML, using htmlize.el.
This is much like `htmlize-region-for-paste', only that it uses
the settings define in the org-... variables."
  (let* ((htmlize-output-type org-gpgweb-htmlize-output-type)
	 (htmlize-css-name-prefix org-gpgweb-htmlize-font-prefix)
	 (htmlbuf (htmlize-region beg end)))
    (unwind-protect
	(with-current-buffer htmlbuf
	  (buffer-substring (plist-get htmlize-buffer-places 'content-start)
			    (plist-get htmlize-buffer-places 'content-end)))
      (kill-buffer htmlbuf))))

(defun org-gpgweb--make-string (n string)
  "Build a string by concatenating N times STRING."
  (let (out) (dotimes (i n out) (setq out (concat string out)))))

(defun org-gpgweb-fix-class-name (kwd)	; audit callers of this function
  "Turn todo keyword KWD into a valid class name.
Replaces invalid characters with \"_\"."
  (save-match-data
    (while (string-match "[^a-zA-Z0-9_]" kwd)
      (setq kwd (replace-match "_" t t kwd))))
  kwd)

(defun org-gpgweb-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let* ((fn-alist (org-export-collect-footnote-definitions
		    (plist-get info :parse-tree) info))
	 (fn-alist
	  (loop for (n type raw) in fn-alist collect
		(cons n (if (eq (org-element-type raw) 'org-data)
			    (org-trim (org-export-data raw info))
			  (format "<p>%s</p>"
				  (org-trim (org-export-data raw info))))))))
    (when fn-alist
      (format "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s: </h2>
<div id=\"text-footnotes\">
%s
</div>
</div>"
       (org-gpgweb--translate "Footnotes" info)
       (format
	"\n%s\n"
	(mapconcat
	 (lambda (fn)
	   (let ((n (car fn)) (def (cdr fn)))
	     (format
	      "<div class=\"footdef\">%s %s</div>\n"
	      (format
	       "<sup>%s</sup>"
	       (org-gpgweb--anchor
		(format "fn.%d" n)
		n
		(format " class=\"footnum\" href=\"#fnr.%d\"" n)
		info))
	      def)))
	 fn-alist
	 "\n"))))))


;;; Template

(defun org-gpgweb--build-meta-info (info)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel."
  (let ((protect-string
	 (lambda (str)
	   (replace-regexp-in-string
	    "\"" "&quot;" (org-gpgweb-encode-plain-text str))))
	(title (org-export-data (plist-get info :title) info))
	(author (and (plist-get info :with-author)
		     (let ((auth (plist-get info :author)))
		       (and auth
			    ;; Return raw Org syntax, skipping non
			    ;; exportable objects.
			    (org-element-interpret-data
			     (org-element-map auth
				 (cons 'plain-text org-element-all-objects)
			       'identity info))))))
	(description (plist-get info :description))
	(keywords (plist-get info :keywords))
	(charset (or (and (fboundp 'coding-system-get)
			  (coding-system-get 'utf-8 'mime-charset))
		     "iso-8859-1")))
    (concat
     (format "<title>%s</title>\n" title)
     (when (plist-get info :time-stamp-file)
       (format-time-string
	 (concat "<!-- "
		 (plist-get info :html-metadata-timestamp-format)
		 " -->\n")))
     (format
      (if (org-gpgweb-html5-p info)
	  (org-gpgweb-close-tag "meta" " charset=\"%s\"" info)
	(org-gpgweb-close-tag
	 "meta" " http-equiv=\"Content-Type\" content=\"text/html;charset=%s\""
	 info))
      charset) "\n"
     (org-gpgweb-close-tag "meta" " name=\"generator\" content=\"Org-mode\"" info)
     "\n"
     (and (org-string-nw-p author)
	  (concat
	   (org-gpgweb-close-tag "meta"
			       (format " name=\"author\" content=\"%s\""
				       (funcall protect-string author))
			       info)
	   "\n"))
     (and (org-string-nw-p description)
	  (concat
	   (org-gpgweb-close-tag "meta"
			       (format " name=\"description\" content=\"%s\"\n"
				       (funcall protect-string description))
			       info)
	   "\n"))
     (and (org-string-nw-p keywords)
	  (concat
	   (org-gpgweb-close-tag "meta"
			       (format " name=\"keywords\" content=\"%s\""
				       (funcall protect-string keywords))
			       info)
	   "\n")))))

(defun org-gpgweb--build-mathjax-config (info)
  "Insert the user setup into the mathjax template.
INFO is a plist used as a communication channel."
  (when (and (memq (plist-get info :with-latex) '(mathjax t))
	     (org-element-map (plist-get info :parse-tree)
		 '(latex-fragment latex-environment) 'identity info t))
    (let ((template (plist-get info :html-mathjax-template))
	  (options (plist-get info :html-mathjax-options))
	  (in-buffer (or (plist-get info :html-mathjax) ""))
	  name val (yes "   ") (no "// ") x)
      (mapc
       (lambda (e)
	 (setq name (car e) val (nth 1 e))
	 (if (string-match (concat "\\<" (symbol-name name) ":") in-buffer)
	     (setq val (car (read-from-string
			     (substring in-buffer (match-end 0))))))
	 (if (not (stringp val)) (setq val (format "%s" val)))
	 (if (string-match (concat "%" (upcase (symbol-name name))) template)
	     (setq template (replace-match val t t template))))
       options)
      (setq val (nth 1 (assq 'mathml options)))
      (if (string-match (concat "\\<mathml:") in-buffer)
	  (setq val (car (read-from-string
			  (substring in-buffer (match-end 0))))))
      ;; Exchange prefixes depending on mathml setting.
      (if (not val) (setq x yes yes no no x))
      ;; Replace cookies to turn on or off the config/jax lines.
      (if (string-match ":MMLYES:" template)
	  (setq template (replace-match yes t t template)))
      (if (string-match ":MMLNO:" template)
	  (setq template (replace-match no t t template)))
      ;; Return the modified template.
      (org-element-normalize-string template))))

(defun org-gpgweb-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-gpgweb-toc depth info)))
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-gpgweb-footnote-section info)))

(defun org-gpgweb--translate (s info)
  "Translate string S according to specified language.
INFO is a plist used as a communication channel."
  (org-export-translate s :html info))

;;;; Anchor

(defun org-gpgweb--anchor (id desc attributes info)
  "Format a HTML anchor."
  (let* ((attributes (concat (and id (format " id=\"%s\"" id))
			     attributes)))
    (format "<a%s>%s</a>" attributes (or desc ""))))

;;;; Todo

(defun org-gpgweb--todo (todo info)
  "Format TODO keywords into HTML."
  (when todo
    (format "<span class=\"%s %s%s\">%s</span>"
	    (if (member todo org-done-keywords) "done" "todo")
	    (plist-get info :html-todo-kwd-class-prefix)
	    (org-gpgweb-fix-class-name todo)
	    todo)))

;;;; Tags

(defun org-gpgweb--tags (tags info)
  "Format TAGS into HTML.
INFO is a plist containing export options."
  (when tags
    (format "<span class=\"tag\">%s</span>"
	    (mapconcat
	     (lambda (tag)
	       (format "<span class=\"%s\">%s</span>"
		       (concat (plist-get info :html-tag-class-prefix)
			       (org-gpgweb-fix-class-name tag))
		       tag))
	     tags "&#xa0;"))))

;;;; Src Code

(defun org-gpgweb-fontify-code (code lang)
  "Color CODE with htmlize library.
CODE is a string representing the source code to colorize.  LANG
is the language used for CODE, as a string, or nil."
  (when code
    (cond
     ;; Case 1: No lang.  Possibly an example block.
     ((not lang)
      ;; Simple transcoding.
      (org-gpgweb-encode-plain-text code))
     ;; Case 2: No htmlize or an inferior version of htmlize
     ((not (and (require 'htmlize nil t) (fboundp 'htmlize-region-for-paste)))
      ;; Emit a warning.
      (message "Cannot fontify src block (htmlize.el >= 1.34 required)")
      ;; Simple transcoding.
      (org-gpgweb-encode-plain-text code))
     ;; Case 3: plain text explicitly set
     ((not org-gpgweb-htmlize-output-type)
      ;; Simple transcoding.
      (org-gpgweb-encode-plain-text code))
     (t
      ;; Map language
      (setq lang (or (assoc-default lang org-src-lang-modes) lang))
      (let* ((lang-mode (and lang (intern (format "%s-mode" lang)))))
	(cond
	 ;; Case 1: Language is not associated with any Emacs mode
	 ((not (functionp lang-mode))
	  ;; Simple transcoding.
	  (org-gpgweb-encode-plain-text code))
	 ;; Case 2: Default.  Fontify code.
	 (t
	  ;; htmlize
	  (setq code (with-temp-buffer
		       ;; Switch to language-specific mode.
		       (funcall lang-mode)
		       ;; Disable fci-mode if present
		       (when (and (fboundp 'fci-mode)
				  (require 'fill-column-indicator nil 'noerror))
			 (fci-mode -1))
		       (insert code)
		       ;; Fontify buffer.
		       (font-lock-ensure)
		       ;; Remove formatting on newline characters.
		       (save-excursion
			 (let ((beg (point-min))
			       (end (point-max)))
			   (goto-char beg)
			   (while (progn (end-of-line) (< (point) end))
			     (put-text-property (point) (1+ (point)) 'face nil)
			     (forward-char 1))))
		       (org-src-mode)
		       (set-buffer-modified-p nil)
		       ;; Htmlize region.
		       (org-gpgweb-htmlize-region-for-paste
			(point-min) (point-max))))
	  ;; Strip any enclosing <pre></pre> tags.
	  (let* ((beg (and (string-match "\\`<pre[^>]*>\n*" code) (match-end 0)))
		 (end (and beg (string-match "</pre>\\'" code))))
	    (if (and beg end) (substring code beg end) code)))))))))

(defun org-gpgweb-do-format-code
  (code &optional lang refs retain-labels num-start)
  "Format CODE string as source code.
Optional arguments LANG, REFS, RETAIN-LABELS and NUM-START are,
respectively, the language of the source code, as a string, an
alist between line numbers and references (as returned by
`org-export-unravel-code'), a boolean specifying if labels should
appear in the source code, and the number associated to the first
line of code."
  (let* ((code-lines (org-split-string code "\n"))
	 (code-length (length code-lines))
	 (num-fmt
	  (and num-start
	       (format "%%%ds: "
		       (length (number-to-string (+ code-length num-start))))))
	 (code (org-gpgweb-fontify-code code lang)))
    (org-export-format-code
     code
     (lambda (loc line-num ref)
       (setq loc
	     (concat
	      ;; Add line number, if needed.
	      (when num-start
		(format "<span class=\"linenr\">%s</span>"
			(format num-fmt line-num)))
	      ;; Transcoded src line.
	      loc
	      ;; Add label, if needed.
	      (when (and ref retain-labels) (format " (%s)" ref))))
       ;; Mark transcoded line as an anchor, if needed.
       (if (not ref) loc
	 (format "<span id=\"coderef-%s\" class=\"coderef-off\">%s</span>"
		 ref loc)))
     num-start refs)))

(defun org-gpgweb-format-code (element info)
  "Format contents of ELEMENT as source code.
ELEMENT is either an example block or a src block.  INFO is
a plist used as a communication channel."
  (let* ((lang (org-element-property :language element))
	 ;; Extract code and references.
	 (code-info (org-export-unravel-code element))
	 (code (car code-info))
	 (refs (cdr code-info))
	 ;; Does the src block contain labels?
	 (retain-labels (org-element-property :retain-labels element))
	 ;; Does it have line numbers?
	 (num-start (case (org-element-property :number-lines element)
		      (continued (org-export-get-loc element info))
		      (new 0))))
    (org-gpgweb-do-format-code code lang refs retain-labels num-start)))


;;; Tables of Contents

(defun org-gpgweb-toc (depth info)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is a
plist used as a communication channel.  Return the table of
contents as a string, or nil if it is empty."
  (let ((toc-entries
	 (mapcar (lambda (headline)
		   (cons (org-gpgweb--format-toc-headline headline info)
			 (org-export-get-relative-level headline info)))
		 (org-export-collect-headlines info depth)))
	(outer-tag (if (and (org-gpgweb-html5-p info)
			    (plist-get info :html-html5-fancy))
		       "nav"
		     "div")))
    (when toc-entries
      (concat (format "<%s id=\"table-of-contents\">\n" outer-tag)
	      (let ((top-level (plist-get info :html-toplevel-hlevel)))
		(format "<h%d>%s</h%d>\n"
			top-level
			(org-gpgweb--translate "Table of Contents" info)
			top-level))
	      "<div id=\"text-table-of-contents\">"
	      (org-gpgweb--toc-text toc-entries)
	      "</div>\n"
	      (format "</%s>\n" outer-tag)))))

(defun org-gpgweb--toc-text (toc-entries)
  "Return innards of a table of contents, as a string.
TOC-ENTRIES is an alist where key is an entry title, as a string,
and value is its relative level, as an integer."
  (let* ((prev-level (1- (cdar toc-entries)))
	 (start-level prev-level))
    (concat
     (mapconcat
      (lambda (entry)
	(let ((headline (car entry))
	      (level (cdr entry)))
	  (concat
	   (let* ((cnt (- level prev-level))
		  (times (if (> cnt 0) (1- cnt) (- cnt)))
		  rtn)
	     (setq prev-level level)
	     (concat
	      (org-gpgweb--make-string
	       times (cond ((> cnt 0) "\n<ul>\n<li>")
			   ((< cnt 0) "</li>\n</ul>\n")))
	      (if (> cnt 0) "\n<ul>\n<li>" "</li>\n<li>")))
	   headline)))
      toc-entries "")
     (org-gpgweb--make-string (- prev-level start-level) "</li>\n</ul>\n"))))

(defun org-gpgweb--format-toc-headline (headline info)
  "Return an appropriate table of contents entry for HEADLINE.
INFO is a plist used as a communication channel."
  (let* ((headline-number (org-export-get-headline-number headline info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (text (org-export-data-with-backend
		(org-export-get-alt-title headline info)
		;; Create an anonymous back-end that will ignore any
		;; footnote-reference, link, radio-target and target
		;; in table of contents.
		(org-export-create-backend
		 :parent 'gpgweb
		 :transcoders '((footnote-reference . ignore)
				(link . (lambda (object c i) c))
				(radio-target . (lambda (object c i) c))
				(target . ignore)))
		info))
	 (tags (and (eq (plist-get info :with-tags) t)
		    (org-export-get-tags headline info))))
    (format "<a href=\"#%s\">%s</a>"
	    ;; Label.
	    (org-export-solidify-link-text
	     (or (org-element-property :CUSTOM_ID headline)
		 (concat "sec-"
			 (mapconcat #'number-to-string headline-number "-"))))
	    ;; Body.
	    (concat
	     (and (not (org-export-low-level-p headline info))
		  (org-export-numbered-headline-p headline info)
		  (concat (mapconcat #'number-to-string headline-number ".")
			  ". "))
	     (apply 'org-gpgweb-format-headline-function
		     todo todo-type priority text tags :section-number nil)))))

(defun org-gpgweb-list-of-listings (info)
  "Build a list of listings.
INFO is a plist used as a communication channel.  Return the list
of listings as a string, or nil if it is empty."
  (let ((lol-entries (org-export-collect-listings info)))
    (when lol-entries
      (concat "<div id=\"list-of-listings\">\n"
	      (let ((top-level (plist-get info :html-toplevel-hlevel)))
		(format "<h%d>%s</h%d>\n"
			top-level
			(org-gpgweb--translate "List of Listings" info)
			top-level))
	      "<div id=\"text-list-of-listings\">\n<ul>\n"
	      (let ((count 0)
		    (initial-fmt (format "<span class=\"listing-number\">%s</span>"
					 (org-gpgweb--translate "Listing %d:" info))))
		(mapconcat
		 (lambda (entry)
		   (let ((label (org-element-property :name entry))
			 (title (org-trim
				 (org-export-data
				  (or (org-export-get-caption entry t)
				      (org-export-get-caption entry))
				  info))))
		     (concat
		      "<li>"
		      (if (not label)
			  (concat (format initial-fmt (incf count)) " " title)
			(format "<a href=\"#%s\">%s %s</a>"
				(org-export-solidify-link-text label)
				(format initial-fmt (incf count))
				title))
		      "</li>")))
		 lol-entries "\n"))
	      "\n</ul>\n</div>\n</div>"))))

(defun org-gpgweb-list-of-tables (info)
  "Build a list of tables.
INFO is a plist used as a communication channel.  Return the list
of tables as a string, or nil if it is empty."
  (let ((lol-entries (org-export-collect-tables info)))
    (when lol-entries
      (concat "<div id=\"list-of-tables\">\n"
	      (let ((top-level (plist-get info :html-toplevel-hlevel)))
		(format "<h%d>%s</h%d>\n"
			top-level
			(org-gpgweb--translate "List of Tables" info)
			top-level))
	      "<div id=\"text-list-of-tables\">\n<ul>\n"
	      (let ((count 0)
		    (initial-fmt (format "<span class=\"table-number\">%s</span>"
					 (org-gpgweb--translate "Table %d:" info))))
		(mapconcat
		 (lambda (entry)
		   (let ((label (org-element-property :name entry))
			 (title (org-trim
				 (org-export-data
				  (or (org-export-get-caption entry t)
				      (org-export-get-caption entry))
				  info))))
		     (concat
		      "<li>"
		      (if (not label)
			  (concat (format initial-fmt (incf count)) " " title)
			(format "<a href=\"#%s\">%s %s</a>"
				(org-export-solidify-link-text label)
				(format initial-fmt (incf count))
				title))
		      "</li>")))
		 lol-entries "\n"))
	      "\n</ul>\n</div>\n</div>"))))


;;; Transcode Functions

;;;; Bold

(defun org-gpgweb-bold (bold contents info)
  "Transcode BOLD from Org to HTML.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "<b>%s</b>" contents))

;;;; Center Block

(defun org-gpgweb-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "<div class=\"center\">\n%s</div>" contents))

;;;; Clock

(defun org-gpgweb-clock (clock contents info)
  "Transcode a CLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "<p>
<span class=\"timestamp-wrapper\">
<span class=\"timestamp-kwd\">%s</span> <span class=\"timestamp\">%s</span>%s
</span>
</p>"
	  org-clock-string
	  (org-translate-time
	   (org-element-property :raw-value
				 (org-element-property :value clock)))
	  (let ((time (org-element-property :duration clock)))
	    (and time (format " <span class=\"timestamp\">(%s)</span>" time)))))

;;;; Code

(defun org-gpgweb-code (code contents info)
  "Transcode CODE from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "<code>%s</code>"
	  (org-gpgweb-encode-plain-text (org-element-property :value code))))

;;;; Drawer

(defun org-gpgweb-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (funcall (lambda (name contents) contents)
	   (org-element-property :drawer-name drawer)
	   contents))

;;;; Dynamic Block

(defun org-gpgweb-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  contents)

;;;; Entity

(defun org-gpgweb-entity (entity contents info)
  "Transcode an ENTITY object from Org to HTML.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (org-element-property :html entity))

;;;; Example Block

(defun org-gpgweb-example-block (example-block contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (if (org-export-read-attribute :attr_html example-block :textarea)
      (org-gpgweb--textarea-block example-block)
    (format "<pre class=\"example\">\n%s</pre>"
	    (org-gpgweb-format-code example-block info))))

;;;; Export Snippet

(defun org-gpgweb-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (when (eq (org-export-snippet-backend export-snippet) 'html)
    (org-element-property :value export-snippet)))

;;;; Export Block

(defun org-gpgweb-export-block (export-block contents info)
  "Transcode an EXPORT-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "HTML")
    (org-remove-indentation (org-element-property :value export-block))))

;;;; Fixed Width

(defun org-gpgweb-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "<pre class=\"example\">\n%s</pre>"
	  (org-gpgweb-do-format-code
	   (org-remove-indentation
	    (org-element-property :value fixed-width)))))

;;;; Footnote Reference

(defun org-gpgweb-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
        "<sup>, </sup>"))
   (let* ((n (org-export-get-footnote-number footnote-reference info))
	  (id (format "fnr.%d%s"
		      n
		      (if (org-export-footnote-first-reference-p
			   footnote-reference info)
			  ""
			".100"))))
     (format
      "<sup>%s</sup>"
      (org-gpgweb--anchor
       id n (format " class=\"footref\" href=\"#fn.%d\"" n) info)))))

;;;; Headline

(defun org-gpgweb-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((numberedp (org-export-numbered-headline-p headline info))
	 (level (+ (org-export-get-relative-level headline info)
		   (1- (plist-get info :html-toplevel-hlevel))))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (text (org-export-data (org-element-property :title headline) info))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags headline info)))
	 (full-text (org-gpgweb-format-headline-function
			     todo todo-type priority text tags info))
	 (contents (or contents "")))
    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)
     ;; Case 2: This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((org-export-low-level-p headline info)
      ;; Build the real contents of the sub-tree.
      (let* ((type (if numberedp 'ordered 'unordered))
	     (itemized-body (org-gpgweb-format-list-item
			     contents type nil info nil full-text)))
	(concat (and (org-export-first-sibling-p headline info)
		     (org-gpgweb-begin-plain-list type))
		itemized-body
		(and (org-export-last-sibling-p headline info)
		     (org-gpgweb-end-plain-list type)))))
     ;; Case 3: Standard headline.  Export it as a section.
     (t
      (let* ((numbers (org-export-get-headline-number headline info))
	     (section-number (mapconcat #'number-to-string numbers "-"))
	     (ids (remq nil
			(list (org-element-property :CUSTOM_ID headline)
			      (concat "sec-" section-number)
			      (org-element-property :ID headline))))
	     (preferred-id (car ids))
	     (extra-ids (cdr ids))
	     (extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
	     (first-content (car (org-element-contents headline))))
	(format "<h%d id=\"%s\">%s%s</h%d>\n%s\n"
		; h?
		level
		; id=?
		preferred-id
		; insert anchors
		(mapconcat
		 (lambda (x)
		   (let ((id (org-export-solidify-link-text
			      (if (org-uuidgen-p x) (concat "ID-" x)
				x))))
		     (org-gpgweb--anchor id nil nil info)))
		 extra-ids "")
		; text of header
		(concat
		 (and numberedp
		      (format
		       "<span class=\"section-number-%d\">%s</span> "
		       level
		       (mapconcat #'number-to-string numbers ".")))
		 full-text)
		; /h?
		level
		; the content of the section
		contents))))))

(defun org-gpgweb-format-headline-function
  (todo todo-type priority text tags info)
  "Function to format headline text.

This function will be called with six arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags (string or nil).
INFO      the export options (plist).

The function result will be used in the section format string."
  (let ((todo (org-gpgweb--todo todo info))
	(tags (org-gpgweb--tags tags info)))
    (concat todo (and todo " ") text (and tags "&#xa0;&#xa0;&#xa0;") tags)))

;;;; Horizontal Rule

(defun org-gpgweb-horizontal-rule (horizontal-rule contents info)
  "Transcode an HORIZONTAL-RULE  object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-gpgweb-close-tag "hr" nil info))

;;;; Inline Src Block

(defun org-gpgweb-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((org-lang (org-element-property :language inline-src-block))
	 (code (org-element-property :value inline-src-block)))
    (let ((lang (org-element-property :language inline-src-block))
	  (code (org-gpgweb-format-code inline-src-block info))
	  (label (let ((lbl (org-element-property :name inline-src-block)))
		   (if (not lbl) ""
		       (format " id=\"%s\""
			       (org-export-solidify-link-text lbl))))))
      (format "<code class=\"src src-%s\"%s>%s</code>" lang label code))))

;;;; Inlinetask

(defun org-gpgweb-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword inlinetask)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type inlinetask)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority inlinetask)))
	 (text (org-export-data (org-element-property :title inlinetask) info))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags inlinetask info))))
    (funcall org-gpgweb-format-inlinetask-function
	     todo todo-type priority text tags contents)))

(defun org-gpgweb-format-inlinetask-function
  (todo todo-type priority text tags contents info)
  "Function called to format an inlinetask in HTML code.

The function must accept seven parameters:
  TODO      the todo keyword, as a string
  TODO-TYPE the todo type, a symbol among `todo', `done' and nil.
  PRIORITY  the inlinetask priority, as a string
  NAME      the inlinetask name, as a string.
  TAGS      the inlinetask tags, as a list of strings.
  CONTENTS  the contents of the inlinetask, as a string.
  INFO      the export options, as a plist

The function should return the string to be exported."
  (format "<div class=\"inlinetask\">\n<b>%s</b>%s\n%s</div>"
	  (org-gpgweb-format-headline-function
	   todo todo-type priority text tags info)
	  (org-gpgweb-close-tag "br" nil info)
	  contents))

;;;; Italic

(defun org-gpgweb-italic (italic contents info)
  "Transcode ITALIC from Org to HTML.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format "<i>%s</i>" contents))

;;;; Item

(defun org-gpgweb-checkbox (checkbox info)
  "Format CHECKBOX into HTML.
INFO is a plist holding contextual information.  See
`org-gpgweb-checkbox-type' for customization options."
  (cdr (assq checkbox
	     (cdr (assq (plist-get info :html-checkbox-type)
			org-gpgweb-checkbox-types)))))

(defun org-gpgweb-format-list-item (contents type checkbox info
					     &optional term-counter-id
					     headline)
  "Format a list item into HTML."
  (let ((class (if checkbox
		   (format " class=\"%s\""
			   (symbol-name checkbox)) ""))
	(checkbox (concat (org-gpgweb-checkbox checkbox info)
			  (and checkbox " ")))
	(br (org-gpgweb-close-tag "br" nil info)))
    (concat
     (case type
       (ordered
	(let* ((counter term-counter-id)
	       (extra (if counter (format " value=\"%s\"" counter) "")))
	  (concat
	   (format "<li%s%s>" class extra)
	   (when headline (concat headline br)))))
       (unordered
	(let* ((id term-counter-id)
	       (extra (if id (format " id=\"%s\"" id) "")))
	  (concat
	   (format "<li%s%s>" class extra)
	   (when headline (concat headline br)))))
       (descriptive
	(let* ((term term-counter-id))
	  (setq term (or term "(no term)"))
	  ;; Check-boxes in descriptive lists are associated to tag.
	  (concat (format "<dt%s>%s</dt>"
			  class (concat checkbox term))
		  "<dd>"))))
     (unless (eq type 'descriptive) checkbox)
     contents
     (case type
       (ordered "</li>")
       (unordered "</li>")
       (descriptive "</dd>")))))

(defun org-gpgweb-item (item contents info)
  "Transcode an ITEM element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((plain-list (org-export-get-parent item))
	 (type (org-element-property :type plain-list))
	 (counter (org-element-property :counter item))
	 (checkbox (org-element-property :checkbox item))
	 (tag (let ((tag (org-element-property :tag item)))
		(and tag (org-export-data tag info)))))
    (org-gpgweb-format-list-item
     contents type checkbox info (or tag counter))))

;;;; Keyword

(defun org-gpgweb-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "HTML") value)
     ((string= key "TOC")
      (let ((value (downcase value)))
	(cond
	 ((string-match "\\<headlines\\>" value)
	  (let ((depth (or (and (string-match "[0-9]+" value)
				(string-to-number (match-string 0 value)))
			   (plist-get info :with-toc))))
	    (org-gpgweb-toc depth info)))
	 ((string= "listings" value) (org-gpgweb-list-of-listings info))
	 ((string= "tables" value) (org-gpgweb-list-of-tables info))))))))

;;;; Latex Environment

(defun org-gpgweb-format-latex (latex-frag processing-type info)
  "Format a LaTeX fragment LATEX-FRAG into HTML.
PROCESSING-TYPE designates the tool used for conversion.  It is
a symbol among `mathjax', `dvipng', `imagemagick', `verbatim' nil
and t.  See `org-gpgweb-with-latex' for more information.  INFO is
a plist containing export properties."
  (let ((cache-relpath "") (cache-dir ""))
    (unless (eq processing-type 'mathjax)
      (let ((bfn (or (buffer-file-name)
		     (make-temp-name
		      (expand-file-name "latex" temporary-file-directory))))
	    (latex-header
	     (let ((header (plist-get info :latex-header)))
	       (and header
		    (concat (mapconcat
			     (lambda (line) (concat "#+LATEX_HEADER: " line))
			     (org-split-string header "\n")
			     "\n")
			    "\n")))))
	(setq cache-relpath
	      (concat "ltxpng/"
		      (file-name-sans-extension
		       (file-name-nondirectory bfn)))
	      cache-dir (file-name-directory bfn))
	;; Re-create LaTeX environment from original buffer in
	;; temporary buffer so that dvipng/imagemagick can properly
	;; turn the fragment into an image.
	(setq latex-frag (concat latex-header latex-frag))))
    (with-temp-buffer
      (insert latex-frag)
      (org-format-latex cache-relpath cache-dir nil "Creating LaTeX Image..."
			nil nil processing-type)
      (buffer-string))))

(defun org-gpgweb-latex-environment (latex-environment contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((processing-type (plist-get info :with-latex))
	(latex-frag (org-remove-indentation
		     (org-element-property :value latex-environment)))
	(attributes (org-export-read-attribute :attr_html latex-environment)))
    (case processing-type
      ((t mathjax)
       (org-gpgweb-format-latex latex-frag 'mathjax info))
      ((dvipng imagemagick)
       (let ((formula-link
	      (org-gpgweb-format-latex latex-frag processing-type info)))
	 (when (and formula-link (string-match "file:\\([^]]*\\)" formula-link))
	   ;; Do not provide a caption or a name to be consistent with
	   ;; `mathjax' handling.
	   (org-gpgweb--wrap-image
	    (org-gpgweb--format-image
	     (match-string 1 formula-link) attributes info) info))))
      (t latex-frag))))

;;;; Latex Fragment

(defun org-gpgweb-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((latex-frag (org-element-property :value latex-fragment))
	(processing-type (plist-get info :with-latex)))
    (case processing-type
      ((t mathjax)
       (org-gpgweb-format-latex latex-frag 'mathjax info))
      ((dvipng imagemagick)
       (let ((formula-link
	      (org-gpgweb-format-latex latex-frag processing-type info)))
	 (when (and formula-link (string-match "file:\\([^]]*\\)" formula-link))
	   (org-gpgweb--format-image (match-string 1 formula-link) nil info))))
      (t latex-frag))))

;;;; Line Break

(defun org-gpgweb-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat (org-gpgweb-close-tag "br" nil info) "\n"))

;;;; Link

(defun org-gpgweb-inline-image-p (link info)
  "Non-nil when LINK is meant to appear as an image.
INFO is a plist used as a communication channel.  LINK is an
inline image when it has no description and targets an image
file (see `org-gpgweb-inline-image-rules' for more information), or
if its description is a single link targeting an image file."
  (if (not (org-element-contents link))
      (org-export-inline-image-p
       link (plist-get info :html-inline-image-rules))
    (not
     (let ((link-count 0))
       (org-element-map (org-element-contents link)
	   (cons 'plain-text org-element-all-objects)
	 (lambda (obj)
	   (case (org-element-type obj)
	     (plain-text (org-string-nw-p obj))
	     (link (if (= link-count 1) t
		     (incf link-count)
		     (not (org-export-inline-image-p
			   obj (plist-get info :html-inline-image-rules)))))
	     (otherwise t)))
         info t)))))

(defvar org-gpgweb-standalone-image-predicate)
(defun org-gpgweb-standalone-image-p (element info)
  "Non-nil if ELEMENT is a standalone image.

INFO is a plist holding contextual information.

An element or object is a standalone image when

  - its type is `paragraph' and its sole content, save for white
    spaces, is a link that qualifies as an inline image;

  - its type is `link' and its containing paragraph has no other
    content save white spaces.

Bind `org-gpgweb-standalone-image-predicate' to constrain paragraph
further.  For example, to check for only captioned standalone
images, set it to:

  \(lambda (paragraph) (org-element-property :caption paragraph))"
  (let ((paragraph (case (org-element-type element)
		     (paragraph element)
		     (link (org-export-get-parent element)))))
    (and (eq (org-element-type paragraph) 'paragraph)
	 (or (not (fboundp 'org-gpgweb-standalone-image-predicate))
	     (funcall org-gpgweb-standalone-image-predicate paragraph))
	 (catch 'exit
	   (let ((link-count 0))
	     (org-element-map (org-element-contents paragraph)
		 (cons 'plain-text org-element-all-objects)
	       #'(lambda (obj)
		   (when (case (org-element-type obj)
			   (plain-text (org-string-nw-p obj))
			   (link (or (> (incf link-count) 1)
				     (not (org-gpgweb-inline-image-p obj info))))
			   (otherwise t))
		     (throw 'exit nil)))
	       info nil 'link)
	     (= link-count 1))))))

(defun org-gpgweb-link (link desc info)
  "Transcode a LINK object from Org to HTML.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((link-org-files-as-html-maybe
	  (function
	   (lambda (raw-path info)
	     "Treat links to `file.org' as links to `file.html', if needed."
	     (cond
	      ((string= ".org" (downcase (file-name-extension raw-path ".")))
	       (concat (file-name-sans-extension raw-path) "."
		        org-gpgweb-extension))
	      (t raw-path)))))
	 (type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (org-string-nw-p desc))
	 (path
	  (cond
	   ((member type '("http" "https" "ftp" "mailto"))
	     ;(org-link-escape-browser
	     ; (org-link-unescape (concat type ":" raw-path))))
	     (concat type ":" raw-path))
	   ((string= type "file")
	    ;; Treat links to ".org" files as ".html".
	    (setq raw-path
		  (funcall link-org-files-as-html-maybe raw-path info))
	    ;; If file path is absolute, prepend it with protocol
	    ;; component - "file:".
	    (cond
	     ((file-name-absolute-p raw-path)
	      (setq raw-path (concat "file:" raw-path))))
	    ;; Add search option, if any.  A search option can be
	    ;; relative to a custom-id or a headline title.  Append
	    ;; a hash sign to any unresolved option, as it might point
	    ;; to a target.
	    (let ((option (org-element-property :search-option link)))
	      (cond ((not option) raw-path)
		    ((eq (aref option 0) ?#) (concat raw-path option))
		    (t
		     (let ((destination
			    (org-publish-resolve-external-fuzzy-link
			     (org-element-property :path link) option)))
		       (concat raw-path
			       (if (not destination) (concat "#" option)
				 (concat "#sec-"
					 (mapconcat #'number-to-string
						    destination "-")))))))))
	   (t raw-path)))
	 ;; Extract attributes from parent's paragraph.  HACK: Only do
	 ;; this for the first link in parent (inner image link for
	 ;; inline images).  This is needed as long as attributes
	 ;; cannot be set on a per link basis.
	 (attributes-plist
	  (let* ((parent (org-export-get-parent-element link))
		 (link (let ((container (org-export-get-parent link)))
			 (if (and (eq (org-element-type container) 'link)
				  (org-gpgweb-inline-image-p link info))
			     container
			   link))))
	    (and (eq (org-element-map parent 'link 'identity info t) link)
		 (org-export-read-attribute :attr_html parent))))
	 (attributes
	  (let ((attr (org-gpgweb--make-attribute-string attributes-plist)))
	    (if (org-string-nw-p attr) (concat " " attr) "")))
	 protocol)
    (cond
     ;; Image file.
     ((and (plist-get info :html-inline-images)
	   (org-export-inline-image-p
	    link (plist-get info :html-inline-image-rules)))
      (org-gpgweb--format-image path attributes-plist info))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(when destination
	  (format "<a href=\"#%s\"%s>%s</a>"
		  (org-export-solidify-link-text
		   (org-element-property :value destination))
		  attributes desc))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(case (org-element-type destination)
	  ;; ID link points to an external file.
	  (plain-text
	   (let ((fragment (concat "ID-" path))
		 ;; Treat links to ".org" files as ".html", if needed.
		 (path (funcall link-org-files-as-html-maybe
				destination info)))
	     (format "<a href=\"%s#%s\"%s>%s</a>"
		     path fragment attributes (or desc destination))))
	  ;; Fuzzy link points nowhere.
	  ((nil)
	   (format "<i>%s</i>"
		   (or desc
		       (org-export-data
			(org-element-property :raw-link link) info))))
	  ;; Link points to a headline.
	  (headline
	   (let ((href
		  ;; What href to use?
		  (cond
		   ;; Case 1: Headline is linked via it's CUSTOM_ID
		   ;; property.  Use CUSTOM_ID.
		   ((string= type "custom-id")
		    (org-element-property :CUSTOM_ID destination))
		   ;; Case 2: Headline is linked via it's ID property
		   ;; or through other means.  Use the default href.
		   ((member type '("id" "fuzzy"))
		    (format "sec-%s"
			    (mapconcat 'number-to-string
				       (org-export-get-headline-number
					destination info) "-")))
		   (t (error "Shouldn't reach here"))))
		 ;; What description to use?
		 (desc
		  ;; Case 1: Headline is numbered and LINK has no
		  ;; description.  Display section number.
		  (if (and (org-export-numbered-headline-p destination info)
			   (not desc))
		      (mapconcat 'number-to-string
				 (org-export-get-headline-number
				  destination info) ".")
		    ;; Case 2: Either the headline is un-numbered or
		    ;; LINK has a custom description.  Display LINK's
		    ;; description or headline's title.
		    (or desc (org-export-data (org-element-property
					       :title destination) info)))))
	     (format "<a href=\"#%s\"%s>%s</a>"
		     (org-export-solidify-link-text href) attributes desc)))
	  ;; Fuzzy link points to a target or an element.
	  (t
	   (let* ((path (org-export-solidify-link-text path))
		  (org-gpgweb-standalone-image-predicate 'org-gpgweb--has-caption-p)
		  (number (cond
			   (desc nil)
			   ((org-gpgweb-standalone-image-p destination info)
			    (org-export-get-ordinal
			     (org-element-map destination 'link
			       'identity info t)
			     info 'link 'org-gpgweb-standalone-image-p))
			   (t (org-export-get-ordinal
			       destination info nil 'org-gpgweb--has-caption-p))))
		  (desc (cond (desc)
			      ((not number) "No description for this link")
			      ((numberp number) (number-to-string number))
			      (t (mapconcat 'number-to-string number ".")))))
	     (format "<a href=\"#%s\"%s>%s</a>" path attributes desc))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let ((fragment (concat "coderef-" path)))
	(format "<a href=\"#%s\"%s%s>%s</a>"
		fragment
		(org-trim
		 (format (concat "class=\"coderef\""
				 " onmouseover=\"CodeHighlightOn(this, '%s');\""
				 " onmouseout=\"CodeHighlightOff(this, '%s');\"")
			 fragment fragment))
		attributes
		(format (org-export-get-coderef-format path desc)
			(org-export-resolve-coderef path info)))))
     ;; Link type is handled by a special function.
     ((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
      (funcall protocol (org-link-unescape path) desc 'gpgweb))
     ;; External link with a description part.
     ((and path desc) (format "<a href=\"%s\"%s>%s</a>" path attributes desc))
     ;; External link without a description part.
     (path (format "<a href=\"%s\"%s>%s</a>" path attributes path))
     ;; No path, only description.  Try to do something useful.
     (t (format "<i>%s</i>" desc)))))

;;;; Node Property

(defun org-gpgweb-node-property (node-property contents info)
  "Transcode a NODE-PROPERTY element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "%s:%s"
          (org-element-property :key node-property)
          (let ((value (org-element-property :value node-property)))
            (if value (concat " " value) ""))))

;;;; Paragraph

(defun org-gpgweb-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let* ((parent (org-export-get-parent paragraph))
	 (parent-type (org-element-type parent))
	 (style '((footnote-definition " class=\"footpara\"")))
	 (attributes (org-gpgweb--make-attribute-string
		      (org-export-read-attribute :attr_html paragraph)))
	 (extra (or (cadr (assoc parent-type style)) "")))
    (cond
     ((and (eq (org-element-type parent) 'item)
	   (= (org-element-property :begin paragraph)
	      (org-element-property :contents-begin parent))
	   (not (org-element-map (org-export-get-parent parent) 'item
		  (lambda (item)
		    (let ((contents (org-element-contents item)))
		      (and contents
			   (or (cdr contents)
			       (not (eq (org-element-type (car contents))
					'paragraph))))))
		  info 'first-match 'item)))
      ;; Leading paragraph in a list item have no tags if every
      ;; element of the containing list is only a single paragraph.
      contents)
     ((org-gpgweb-standalone-image-p paragraph info)
      ;; Standalone image.
      (let ((caption
	     (let ((raw (org-export-data
			 (org-export-get-caption paragraph) info))
		   (org-gpgweb-standalone-image-predicate
		    'org-gpgweb--has-caption-p))
	       (if (not (org-string-nw-p raw)) raw
		 (concat
                  "<span class=\"figure-number\">"
		  (format (org-gpgweb--translate "Figure %d:" info)
			  (org-export-get-ordinal
			   (org-element-map paragraph 'link
			     'identity info t)
			   info nil 'org-gpgweb-standalone-image-p))
		  "</span> " raw))))
	    (label (org-element-property :name paragraph)))
	(org-gpgweb--wrap-image contents info caption label)))
     ;; Regular paragraph.
     (t (format "<p%s%s>\n%s</p>"
		(if (org-string-nw-p attributes)
		    (concat " " attributes) "")
		extra contents)))))

;;;; Plain List

;; FIXME Maybe arg1 is not needed because <li value="20"> already sets
;; the correct value for the item counter
(defun org-gpgweb-begin-plain-list (type &optional arg1)
  "Insert the beginning of the HTML list depending on TYPE.
When ARG1 is a string, use it as the start parameter for ordered
lists."
  (case type
    (ordered
     (format "<ol%s>"
	     (if arg1 (format " start=\"%d\"" arg1) "")))
    (unordered "<ul>")
    (descriptive "<dl>")))

(defun org-gpgweb-end-plain-list (type)
  "Insert the end of the HTML list depending on TYPE."
  (case type
    (ordered "</ol>")
    (unordered "</ul>")
    (descriptive "</dl>")))

(defun org-gpgweb-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to HTML.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* (arg1 ;; (assoc :counter (org-element-map plain-list 'item
	 (type (org-element-property :type plain-list)))
    (format "%s\n%s%s"
	    (org-gpgweb-begin-plain-list type)
	    contents (org-gpgweb-end-plain-list type))))

;;;; Plain Text

(defun org-gpgweb-convert-special-strings (string)
  "Convert special characters in STRING to HTML."
  (let ((all org-gpgweb-special-string-regexps)
	e a re rpl start)
    (while (setq a (pop all))
      (setq re (car a) rpl (cdr a) start 0)
      (while (string-match re string start)
	(setq string (replace-match rpl t nil string))))
    string))

(defun org-gpgweb-encode-plain-text (text)
  "Convert plain text characters from TEXT to HTML equivalent.
Possible conversions are set in `org-gpgweb-protect-char-alist'."
  (mapc
   (lambda (pair)
     (setq text (replace-regexp-in-string (car pair) (cdr pair) text t t)))
   org-gpgweb-protect-char-alist)
  text)

(defun org-gpgweb-plain-text (text info)
  "Transcode a TEXT string from Org to HTML.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (let ((output text))
    ;; Protect following characters: <, >, &.
    (setq output (org-gpgweb-encode-plain-text output))
    ;; Handle smart quotes.  Be sure to provide original string since
    ;; OUTPUT may have been modified.
    (when (plist-get info :with-smart-quotes)
      (setq output (org-export-activate-smart-quotes output :html info text)))
    ;; Handle special strings.
    (when (plist-get info :with-special-strings)
      (setq output (org-gpgweb-convert-special-strings output)))
    ;; Handle break preservation if required.
    (when (plist-get info :preserve-breaks)
      (setq output
	    (replace-regexp-in-string
	     "\\(\\\\\\\\\\)?[ \t]*\n"
	     (concat (org-gpgweb-close-tag "br" nil info) "\n") output)))
    ;; Return value.
    output))


;; Planning

(defun org-gpgweb-planning (planning contents info)
  "Transcode a PLANNING element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((span-fmt "<span class=\"timestamp-kwd\">%s</span> <span class=\"timestamp\">%s</span>"))
    (format
     "<p><span class=\"timestamp-wrapper\">%s</span></p>"
     (mapconcat
      'identity
      (delq nil
	    (list
	     (let ((closed (org-element-property :closed planning)))
	       (when closed
		 (format span-fmt org-closed-string
			 (org-translate-time
			  (org-element-property :raw-value closed)))))
	     (let ((deadline (org-element-property :deadline planning)))
	       (when deadline
		 (format span-fmt org-deadline-string
			 (org-translate-time
			  (org-element-property :raw-value deadline)))))
	     (let ((scheduled (org-element-property :scheduled planning)))
	       (when scheduled
		 (format span-fmt org-scheduled-string
			 (org-translate-time
			  (org-element-property :raw-value scheduled)))))))
      " "))))

;;;; Property Drawer

(defun org-gpgweb-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to HTML.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (and nil
       (org-string-nw-p contents)
       (format "<pre class=\"example\">\n%s</pre>" contents)))

;;;; Quote Block

(defun org-gpgweb-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "<blockquote>\n%s</blockquote>" contents))

;;;; Section

(defun org-gpgweb-section (section contents info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let ((parent (org-export-get-parent-headline section)))
    ;; Before first headline: no container, just return CONTENTS.
    (if (not parent) contents
      ;; Get div's class and id references.
      (let* ((class-num (+ (org-export-get-relative-level parent info)
			   (1- (plist-get info :html-toplevel-hlevel))))
	     (section-number
	      (mapconcat
	       'number-to-string
	       (org-export-get-headline-number parent info) "-")))
        ;; Build return value.
	(format "<div class=\"outline-text-%d\" id=\"text-%s\">\n%s</div>"
		class-num
		(or (org-element-property :CUSTOM_ID parent) section-number)
		contents)))))

;;;; Radio Target

(defun org-gpgweb-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to HTML.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (let ((id (org-export-solidify-link-text
	     (org-element-property :value radio-target))))
    (org-gpgweb--anchor id text nil info)))

;;;; Special Block

(defun org-gpgweb-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (if (org-export-raw-special-block-p special-block info)
      (org-remove-indentation (org-element-property :raw-value special-block))
    (let* ((block-type (downcase (org-element-property :type special-block)))
	   (contents (or contents ""))
	   (html5-fancy (and (org-gpgweb-html5-p info)
			     (plist-get info :html-html5-fancy)
			     (member block-type org-gpgweb-html5-elements)))
	   (attributes (org-export-read-attribute :attr_html special-block)))
      (unless html5-fancy
	(let ((class (plist-get attributes :class)))
	  (setq attributes (plist-put attributes :class
				      (if class (concat class " " block-type)
					block-type)))))
      (setq attributes (org-gpgweb--make-attribute-string attributes))
      (when (not (equal attributes ""))
	(setq attributes (concat " " attributes)))
      (if html5-fancy
	  (format "<%s%s>\n%s</%s>" block-type attributes contents block-type)
	(format "<div%s>\n%s\n</div>" attributes contents)))))

;;;; Src Block

(defun org-gpgweb-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-gpgweb--textarea-block src-block)
    (let ((lang (org-element-property :language src-block))
	  (caption (org-export-get-caption src-block))
	  (code (org-gpgweb-format-code src-block info))
	  (label (let ((lbl (org-element-property :name src-block)))
		   (if (not lbl) ""
		     (format " id=\"%s\""
			     (org-export-solidify-link-text lbl))))))
      (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
	(format
	 "<div class=\"org-src-container\">\n%s%s\n</div>"
	 (if (not caption) ""
	   (format "<label class=\"org-src-name\">%s</label>"
		   (org-export-data caption info)))
	 (format "\n<pre class=\"src src-%s\"%s>%s</pre>" lang label code))))))

;;;; Statistics Cookie

(defun org-gpgweb-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((cookie-value (org-element-property :value statistics-cookie)))
    (format "<code>%s</code>" cookie-value)))

;;;; Strike-Through

(defun org-gpgweb-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to HTML.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "<del>%s</del>" contents))

;;;; Subscript

(defun org-gpgweb-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<sub>%s</sub>" contents))

;;;; Superscript

(defun org-gpgweb-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<sup>%s</sup>" contents))

;;;; Table Cell

(defun org-gpgweb-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((table-row (org-export-get-parent table-cell))
	 (table (org-export-get-parent-table table-cell))
	 (cell-attrs
	  (if (not (plist-get info :html-table-align-individual-fields)) ""
	    (format (if (and (boundp 'org-gpgweb-format-table-no-css)
			     org-gpgweb-format-table-no-css)
			" align=\"%s\"" " class=\"%s\"")
		    (org-export-table-cell-alignment table-cell info)))))
    (when (or (not contents) (string= "" (org-trim contents)))
      (setq contents "&#xa0;"))
    (cond
     ((and (org-export-table-has-header-p table info)
	   (= 1 (org-export-table-row-group table-row info)))
      (let ((header-tags (plist-get info :html-table-header-tags)))
	(concat "\n" (format (car header-tags) "col" cell-attrs)
		contents
		(cdr header-tags))))
     ((and (plist-get info :html-table-use-header-tags-for-first-column)
	   (zerop (cdr (org-export-table-cell-address table-cell info))))
      (let ((header-tags (plist-get info :html-table-header-tags)))
	(concat "\n" (format (car header-tags) "row" cell-attrs)
		contents
		(cdr header-tags))))
     (t (let ((data-tags (plist-get info :html-table-data-tags)))
	  (concat "\n" (format (car data-tags) cell-attrs)
		  contents
		  (cdr data-tags)))))))

;;;; Table Row

(defun org-gpgweb-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to HTML.
CONTENTS is the contents of the row.  INFO is a plist used as a
communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((rowgroup-number (org-export-table-row-group table-row info))
	   (row-number (org-export-table-row-number table-row info))
	   (start-rowgroup-p
	    (org-export-table-row-starts-rowgroup-p table-row info))
	   (end-rowgroup-p
	    (org-export-table-row-ends-rowgroup-p table-row info))
	   ;; `top-row-p' and `end-rowgroup-p' are not used directly
	   ;; but should be set so that `org-gpgweb-table-row-tags' can
	   ;; use them (see the docstring of this variable.)
	   (top-row-p (and (equal start-rowgroup-p '(top))
			   (equal end-rowgroup-p '(below top))))
	   (bottom-row-p (and (equal start-rowgroup-p '(above))
			      (equal end-rowgroup-p '(bottom above))))
	   (rowgroup-tags
	    (cond
	     ;; Case 1: Row belongs to second or subsequent rowgroups.
	     ((not (= 1 rowgroup-number))
	      '("<tbody>" . "\n</tbody>"))
	     ;; Case 2: Row is from first rowgroup.  Table has >=1 rowgroups.
	     ((org-export-table-has-header-p
	       (org-export-get-parent-table table-row) info)
	      '("<thead>" . "\n</thead>"))
	     ;; Case 2: Row is from first and only row group.
	     (t '("<tbody>" . "\n</tbody>")))))
      (concat
       ;; Begin a rowgroup?
       (when start-rowgroup-p (car rowgroup-tags))
       ;; Actual table row
       (concat "\n" (eval (car (plist-get info :html-table-row-tags)))
	       contents
	       "\n"
	       (eval (cdr (plist-get info :html-table-row-tags))))
       ;; End a rowgroup?
       (when end-rowgroup-p (cdr rowgroup-tags))))))

;;;; Table

(defun org-gpgweb-table-first-row-data-cells (table info)
  "Transcode the first row of TABLE.
INFO is a plist used as a communication channel."
  (let ((table-row
	 (org-element-map table 'table-row
	   (lambda (row)
	     (unless (eq (org-element-property :type row) 'rule) row))
	   info 'first-match))
	(special-column-p (org-export-table-has-special-column-p table)))
    (if (not special-column-p) (org-element-contents table-row)
      (cdr (org-element-contents table-row)))))

(defun org-gpgweb-table--table.el-table (table info)
  "Format table.el tables into HTML.
INFO is a plist used as a communication channel."
  (when (eq (org-element-property :type table) 'table.el)
    (require 'table)
    (let ((outbuf (with-current-buffer
		      (get-buffer-create "*org-export-table*")
		    (erase-buffer) (current-buffer))))
      (with-temp-buffer
	(insert (org-element-property :value table))
	(goto-char 1)
	(re-search-forward "^[ \t]*|[^|]" nil t)
	(table-generate-source 'html outbuf))
      (with-current-buffer outbuf
	(prog1 (org-trim (buffer-string))
	  (kill-buffer) )))))

(defun org-gpgweb-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (case (org-element-property :type table)
    ;; Case 1: table.el table.  Convert it using appropriate tools.
    (table.el (org-gpgweb-table--table.el-table table info))
    ;; Case 2: Standard table.
    (t
     (let* ((label (org-element-property :name table))
	    (caption (org-export-get-caption table))
	    (number (org-export-get-ordinal
		     table info nil 'org-gpgweb--has-caption-p))
	    (attributes
	     (org-gpgweb--make-attribute-string
	      (org-combine-plists
	       (and label (list :id (org-export-solidify-link-text label)))
	       (and (not (org-gpgweb-html5-p info))
		    (plist-get info :html-table-attributes))
	       (org-export-read-attribute :attr_html table))))
	    (alignspec
	     (if (and (boundp 'org-gpgweb-format-table-no-css)
		      org-gpgweb-format-table-no-css)
		 "align=\"%s\"" "class=\"%s\""))
	    (table-column-specs
	     (function
	      (lambda (table info)
		(mapconcat
		 (lambda (table-cell)
		   (let ((alignment (org-export-table-cell-alignment
				     table-cell info)))
		     (concat
		      ;; Begin a colgroup?
		      (when (org-export-table-cell-starts-colgroup-p
			     table-cell info)
			"\n<colgroup>")
		      ;; Add a column.  Also specify it's alignment.
		      (format "\n%s"
			      (org-gpgweb-close-tag
			       "col" (concat " " (format alignspec alignment)) info))
		      ;; End a colgroup?
		      (when (org-export-table-cell-ends-colgroup-p
			     table-cell info)
			"\n</colgroup>"))))
		 (org-gpgweb-table-first-row-data-cells table info) "\n")))))
       (format "<table%s>\n%s\n%s\n%s</table>"
	       (if (equal attributes "") "" (concat " " attributes))
	       (if (not caption) ""
		 (format (if (plist-get info :html-table-caption-above)
			     "<caption class=\"t-above\">%s</caption>"
			   "<caption class=\"t-bottom\">%s</caption>")
			 (concat
			  "<span class=\"table-number\">"
                          (format (org-gpgweb--translate "Table %d:" info) number)
			  "</span> " (org-export-data caption info))))
	       (funcall table-column-specs table info)
	       contents)))))

;;;; Target

(defun org-gpgweb-target (target contents info)
  "Transcode a TARGET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((id (org-export-solidify-link-text
	     (org-element-property :value target))))
    (org-gpgweb--anchor id nil nil info)))

;;;; Timestamp

(defun org-gpgweb-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-gpgweb-plain-text
		(org-timestamp-translate timestamp) info)))
    (format "<span class=\"timestamp-wrapper\"><span class=\"timestamp\">%s</span></span>"
	    (replace-regexp-in-string "--" "&#x2013;" value))))

;;;; Underline

(defun org-gpgweb-underline (underline contents info)
  "Transcode UNDERLINE from Org to HTML.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "<span class=\"underline\">%s</span>" contents))

;;;; Verbatim

(defun org-gpgweb-verbatim (verbatim contents info)
  "Transcode VERBATIM from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "<code>%s</code>"
         (org-gpgweb-encode-plain-text (org-element-property :value verbatim))))

;;;; Verse Block

(defun org-gpgweb-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to HTML.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  ;; Replace each newline character with line break.  Also replace
  ;; each blank line with a line break.
  (setq contents (replace-regexp-in-string
		  "^ *\\\\\\\\$" (format "%s\n" (org-gpgweb-close-tag "br" nil info))
		  (replace-regexp-in-string
		   "\\(\\\\\\\\\\)?[ \t]*\n"
		   (format "%s\n" (org-gpgweb-close-tag "br" nil info)) contents)))
  ;; Replace each white space at beginning of a line with a
  ;; non-breaking space.
  (while (string-match "^[ \t]+" contents)
    (let* ((num-ws (length (match-string 0 contents)))
	   (ws (let (out) (dotimes (i num-ws out)
			    (setq out (concat out "&#xa0;"))))))
      (setq contents (replace-match ws nil t contents))))
  (format "<p class=\"verse\">\n%s</p>" contents))


;;; Filter Functions

(defun org-gpgweb-final-function (contents backend info)
  "Filter to indent the HTML and convert HTML entities."
  (with-temp-buffer
    (insert contents)
    (buffer-substring-no-properties (point-min) (point-max))))


;;; End-user functions

;;;###autoload
(defun org-gpgweb-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

The optional argument BODY-ONLY is ignored.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'gpgweb "*Org GPGWEB Export*"
    async subtreep visible-only t ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-gpgweb-convert-region-to-html ()
  "Assume the current region has org-mode syntax, and convert it to HTML.
This can be used in any buffer.  For example, you can write an
itemized list in org-mode syntax in an HTML buffer and use this
command to convert it."
  (interactive)
  (org-export-replace-region-by 'gpgweb))

;;;###autoload
(defun org-gpgweb-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." (or (plist-get ext-plist :html-extension)
				    org-gpgweb-extension
				    "html")))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system 'utf-8))
    (org-export-to-file 'gpgweb file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-gpgweb-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'gpgweb filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-gpgweb-extension
				      "html"))
		      plist pub-dir))


(provide 'ox-gpgweb)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-gpgweb.el ends here
