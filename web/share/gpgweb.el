;;; gpgweb.el --- elisp helper code for the GnuPG web pages

(if (< (string-to-number emacs-version) 24)
    (require 'org-exp))

;; makeindex disabled because the generated file is created in the
;; source directory.
(defun gpgweb-setup-project ()
  "Set up an org-publish project for the gnupg.org website."
  (progn
   (require 'ox-gpgweb (concat gpgweb-root-dir "share/ox-gpgweb.el"))
   (aput 'org-publish-project-alist "gpgweb-org"
   `(:base-directory ,gpgweb-root-dir
     :base-extension "org"
     :language "en"
     :html-extension "html"
     :recursive t
     :publishing-directory ,gpgweb-stage-dir
     :publishing-function gpgweb-org-to-html
     :body-only t
     :section-numbers nil
     :tags nil
     :with-toc nil
     :makeindex nil
     :auto-sitemap nil
     :sitemap-title "GnuPG - Sitemap"
     :sitemap-sort-folders "last"
     :sitemap-file-entry-format "%t  @@html:<span id=\"smallnote\">@@(%d)@@html:</span>@@"
     :style-include-default nil
     :timestamp-file nil
     :html-head "<link rel=\"stylesheet\" href=\"gnupg.css\" type=\"text/css\" />"
     :html-head-include-scripts nil))

   (aput 'org-publish-project-alist "gpgweb-other"
   `(:base-directory ,gpgweb-root-dir
     :base-extension "jpg\\|png\\|css\\|txt\\|rss\\|lst\\|sig"
     :recursive t
     :publishing-directory ,gpgweb-stage-dir
     :publishing-function org-publish-attachment
     :completion-function gpgweb-upload))

   (aput 'org-publish-project-alist "gpgweb"
   '(:components ("gpgweb-org" "gpgweb-other")))

   (add-hook 'org-export-before-processing-hook 'gpgweb-preprocess)))


(defun gpgweb-preprocess (backend)
  "Insert certain stuff before processing."
  (let ()
    (goto-char (point-min))
    (when (re-search-forward
           "^#\\+GPGWEB-NEED-SWDB\\b" 2048 t)
      (beginning-of-line)
      (kill-line 1)
      (insert (org-file-contents (concat gpgweb-root-dir "swdb.mac")
                                 'noerror)))))


(defun gpgweb-insert-header (title committed-at)
  "Insert the header.

COMMITTED-AT is the commit date string of the source file or nil
if not available."
  (goto-char (point-min))
  (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
               \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">
<head>
<title>" title "</title>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />\n")
  (when (and committed-at (>= (length committed-at) 10))
      (insert "<meta name=\"DC.Date\" content=\""
              (substring committed-at 0 10) "\" />\n"))
  (insert "<meta name=\"DC.Language\" content=\"en\" />
<meta name=\"DC.Title\" content=\"" title "\" />
<meta name=\"DC.Description\"
 content=\"GnuPG is a free implementation of OpenPGP\" />
<meta name=\"DC.Creator\" content=\"The People of the GnuPG Project\" />
<meta name=\"DC.Publisher\" content=\"The GnuPG Project\" />
<meta name=\"DC.Identifier\" content=\"https://gnupg.org/\" />
<meta name=\"DC.Rights\" content=\"https://gnupg.org/copying.html\" />
<link rel=\"stylesheet\" href=\"/share/site.css\" type=\"text/css\" />
</head>
<body>
"))

(defconst gpgweb-gnupg-menu-alist
  '(("/index.html"
     "Home"
     (("/index.html"                       "Home")
      ("/features.html"                    "Features")
      ("/news.html"                        "News")
      ("/people/index.html"                "People")
      ("/documentation/sites.html"         "Sites")
      ("/service.html"                     "Service")))
    ("/donate/index.html"
     "Donate"
     (("/donate/index.html"                "Donate")
      ("/donate/kudos.html"                "List of Donors")))
    ("/download/index.html"
     "Download"
     (("/download/index.html"              "Download")
      ("/download/integrity_check.html"    "Integrity&nbsp;Check")
      ("/download/supported_systems.html"  "Supported&nbsp;Systems")
      ("/download/release_notes.html"      "Release&nbsp;Notes")
      ("/download/mirrors.html"            "Mirrors")
      ("/download/git.html"                "GIT")))
    ("/documentation/index.html"
     "Support"
     (("/documentation/howtos.html"        "HOWTOs")
      ("/documentation/manuals.html"       "Manuals")
      ("/documentation/guides.html"        "Guides")
      ("/documentation/faqs.html"          "FAQs")
      ("/documentation/mailing-lists.html" "Mailing&nbsp;Lists")
      ("/documentation/bts.html"           "Bug&nbsp;Tracker")
      ("/documentation/security.html"      "Security")))
    ("/related_software/index.html"
     "Related software"
     (("/related_software/frontends.html"  "Frontends")
      ("/related_software/tools.html"      "Tools")
      ("/related_software/libraries.html"  "Libraries")
      ("/related_software/swlist.html"     "All")))
    ("/blog/index.html"
     "Blog"))
  "The definition of the gnupg.org menu structure.")

(defconst gpgweb-gnupg-bottom-menu-alist
  '(("/privacy-policy.html"
     "Privacy&nbsp;Policy"
     ())
    ("/imprint.html"
     "Imprint"
     ())
    ("/misc/index.html"
     "Archive"
     ())
    ("/sitemap.html"
     "Sitemap"
     ())
    ("/blog/index.html"
     "Blog"
     ()))
  "The definition of the gnupg.org bottom menu structure.")


(defun gpgweb--any-selected-menu-p (menu selected-file)
  "Return t if any item in MENU has been selected."
  (let ((item (car menu))
        res)
    (when menu
      (when item
        (when (string= (car item) selected-file)
            (setq res t))
        (when (caddr item)
          (when (gpgweb--any-selected-menu-p (caddr item) selected-file)
            (setq res t))))
      (when (gpgweb--any-selected-menu-p (cdr menu) selected-file)
        (setq res t)))
    res))


(defun gpgweb--selected-top-menu (menu selected-file)
  "Return the selected top menu or nil."
  (when menu
    (let ((item (car menu)))
      (if (and item
               (or (string= (car item) selected-file)
                   (gpgweb--any-selected-menu-p (caddr item) selected-file)))
          menu
        (gpgweb--selected-top-menu (cdr menu) selected-file)))))

(defun gpgweb--insert-menu (menu lvl selected-file)
  "Helper function to insert the menu."
  (when menu
    (let ((item (car menu)))
      (when item
        (dotimes (i (1+ lvl)) (insert "  "))
        (insert "  <li><a href=\"" (car item) "\"")
        (when (or (string= (car item) selected-file)
                  (gpgweb--any-selected-menu-p (caddr item) selected-file))
          (insert " class=\"selected\""))
        (insert  ">" (cadr item) "</a>\n")
        (when (caddr item)
          (dotimes (i (1+ lvl)) (insert "  "))
          (insert "  <ul class=\"sub-menu\">\n")
          (gpgweb--insert-menu (caddr item) (1+ lvl) selected-file)
          (dotimes (i (1+ lvl)) (insert "  "))
          (insert "  </ul>\n"))
        (dotimes (i (1+ lvl)) (insert "  "))
        (insert "  </li>\n")))
    (gpgweb--insert-menu (cdr menu) lvl selected-file)))


(defun gpgweb--insert-submenu (menu selected-file)
   "Helper function to insert the sub-menu."
   (when menu
     (let ((item (car menu)))
       (when item
         (insert "    <li><a href=\"" (car item) "\"")
         (when (or (string= (car item) selected-file)
                   (gpgweb--any-selected-menu-p (caddr item) selected-file))
           (insert " class=\"selected\""))
         (insert ">" (cadr item) "</a></li>\n")))
     (gpgweb--insert-submenu (cdr menu) selected-file)))


(defun gpgweb-insert-menu (selected-file)
  "Insert the menu structure into the HTML file."
  (goto-char (point-min))
  (when (re-search-forward "^<body>\n" nil t)
    (insert "<div id=\"wrapper\">
<div id=\"header\"><a href=\"/index.html\" class=\"logo\"
     ><img src=\"/share/logo-gnupg-light-purple-bg.png\"></a>&nbsp;</div>
<nav>
  <ul>
")
    (gpgweb--insert-menu gpgweb-gnupg-menu-alist 0 selected-file)
    (insert "  </ul>
</nav>
")
    (let ((m (caddr (car (gpgweb--selected-top-menu
                          gpgweb-gnupg-menu-alist selected-file)))))
      (when m
          (insert "<nav class=\"subnav\">\n  <ul>\n")
          (gpgweb--insert-submenu m selected-file)
          (insert "  </ul>\n</nav>\n")))
    (insert "<main>
<div id=\"content\">
")))


(defun gpgweb-blog-index (orgfile filelist)
  "Return the index of ORGFILE in FILELIST or nil if not found."
  (let (found
        (i 0))
    (while (and filelist (not found))
      (if (string= orgfile (car filelist))
          (setq found i))
      (setq i (1+ i))
      (setq filelist (cdr filelist)))
    found))

(defun gpgweb-blog-prev (fileidx filelist)
  "Return the chronological previous file at FILEIDX from FILELIST
with the suffixed replaced by \"html\"."
  (if (> fileidx 1)
      (concat (file-name-sans-extension (nth (1- fileidx) filelist)) ".html")))

(defun gpgweb-blog-next (orgfile filelist)
  "Return the chronological next file at FILEIDX from FILELIST
with the suffixed replaced by \"html\"."
  (if (< fileidx (1- (length filelist)))
      (concat (file-name-sans-extension (nth (1+ fileidx) filelist)) ".html")))

(defun gpgweb-fixup-blog (info orgfile filelist)
  "Insert the blog specific content.  INFO is the usual
plist. ORGFILE is the name of the current source file without the
directory part.  If FILELIST is a list it has an ordered list of
org filenames."
  (let ((authorstr (car (plist-get info :author)))
        (datestr   (car (plist-get info :date))))
    (goto-char (point-min))
    (if (re-search-forward "^<main>" nil t)
        (let* ((indexp (string= orgfile "index.org"))
               (fileidx (if (listp filelist)
                            (if indexp
                                (1- (length filelist))
                              (gpgweb-blog-index orgfile filelist))))
               (prevfile (if fileidx
                             (gpgweb-blog-prev fileidx filelist)))
               (nextfile (if (and fileidx (not indexp))
                           (gpgweb-blog-next fileidx filelist))))
          (move-beginning-of-line nil)
          (insert "<nav class=\"subnav\">\n  <ul>\n")
          (if prevfile
              (insert "    <li><a href=\"" prevfile "\">Previous</a></li>\n"))
          (insert
           "    <li><a href=\"/blog/index.html#blogindex\">Index</a></li>\n")
          (if nextfile
              (insert "    <li><a href=\"" nextfile "\">Next</a></li>\n"))
          (insert "  </ul>\n</nav>\n")))
    (if (and datestr authorstr)
        (if (re-search-forward "^<h2 id=.*\n" nil t)
            (insert "<p class=\"postdate\">Posted "
                    datestr
                    " by "
                    authorstr
                    "</p>\n")))))


(defun gpgweb-insert-footer (htmlfile committed-at blogmode)
  "Insert the footer.

HTMLFILE is HTML file name and COMMITTED-AT is the commit date
string of the source file or nil if not available."
  (let ((srcfile (concat "https://git.gnupg.org/cgi-bin/gitweb.cgi?"
                         "p=gnupg-doc.git;a=blob;f="
                         (if blogmode "misc/blog.gnupg.org/" "web/")
                         ;; The replace below is a hack to cope with
                         ;; blogmode where HTMLFILE is like "./foo.html".
                         (replace-regexp-in-string
                          "^\\./" "/"
                          (file-name-sans-extension htmlfile) t)
                         ".org"))
        (changed (if (and committed-at (>= (length committed-at) 10))
                     (substring committed-at 0 10)
                     "[unknown]")))
    (goto-char (point-max))
    (insert "</div><!-- end content -->
</main>
<div id=\"footer\">
  <div id=\"nav_bottom\">
  <ul>
")
    (gpgweb--insert-menu gpgweb-gnupg-bottom-menu-alist 0 nil)
    (insert "    </ul>
  </div>
")
    (insert "  <div class=\"footerbox\">
  <a><img src=\"/share/traueranzeige-g10_v2015.png\"
          width=\"200px\" height=\"73px\"
          alt=\"Traueranzeige: Wir nehmen Abschied von einem sicher geglaubten Freund, dem | Fernmeldegeheimniss | (Artikel 10 Grundgesetz) | * 23. Mai 1949, + 18. Dezember 2015\"
          title=\"Article 10 of the German constitution (communication privacy) is not anymore with us.\" /></a>
  <p></p>
  </div>
")
    (goto-char (point-min))
    (unless (search-forward "<!--disable-copyright-footer-->" nil t)
      (goto-char (point-max))
      (insert "  <div id=\"cpyright\">
    <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\"
      ><img alt=\"CC-BY-SA 3.0\" style=\"border: 0\"
            src=\"/share/cc-by-sa-3.0_80x15.png\"/></a>&nbsp;
    These web pages are
    Copyright 1998--2015 The GnuPG Project and licensed under a
    <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\"
    >Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.  See
    <a href=\"/copying.html\">copying</a> for details.
    Page <a href=\"" srcfile "\">source</a> last changed on " changed ".
  </div>\n"))
  (goto-char (point-max))
  (insert "</div>
</div><!-- end wrapper -->
</body>
</html>")))


;;; Post-process the generated HTML file:
;;;
;;; - Insert header and footer
;;; - Insert "class=selected" into the active menu entry
;;; - Fixup sitemap.
;;;
;;; If blogmode is not nil the output is rendered as a blog.  BLOGMODE
;;; may then contain an ordered list of org file names which are used
;;; to create the previous and Next links for an entry.
;;;
(defun gpgweb-postprocess-html (plist orgfile htmlfile blogmode)
  (let* ((visitingp (find-buffer-visiting htmlfile))
	 (work-buffer (or visitingp (find-file-noselect htmlfile)))
         (committed-at (shell-command-to-string
                        (concat "git log -1 --format='%ci' -- " orgfile))))
    (prog1 (with-current-buffer work-buffer
             (let ((fname (file-name-nondirectory htmlfile))
                   (fname-2 (replace-regexp-in-string
                              ".*/gpgweb-stage/\\(.*\\)$" "\\1" htmlfile t))
                   (title (org-publish-find-title orgfile)))
               ;; Insert header, menu, and footer.
               (gpgweb-insert-header title committed-at)
               (gpgweb-insert-menu fname-2)
               (if blogmode
                   (gpgweb-fixup-blog plist
                                      (file-name-nondirectory orgfile)
                                      blogmode))
               (gpgweb-insert-footer fname-2 committed-at blogmode)

               ; Fixup the sitemap
               (when (string-equal fname "sitemap.html")
                 (goto-char (point-min))
                 (while (re-search-forward
                         "^.*<li>.*>\\(GnuPG - \\).*<span.*$" nil t)
                   (replace-match "" t nil nil 1)))

               ; Due to a problem with the current org exporter (cases
               ; were we link to file mapped via a webserver alias) we
               ; have to use a full URL at some places in the org
               ; source.  We fix that up here.
               (goto-char (point-min))
               (while (re-search-forward
                       "href=\"\\(https://www.gnupg.org\\)/.*\"" nil t)
                 (replace-match "" t t nil 1))

               ; If the wideright flag is used, change <td> and <th>
               ; attributes.
               (goto-char (point-min))
               (when (search-forward "<!--table_data_wideright-->" nil t)
                 (goto-char (point-min))
                 (while (re-search-forward
                         "^<t[hd].*class=\"\\(right\\)\".*$" nil t)
                   (replace-match "right wideright" t nil nil 1)))

               ; And save the changes
               (basic-save-buffer))
      (unless visitingp (kill-buffer work-buffer))))))


;;;
;;; The publishing function used by the HTML exporter
;;;
(defun gpgweb-org-to-html (plist filename pub-dir)
  (gpgweb-postprocess-html plist
                           filename
                           (org-gpgweb-publish-to-html plist filename pub-dir)
                           nil))


;;;
;;; Turn the current buffer which has an org-mode blog entry into its
;;; rendered form and save it with the suffix .html.
;;;
(defun gpgweb-render-blog (&optional filelist)
  (interactive)
  (let* ((extplist '(:language "en"
                     :section-numbers nil
                     :tags nil
                     :with-toc nil))
         (orgfile (buffer-file-name))
         (plist (org-export-get-environment 'gpgweb nil extplist))
         (htmlfile (org-gpgweb-export-to-html nil nil nil t extplist)))
    (gpgweb-postprocess-html plist orgfile htmlfile (if filelist filelist t))))


;;;
;;; Publish all blog entries in the current directory
;;;
(defun gpgweb-publish-blogs ()
  (interactive)
  (let ((orgfiles (directory-files "." nil "^2[0-9]+-.*\.org$")))
    (dolist (file (cons "index.org" orgfiles))
      (let* ((visitingp (find-buffer-visiting file))
             (work-buffer (or visitingp (find-file-noselect file))))
        (with-current-buffer work-buffer
          (gpgweb-render-blog orgfiles)
          (basic-save-buffer))
        (unless visitingp
          (kill-buffer work-buffer))))))


;;;
;;; We don't do an upload directly.  Instead we only print the
;;; commands to do that.  In reality a cron jobs syncs the stage dir.
;;;
(defun gpgweb-upload ()
  (let ((stagedir (plist-get project-plist :publishing-directory)))
    (message "gpgweb  rootdir '%s'" gpgweb-root-dir)
    (message "gpgweb stagedir '%s'" stagedir)
    (message
     (concat "cd " gpgweb-root-dir " && cd " stagedir
             " && echo rsync -rlt --exclude \"*~\" ./ "
             "werner@trithemius.gnupg.org:"
             "/var/www/www/www.gnupg.org/htdocs/ ;"
             " echo ssh werner@trithemius.gnupg.org"
             " touch /var/www/www/www.gnupg.org/htdocs/donate/donors.dat"))
))

(provide 'gpgweb)
