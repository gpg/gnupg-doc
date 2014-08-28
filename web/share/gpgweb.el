;;; gpgweb.el --- elisp helper code for the GnuPG web pages

(require 'org-exp)

(defun gpgweb-setup-project ()
  "Set up an org-publish project for the gnupg.org website."
  (progn
   (require 'ox-gpgweb (concat gpgweb-root-dir "share/ox-gpgweb.el"))
   (aput 'org-publish-project-alist "gpgweb-org"
   '(:base-directory "~/s/gnupg-doc/web"
     :base-extension "org"
     :language "en"
     :html-extension "html"
     :recursive t
     :publishing-directory "../stage"
     :publishing-function gpgweb-org-to-html
     :body-only t
     :section-numbers nil
     :tags nil
     :with-toc nil
     :makeindex t
     :auto-sitemap nil
     :sitemap-title "GnuPG - Sitemap"
     :sitemap-sort-folders "last"
     :sitemap-file-entry-format "%t  @@html:<span id=\"smallnote\">@@(%d)@@html:</span>@@"
     :style-include-default nil
     :timestamp-file nil
     :html-head "<link rel=\"stylesheet\" href=\"gnupg.css\" type=\"text/css\" />"
     :html-head-include-scripts nil))

   (aput 'org-publish-project-alist "gpgweb-other"
   '(:base-directory "."
     :base-extension "jpg\\|png\\|css\\|txt\\|rss\\|lst\\|sig"
     :recursive t
     :publishing-directory "../stage"
     :publishing-function org-publish-attachment
     :completion-function gpgweb-upload))

   (aput 'org-publish-project-alist "gpgweb"
   '(:components ("gpgweb-org" "gpgweb-other")))))


(defun gpgweb-insert-header (title generated-at)
  "Insert the header.

Note that using GENERATED-AT is highly problematic because rsync
would the always update the file.  IF would be better to use the
file date of the source file but that has the problem that git
does not track it.  We need to find a solution for this unless we
can do without DC.Date.  A possible way to fix this is to use a
source file property which could be updated using Emacs features.
Or set a new date only if the file really changed. "
  (goto-char (point-min))
  (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
               \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">
<head>
<title>" title "</title>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\"/>
<meta name=\"DC.Language\" content=\"en\" />
<meta name=\"DC.Title\" content=\"" title "\"/>
<meta name=\"DC.Description\"
 content=\"GnuPG is a free implementation of OpenPGP\" />
<meta name=\"DC.Creator\" content=\"The People of the GnuPG Project\" />
<meta name=\"DC.Publisher\" content=\"The GnuPG Project\" />
<meta name=\"DC.Identifier\" content=\"https://gnupg.org/\" />
<meta name=\"DC.Rights\" content=\"https://gnupg.org/copying.html\" />
<link rel=\"stylesheet\" href=\"/share/site.css\" type=\"text/css\" />
</head>
<body>\n"))
;;; <meta name=\"DC.Date\" content=\""
;;;   (format-time-string "%Y-%m-%d" generated-at t) "\" />


(defconst gpgweb-gnupg-menu-alist
  '(("/index.html"
     "Home"
     (("/features.html"                    "Features")
      ("/news.html"                        "News")
      ("/service.html"                     "Service")))
    ("/donate/index.html"
     "Donate"
     (("/donate/kudos.html"                "List of Donors")))
    ("/download/index.html"
     "Download"
     (("/download/integrity_check.html"    "Integrity&nbsp;Check")
      ("/download/supported_systems.html"  "Supported&nbsp;Systems")
      ("/download/release_notes.html"      "Release&nbsp;Notes")
      ("/download/mirrors.html"            "Mirrors")
      ("/download/cvs_access.html"         "GIT")))
    ("/documentation/index.html"
     "Documentation"
     (("/documentation/howtos.html"        "HOWTOs")
      ("/documentation/manuals.html"       "Manuals")
      ("/documentation/guides.html"        "Guides")
      ("/documentation/faqs.html"          "FAQs")
      ("/documentation/mailing-lists.html" "Mailing&nbsp;Lists")
      ("/documentation/sites.html"         "Sites")
      ("/documentation/bts.html"           "Bug&nbsp;Tracker")))
    ("/related_software/index.html"
     "Related software"
     (("/related_software/frontends.html"  "Frontends")
      ("/related_software/tools.html"      "Tools")
      ("/related_software/libraries.html"  "Libraries")
      ("/related_software/swlist.html"     "All")))
    ("/blog/index.html"
     "Blog"
     ())
    ("/privacy-policy.html"
     "Privacy&nbsp;Policy"
     ())
    ("/misc/index.html"
     "Archive"
     ())
    ("/sitemap.html"
     "Sitemap"
     ()))
  "The definition of the gnupg.org menu structure.")

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


(defun gpgweb--insert-menu (menu lvl selected-file)
  "Helper function to insert the menu."
  (when menu
    (let ((item (car menu))
          sel)
      (when item
        (dotimes (i lvl) (insert "  "))
        (insert "    <li><a href=\"" (car item) "\"")
        (when (string= (car item) selected-file)
          (setq sel t)
          (insert " class=\"selected\""))
        (insert  ">" (cadr item) "</a></li>\n")
        (when (and (caddr item)
                   (or
                    sel
                    (gpgweb--any-selected-menu-p (caddr item) selected-file)))
          (dotimes (i (1+ lvl)) (insert "  "))
          (insert "  <ul>\n")
          (gpgweb--insert-menu (caddr item) (1+ lvl) selected-file)
          (dotimes (i (1+ lvl)) (insert "  "))
          (insert "  </ul>\n"))))
    (gpgweb--insert-menu (cdr menu) lvl selected-file)))

(defun gpgweb-insert-menu (selected-file)
  "Insert the menu structure into the HTML file."
  (goto-char (point-min))
  (when (re-search-forward "^<body>\n" nil t)
    (insert "<div id=\"header\">&nbsp;</div>
<div id=\"leftColumn\">
  <nav>
  <ul>
")
    (gpgweb--insert-menu gpgweb-gnupg-menu-alist 0 selected-file)
    (insert "  </ul>
  </nav>
</div>
<main>
")))

(defun gpgweb-insert-footer ()
  (goto-char (point-min))
  (unless (search-forward "<!--disable-copyright-footer-->" nil t)
    (goto-char (point-max))
    (insert "<div id=\"cpyright\">
    <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/\"
      ><img alt=\"CC-BY-SA 3.0\" style=\"border: 0\"
            src=\"/share/cc-by-sa-3.0_80x15.png\"/></a><br/>
    These web pages are
    Copyright 1998--2014 The GnuPG Project<a href=\"/copying.html\">¹</a>
    and licensed under a
    <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/\"
    >Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.  See
    <a href=\"/copying.html\">copying</a> for details.
</div>
</main>
</body>
</html>")))


;;; Post-process the generated HTML file:
;;;
;;; - Insert header and footer
;;; - Insert "class=selected" into the active menu entry
;;; - Fixup sitemap.
(defun gpgweb-postprocess-html (plist orgfile htmlfile)
  (let* ((visitingp (find-buffer-visiting htmlfile))
	 (work-buffer (or visitingp (find-file-noselect htmlfile))))
    (prog1 (with-current-buffer work-buffer
             (let ((fname (file-name-nondirectory htmlfile))
                   (fname-2 (replace-regexp-in-string
                             ".*/stage\\(/.*\\)$" "\\1" htmlfile t))
                   (title (org-publish-find-title orgfile))
                   (generated-at (current-time)))
               ;; Insert header, menu, and footer.
               (gpgweb-insert-header title generated-at)
               (gpgweb-insert-menu fname-2)
               (gpgweb-insert-footer)

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

               ; And save the changes
               (basic-save-buffer))
      (unless visitingp (kill-buffer work-buffer))))))

;;;
;;; The publishing function used by the HTML exporter
;;;
(defun gpgweb-org-to-html (plist filename pub-dir)
  (gpgweb-postprocess-html plist filename
                           (org-gpgweb-publish-to-html plist filename pub-dir)))


(defun gpgweb-upload ()
  (let ((stagedir (plist-get project-plist :publishing-directory)))
    (message "gpgweb  rootdir '%s'" gpgweb-root-dir)
    (message "gpgweb stagedir '%s'" stagedir)
    (shell-command
     (concat "cd " gpgweb-root-dir " && cd " stagedir
             "&& rsync -rlt --exclude \"*~\" ./ "
             "werner@trithemius.gnupg.org:"
             "/var/www/www/www.gnupg.org/htdocs/ ;"
             " ssh werner@trithemius.gnupg.org"
             " touch /var/www/www/www.gnupg.org/htdocs/donate/donors.dat"))
))

(provide 'gpgweb)
