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


(defun gpgweb-insert-header (title committed-at)
  "Insert the header.

COMMITED-AT is the commit date string of the source file or nil
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
<body>\n"))

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
      ("/related_software/swlist.html"     "All"))))
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
          (insert "  <ul>\n")
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
    (insert "<div id=\"header\">&nbsp;</div>
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
")))


(defun gpgweb-insert-footer ()
  (goto-char (point-max))
  (insert "</main>
<div id=\"footer\">
  <p>This site is currently undergoing a complete redesign.
     We apologize for any inconveniences like broken links
     or bad formatting.  Please do not report such problems as we are probably
     already aware of them.  (2014-05-28 wk)</p>
  <div id=\"nav_bottom\">
  <ul>
")
  (gpgweb--insert-menu gpgweb-gnupg-bottom-menu-alist 0 nil)
  (insert "  </ul>
  </div>
")
  (goto-char (point-min))
  (unless (search-forward "<!--disable-copyright-footer-->" nil t)
    (goto-char (point-max))
    (insert "  <div id=\"cpyright\">
    <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/\"
      ><img alt=\"CC-BY-SA 3.0\" style=\"border: 0\"
            src=\"/share/cc-by-sa-3.0_80x15.png\"/></a>&nbsp;
    These web pages are
    Copyright 1998--2014 The GnuPG Project<a href=\"/copying.html\">¹</a>
    and licensed under a
    <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/\"
    >Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.  See
    <a href=\"/copying.html\">copying</a> for details.
  </div>\n"))
  (goto-char (point-max))
  (insert "</div>
</body>
</html>"))


;;; Post-process the generated HTML file:
;;;
;;; - Insert header and footer
;;; - Insert "class=selected" into the active menu entry
;;; - Fixup sitemap.
(defun gpgweb-postprocess-html (plist orgfile htmlfile)
  (let* ((visitingp (find-buffer-visiting htmlfile))
	 (work-buffer (or visitingp (find-file-noselect htmlfile)))
         (committed-at (shell-command-to-string
                        (concat "git log -1 --format='%ci' -- " orgfile))))
    (prog1 (with-current-buffer work-buffer
             (let ((fname (file-name-nondirectory htmlfile))
                   (fname-2 (replace-regexp-in-string
                             ".*/stage\\(/.*\\)$" "\\1" htmlfile t))
                   (title (org-publish-find-title orgfile)))
               ;; Insert header, menu, and footer.
               (gpgweb-insert-header title committed-at)
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
