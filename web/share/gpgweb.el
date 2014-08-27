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
<meta name=\"DC.Date\" content=\""
  (format-time-string "%Y-%m-%d" generated-at t) "\" />
<meta name=\"DC.Identifier\" content=\"https://gnupg.org/\" />
<meta name=\"DC.Rights\" content=\"https://gnupg.org/copying.html\" />
<link rel=\"stylesheet\" href=\"/share/site.css\" type=\"text/css\" />
</head>
<body>\n"))

(defun gpgweb-insert-menu ()
  (goto-char (point-min))
  (when (re-search-forward "^<body>\n" nil t)
    (insert "<div id=\"header\">&nbsp;</div>
<div id=\"leftColumn\">
  <nav>
  <ul>
    <li><a href=\"/index.html\"                 >Home</a></li>
      <ul>
        <li><a href=\"/features.html\" >Features</a></li>
        <li><a href=\"/news.html\"     >News</a></li>
        <li><a href=\"/service.html\"  >Service</a></li>
      </ul>
    <li><a href=\"/donate/index.html\"          >Donate</a></li>
      <ul>
        <li><a href=\"/donate/kudos.html\" >List of Donors</a></li>
      </ul>
    <li><a href=\"/download/index.html\"        >Download</a></li>
      <ul>
        <li><a href=\"/download/integrity_check.html\"
                                                >Integrity&nbsp;Check</a></li>
        <li><a href=\"/download/supported_systems.html\"
                                                >Supported&nbsp;Systems</a></li>
        <li><a href=\"/download/release_notes.html\"
                                                >Release&nbsp;Notes</a></li>
        <li><a href=\"/download/mirrors.html\"   >Mirrors</a></li>
        <li><a href=\"/download/cvs_access.html\" >GIT</a></li>
      </ul>
    <li><a href=\"/documentation/index.html\"   >Documentation</a></li>
      <ul>
        <li><a href=\"/documentation/howtos.html\"       >HOWTOs</a></li>
        <li><a href=\"/documentation/manuals.html\"      >Manuals</a></li>
        <li><a href=\"/documentation/guides.html\"       >Guides</a></li>
        <li><a href=\"/documentation/faqs.html\"         >FAQs</a></li>
        <li><a href=\"/documentation/mailing-lists.html\"
                                                 >Mailing&nbsp;Lists</a></li>
        <li><a href=\"/documentation/sites.html\"    >Sites</a></li>
        <li><a href=\"/documentation/bts.html\"      >Bug&nbsp;Tracker</a></li>
      </ul>
    <li><a href=\"/related_software/index.html\">Related software</a></li>
      <ul>
        <li><a href=\"/related_software/frontends.html\" >Frontends</a></li>
        <li><a href=\"/related_software/tools.html\"     >Tools</a></li>
        <li><a href=\"/related_software/libraries.html\" >Libraries</a></li>
        <li><a href=\"/related_software/swlist.html\"    >All</a></li>
      </ul>
    <li><a href=\"/blog/index.html\"            >Blog</a></li>
    <li><a href=\"/privacy-policy.html\"        >Privacy Policy</a></li>
    <li><a href=\"/misc/index.html\"            >Archive</a></li>
    <li><a href=\"/sitemap.html\"               >Sitemap</a></li>
  </ul>
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
                   (generated-at (current-time))
                   (tmppnt))
               ;; Insert the header and mark the active menu
               (gpgweb-insert-header title generated-at)
               (gpgweb-insert-menu)
               (setq tmppnt (point))
               (goto-char (point-min))
               (while (re-search-forward
                       (concat "href=\"" (regexp-quote fname-2) "\"")
                       tmppnt t)
                 (replace-match "\\& class=\"selected\"" t))

               ; Insert the footer
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
