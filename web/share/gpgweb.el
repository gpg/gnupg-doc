;;; gpgweb.el --- elisp helper code for the GnuPG web pages

(require 'org-exp)
;; cl-macs is required by ox-html.el but for whatever reasons not
;; autoloaded.
(load-library "cl-macs")

(defun gpgweb-setup-project ()
  (progn
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
     :auto-sitemap t
     :sitemap-title "GnuPG - Sitemap"
     :sitemap-sort-folders "last"
     :sitemap-file-entry-format "%t  @@html:<span id=\"smallnote\">@@(%d)@@html:</span>@@"
     :style-include-default nil
     :timestamp-file t
     :html-head "<link rel=\"stylesheet\" href=\"gnupg.css\" type=\"text/css\" />"
     :html-head-include-scripts nil))

   (aput 'org-publish-project-alist "gpgweb-other"
   '(:base-directory "."
     :base-extension "jpg\\|png\\|css\\|txt\\|rss"
     :recursive t
     :publishing-directory "../stage"
     :publishing-function org-publish-attachment
     :completion-function gpgweb-upload))

   (aput 'org-publish-project-alist "gpgweb"
   '(:components ("gpgweb-org" "gpgweb-other")))))




(defun gpgweb-insert-header (title)
  (goto-char (point-min))
  (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
               \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">
<head>
<title>" title "</title>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\"/>
<meta name=\"title\" content=\"" title "\"/>
<meta name=\"generator\" content=\"Org-mode\"/>
<meta name=\"generated\" content=\"" generated-at "\"/>
<meta name=\"author\" content=\"Werner Koch\"/>
<meta name=\"description\" content=\"\"/>
<meta name=\"keywords\" content=\"\"/>
<link rel=\"stylesheet\" href=\"/share/site.css\" type=\"text/css\" />
</head>
<body>
<div id=\"header\">&nbsp;</div>
<div id=\"leftColumn\">
  <nav>
  <ul>
    <li><a href=\"/index.html\"                 >Home</a></li>
      <ul>
        <li><a href=\"/features.html\" >Features</a></li>
        <li><a href=\"/news.html\"     >News</a></li>
        <li><a href=\"/service.html\"  >Service</a></li>
        <li><a href=\"/legal.html\"    >Legal</a></li>
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
    <li><a href=\"/misc/index.html\"            >Miscellaneous</a></li>
      <ul>
        <li><a href=\"/misc/links.html\"         >Links</a></li>
        <li><a href=\"/misc/logo-contest.html\"  >Logo&nbsp;Contest</a></li>
        <li><a href=\"/misc/thanks.html\"        >Thanks</a></li>
        <li><a href=\"/misc/donations.html\"     >Donations</a></li>
      </ul>
    <li><a href=\"http://blog.gnupg.org/\"      >Blog</a></li>
    <li><a href=\"/privacy-policy.html\"        >Privacy Policy</a></li>
    <li><a href=\"/sitemap.html\"               >Sitemap</a></li>
  </ul>
  </nav>
</div>
<main>
"))

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


;;; The old pwik code.
;;;  (insert "
;;;<script type=\"text/javascript\">
;;;  var _paq = _paq || [];
;;;  _paq.push([\"trackPageView\"]);
;;;  _paq.push([\"enableLinkTracking\"]);
;;;
;;;  (function() {
;;;    var u=((\"https:\" == document.location.protocol) ? \"https\" : \"http\")
;;;            + \"://alberti.gnupg.org/piwik/\";
;;;    _paq.push([\"setTrackerUrl\", u+\"piwik.php\"]);
;;;    _paq.push([\"setSiteId\", \"1\"]);
;;;    var d=document,
;;;        g=d.createElement(\"script\"),
;;;        s=d.getElementsByTagName(\"script\")[0];
;;;    g.type=\"text/javascript\";
;;;    g.defer=true;
;;;    g.async=true;
;;;    g.src=u+\"piwik.js\";
;;;    s.parentNode.insertBefore(g,s);
;;;  })();
;;;</script>
;;;</main>
;;;</body>
;;;</html>
;;;")

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
                   (generated-at (org-today))
                   (tmppnt))
               ;; Insert the header and mark the active menu
               (gpgweb-insert-header title)
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
               (basic-save-buffer))
      (unless visitingp (kill-buffer work-buffer))))))

;;;
;;; The publishing function used by the HTML exporter
;;;
(defun gpgweb-org-to-html (plist filename pub-dir)
  (gpgweb-postprocess-html plist filename
                           (org-html-publish-to-html plist filename pub-dir)))


(defun gpgweb-upload ()
  (let ((stagedir (plist-get project-plist :publishing-directory)))
    (message "gpgweb  rootdir '%s'" gpgweb-root-dir)
    (message "gpgweb stagedir '%s'" stagedir)
    (shell-command
     (concat "cd " gpgweb-root-dir " && cd " stagedir
             "&& rsync -rlt --exclude \"*~\" ./ "
             "werner@trithemius.gnupg.org:"
             "/var/www/www/www.gnupg.org/htdocs/"))))


(provide 'gpgweb)

;; commit 6f5180bd9fc230a31913cbdb9a4dd48cc247adc2
;; Author: Rick Frankel <rick@rickster.com>
;; Date:   Wed Oct 2 18:26:27 2013 -0400
;;
;;     Fix escaping of links in html export.
;;
;;     * lisp/ox-html.el (org-html-link): Unescape org-escaped links an
;;       re-escape for html (browser).
;;
;;
;; diff --git a/lisp/ox-html.el b/lisp/ox-html.el
;; index 66862bc..0600204 100644
;; --- a/lisp/ox-html.el
;; +++ b/lisp/ox-html.el
;; @@ -2624,7 +2624,9 @@ INFO is a plist holding contextual information.  See
;;          (path
;;           (cond
;;            ((member type '("http" "https" "ftp" "mailto"))
;; -           (concat type ":" raw-path))
;; +           (org-link-escape
;; +            (org-link-unescape
;; +             (concat type ":" raw-path)) org-link-escape-chars-browser))
;;            ((string= type "file")
;;             ;; Treat links to ".org" files as ".html", if needed.
;;             (setq raw-path
