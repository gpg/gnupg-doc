;;; gpgweb.el --- elisp helper code for the GnuPG web pages

(require 'org-exp)

(defun gpgweb-insert-header ()
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
      </ul>
    <li><a href=\"/misc/index.html\"            >Miscellaneous</a></li>
      <ul>
        <li><a href=\"/misc/links.html\"         >Links</a></li>
        <li><a href=\"/misc/logo-contest.html\"  >Logo&nbsp;Contest</a></li>
        <li><a href=\"/misc/thanks.html\"        >Thanks</a></li>
        <li><a href=\"/misc/donations.html\"     >Donations</a></li>
      </ul>
    <li><a href=\"/sitemap.html\"               >Sitemap</a></li>
  </ul>
  </nav>
</div>
<main>
"))

(defun gpgweb-insert-footer ()
  (goto-char (point-max))
  (insert "<div id=\"cpyright\">
    <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/\"
      ><img alt=\"CC-BY-SA 3.0\" style=\"border: 0\"
            src=\"share/cc-by-sa-3.0_80x15.png\"/></a><br/>
    These web pages are
    Copyright 1998--2013 The GnuPG Project<a href=\"copying.html\">¹</a>
    and licensed under a
    <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/\"
    >Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.  See
    <a href=\"copying.html\">copying<a/> for details.
    </div>
</main>
</body>
</html>
"))


;; Our publishing tweaks:
;;
;; - Substitute @FNAME@ by the actual file name.
;; - Substitute @MENU-ACTIVE@ by an empty string.
(defun gpgweb-postprocess-html ()
  (let ((fname (file-name-nondirectory (buffer-file-name)))
        (generated-at (org-today)))
    (gpgweb-insert-header)
    (gpgweb-insert-footer)
    (when (string-match "\\.\\([a-z][a-z]\\.\\)?html$" fname)
          (setq fname (substring fname 0 (match-beginning 0))))
    (goto-char (point-min))
    (while (search-forward "href=\"@FNAME@" nil t)
      (replace-match (concat "href=\"" ) t nil))
    (goto-char (point-min))
    (while (search-forward "@MENU-ACTIVE@" nil t)
      (replace-match "" t nil))))

(defun gpgweb-org-to-html (plist filename pub-dir)
  (add-hook 'org-export-html-final-hook
            'gpgweb-postprocess-html)
  (org-publish-org-to-html plist filename pub-dir)
  (remove-hook 'org-export-html-final-hook
               'gpgweb-postprocess-html))

(defun gpgweb-upload ()
  (let ((stagedir (plist-get project-plist :publishing-directory)))
    (message "gpgweb  rootdir '%s'" gpgweb-root-dir)
    (message "gpgweb stagedir '%s'" stagedir)
    (shell-command
     (concat "rsync -rlt --exclude \"*~\" " stagedir "/ "
             "werner@trithemius.gnupg.org:"
             "/var/www/www/w3.gnupg.org/htdocs/"))))


(provide 'gpgweb)
