;;; gpgweb.el --- elisp helper code for the GnuPG web pages

(require 'org-exp)

(defun gpgweb-insert-header ()
  (goto-char (point-min))
  (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
               \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">
<head>
<title>@TITLE@</title>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\"/>
<meta name=\"title\" content=\"@TITLE@\"/>
<meta name=\"generator\" content=\"Org-mode\"/>
<meta name=\"generated\" content=\"2013-03-22T17:25+0100\"/>
<meta name=\"author\" content=\"Werner Koch\"/>
<meta name=\"description\" content=\"\"/>
<meta name=\"keywords\" content=\"\"/>
<link rel=\"stylesheet\" href=\"share/site.css\" type=\"text/css\" />
</head>
<body>
<div id=\"content\">
<div id=\"header\">&nbsp;</div>
<div id=\"leftColumn\">
  <div id=\"navigation\">
  <ul>
    <li><a href=\"index.html\"   >home</a></li>
    <li><a href=\"sitemap.html\" >sitemap</a></li>
    <li><a href=\"contact.html\" >contact</a></li>
  </ul>
  </div>
</div>
<div id=\"mainText\">
<p id=\"kicker\">@TITLE@</p>
"))

(defun gpgweb-insert-footer ()
  (goto-char (point-max))
  (insert "<div id=\"cpyright\">
  <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/\"
    ><img alt=\"CC-BY-SA 3.0\" style=\"border: 0\"
          src=\"share/cc-by-sa-3.0_80x15.png\"/></a>
   <br/>These web pages are Copyright 1998--2013 The GnuPG Project and
  licensed under a
  <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/\"
   >Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.  See
   <a href=\"copying.html\">copying<a/> for details.
</div>
</div>
</div>
</body>
</html>
"))


(defun gpgweb-postprocess-html ()
  (gpgweb-insert-header)
  (gpgweb-insert-footer)
  (let ((fname (file-name-nondirectory (buffer-file-name))))
    (when (string-match "\\.\\([a-z][a-z]\\.\\)?html$" fname)
          (setq fname (substring fname 0 (match-beginning 0))))
    (goto-char (point-min))
    (while (search-forward "href=\"@FNAME@" nil t)
      (replace-match (concat "href=\"" fname) t nil))
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
   ())
;  (let ((stagedir (plist-get project-plist :publishing-directory)))
;    (message "gpgweb stagedir '%s'" stagedir)
;    (shell-command
;     (concat "rsync -rlt --exclude \"*~\" " stagedir "/ "
;             "werner@trithemius.gnupg.org:"
;             "/var/www/all/preview.gnupg.org/htdocs/"))))


(provide 'gpgweb)
