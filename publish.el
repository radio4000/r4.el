;;; publish.el --- Publish the r4.el website  -*- lexical-binding: t; -*-

;;; Commentary:

;; Export readme.org (the home page) and source.org (the source code)
;; to HTML in ./public.  Run with: make site

;;; Code:

(require 'package)
(setq package-user-dir (expand-file-name ".site-packages" default-directory))
(package-initialize)
(unless (package-installed-p 'htmlize)
  (package-refresh-contents)
  (package-install 'htmlize))

;; Colors of the highlighted source blocks.  Themes only color
;; graphical or color terminals, not batch Emacs, so set them here.
(dolist (spec '((font-lock-comment-face "#6a737d")
                (font-lock-comment-delimiter-face "#6a737d")
                (font-lock-doc-face "#6a737d")
                (font-lock-string-face "#032f62")
                (font-lock-keyword-face "#d73a49")
                (font-lock-builtin-face "#005cc5")
                (font-lock-constant-face "#005cc5")
                (font-lock-function-name-face "#6f42c1")
                (font-lock-type-face "#6f42c1")
                (font-lock-variable-name-face "#e36209")
                (font-lock-warning-face "#b31d28")))
  (set-face-attribute (car spec) nil :inherit nil :foreground (cadr spec)
                      :weight 'normal :slant 'normal :underline nil))

(require 'org)
(require 'ox-publish)

(setq org-export-with-section-numbers nil
      org-export-with-smart-quotes t
      org-export-with-author nil
      org-export-with-creator nil
      org-export-with-date nil
      org-export-time-stamp-file nil
      org-src-preserve-indentation t
      org-html-htmlize-output-type 'inline-css
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-validation-link nil
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-head "<link rel=\"stylesheet\" href=\"styles.css\">"
      org-html-divs '((preamble "header" "preamble")
                      (content "main" "content")
                      (postamble "footer" "postamble"))
      org-html-container-element "section"
      org-html-preamble "<nav><a href=\"index.html\">r4.el</a> <a href=\"features.html\">Features</a> <a href=\"source.html\">Source</a> <a href=\"https://github.com/radio4000/r4.el\">Repository</a> <a href=\"https://radio4000.com\">radio4000.com</a></nav>"
      org-html-postamble nil)

(setq org-publish-project-alist
      `(("r4-pages"
         :base-directory "."
         :base-extension "org"
         :exclude ".*"
         :include ("readme.org" "features.org" "source.org")
         :publishing-directory "./public"
         :publishing-function org-html-publish-to-html)
        ("r4-static"
         :base-directory "."
         :base-extension "css"
         :exclude ".*"
         :include ("styles.css")
         :publishing-directory "./public"
         :publishing-function org-publish-attachment)
        ("r4-site" :components ("r4-pages" "r4-static"))))

(org-publish "r4-site" t)
(copy-file "public/readme.html" "public/index.html" t)

;;; publish.el ends here
