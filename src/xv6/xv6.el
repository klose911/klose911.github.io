 (require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("xv6-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/xv6" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/xv6"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-sitemap nil
	 :section-numbers nil
         ;; :with-toc nil
         :html-head "<link rel=\"stylesheet\"
                       href=\"css/main.css\" type=\"text/css\"/>"
         :html-preamble t
	 :html-postamble klose-html-postamble
	 :htmlized-source t
         )

        ;; These are static files (images, pdf, etc)
        ("xv6-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/xv6/css"
         :recursive t
         :publishing-function org-publish-attachment
         )
	;; picture
        ("xv6-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/xv6/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif\\|svg"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/xv6/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("xv6" :components ("xv6-notes" "xv6-static" "xv6-pic"))
        )
      )

(setq org-footnote-re
      (concat "\\[\\(?:"
	      ;; Match inline footnotes.
	      (org-re "fn:\\([-_[:word:]]+\\)?:\\|")
	      ;; Match other footnotes.
	      ;; "\\(?:\\([0-9]+\\)\\]\\)\\|"
	      (org-re "\\(fn:[-_[:word:]]+\\)")
	      "\\)"))

(setq org-footnote-definition-re
      (org-re "^\\[\\(fn:[-_[:word:]]+\\)\\]"))

(defun xv6-publish (no-cache)
  "Publish xv6"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "xv6" t)
    (org-publish "xv6" nil)))
