(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("docker-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/docker" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/docker"
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
        ("docker-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/docker/css"
         :recursive t
         :publishing-function org-publish-attachment
         )
	;; picture
        ("docker-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/docker/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif\\|svg"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/docker/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("docker" :components ("docker-notes" "docker-static" "docker-pic"))
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

(defun docker-publish (no-cache)
  "Publish docker"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "docker" t)
    (org-publish "docker" nil)))
