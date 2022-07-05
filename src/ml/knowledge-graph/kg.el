(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("kg-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/ml/knowledge-graph" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/ml/knowledge-graph"
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
        ("kg-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/ml/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; picture
        ("kg-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/ml/knowledge-graph/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/ml/knowledge-graph/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("kg" :components ("kg-notes" "kg-static" "kg-pic"))
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

(defun kg-publish (no-cache)
  "Publish kg"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "kg" t)
    (org-publish "kg" nil)))

