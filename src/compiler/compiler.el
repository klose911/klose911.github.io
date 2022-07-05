(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("compiler-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/compiler" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/compiler"
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
        ("compiler-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/compiler/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; picture
        ("compiler-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/compiler/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/compiler/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("compiler" :components ("compiler-notes" "compiler-static" "compiler-pic"))
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

(defun compiler-publish (no-cache)
  "Publish compiler"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "compiler" t)
    (org-publish "compiler" nil)))

