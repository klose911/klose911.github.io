(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("elisp-manual-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/elisp/manual" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/elisp/manual"
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
        ("elisp-manual-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/elisp/manual/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; picture
        ("elisp-manual-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/elisp/manual/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/elisp/manual/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("elisp-manual" :components ("elisp-manual-notes" "elisp-manual-static" "elisp-manual-pic"))
        )
      )

(defun elisp-manual-publish (no-cache)
  "Publish elisp manual"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "elisp-manual" t)
    (org-publish "elisp-manual" nil)))
