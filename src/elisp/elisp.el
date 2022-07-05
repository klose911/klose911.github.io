(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("elisp-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/elisp" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/elisp"
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
        ("elisp-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/elisp/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; picture
        ("elisp-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/elisp/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/elisp/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("elisp" :components ("elisp-notes" "elisp-static" "elisp-pic"))
        )
      )

(defun elisp-publish (no-cache)
  "Publish elisp"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "elisp" t)
    (org-publish "elisp" nil)))
