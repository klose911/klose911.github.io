(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("slt-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/scheme/tutorial" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/scheme/tutorial"
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
        ("slt-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/scheme/tutorial/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

      	;; picture
        ("slt-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/scheme/tutorial/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/scheme/tutorial/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("slt" :components ("slt-notes" "slt-static" "slt-pic"))
        )
      )

(defun slt-publish (no-cache)
  "Publish Scheme Learning Tutorial"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "slt" t)
    (org-publish "slt" nil)))
