(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("effj-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/effj" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/effj"
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
        ("effj-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/effj/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("effj" :components ("effj-notes" "effj-static"))
        )
      )

(defun effj-publish (no-cache)
  "Publish effj"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "effj" t)
    (org-publish "effj" nil)))
