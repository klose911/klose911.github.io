(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("go-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/go" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/go"
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
        ("go-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/go/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("go" :components ("go-notes" "go-static"))
        )
      )

(defun go-publish (no-cache)
  "Publish go"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "go" t)
    (org-publish "go" nil)))
