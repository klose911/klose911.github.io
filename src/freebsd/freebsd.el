(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("freebsd-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/freebsd" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/freebsd"
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
        ("freebsd-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/freebsd/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("freebsd" :components ("freebsd-notes" "freebsd-static"))
        )
      )

(defun freebsd-publish (no-cache)
  "Publish freebsd"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "freebsd" t)
    (org-publish "freebsd" nil)))
