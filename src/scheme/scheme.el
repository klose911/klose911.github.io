(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("scheme-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/scheme" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/scheme"
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
        ("scheme-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/scheme/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; picture
        ("scheme-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/scheme/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/scheme/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("scheme" :components ("scheme-notes" "scheme-static" "scheme-pic"))
        )
      )

(defun scheme-publish (no-cache)
  "Publish scheme"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "scheme" t)
    (org-publish "scheme" nil)))
