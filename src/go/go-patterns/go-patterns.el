(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("go-patterns-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/go/go-patterns" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/go/go-patterns"
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
        ("go-patterns-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/go/go-patterns/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; picture
        ("go-patterns-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/go/go-patterns/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/go/go-patterns/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("go-patterns" :components ("go-patterns-notes" "go-patterns-static" "go-patterns-pic"))
        )
      )

(defun go-patterns-publish (no-cache)
  "Publish Go-Patterns go"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "go-patterns" t)
    (org-publish "go-patterns" nil)))
