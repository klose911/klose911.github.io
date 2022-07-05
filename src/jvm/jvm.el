
(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("jvm-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/jvm" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/jvm"
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
        ("jvm-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/jvm/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; picture
        ("jvm-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/jvm/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/jvm/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("jvm" :components ("jvm-notes" "jvm-static" "jvm-pic"))
        )
      )

(defun jvm-publish (no-cache)
  "Publish jvm"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "jvm" t)
    (org-publish "jvm" nil)))
