(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("istio-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/istio" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/istio"
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
        ("istio-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/istio/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; picture
        ("istio-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/istio/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif\\|svg"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/istio/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("istio" :components ("istio-notes" "istio-static" "istio-pic"))
        )
      )

(defun istio-publish (no-cache)
  "Publish istio"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "istio" t)
    (org-publish "istio" nil)))
