(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("kubernates-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/docker/kubernates" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/docker/kubernates"
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
        ("kubernates-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/docker/kubernates/css"
         :recursive t
         :publishing-function org-publish-attachment
         )
	;; picture
        ("kubernates-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/docker/kubernates/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif\\|svg"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/docker/kubernates/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("kubernates" :components ("kubernates-notes" "kubernates-static" "kubernates-pic"))
        )
      )

(defun kubernates-publish (no-cache)
  "Publish kubernates"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "kubernates" t)
    (org-publish "kubernates" nil)))
