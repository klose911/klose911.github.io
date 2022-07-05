(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("kubernates-handbook-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/docker/kubernates-handbook" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/docker/kubernates-handbook"
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
        ("kubernates-handbook-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/docker/kubernates-handbook/css"
         :recursive t
         :publishing-function org-publish-attachment
         )
	;; picture
        ("kubernates-handbook-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/docker/kubernates-handbook/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif\\|svg"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/docker/kubernates-handbook/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("kubernates-handbook" :components ("kubernates-handbook-notes" "kubernates-handbook-static" "kubernates-handbook-pic"))
        )
      )

(defun kubernates-handbook-publish (no-cache)
  "Publish kubernates-handbook"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "kubernates-handbook" t)
    (org-publish "kubernates-handbook" nil)))
