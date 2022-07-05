(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("kafka-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/kafka" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/kafka"
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
        ("kafka-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/kafka/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; picture
        ("kafka-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/kafka/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/kafka/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("kafka" :components ("kafka-notes" "kafka-static" "kafka-pic"))
        )
      )

(defun kafka-publish (no-cache)
  "Publish kafka"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "kafka" t)
    (org-publish "kafka" nil)))
