(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("deep-learning-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/ml/deep-learning" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/ml/deep-learning"
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
        ("deep-learning-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/ml/deep-learning/css"
         :recursive t
         :publishing-function org-publish-attachment
         )
	;; picture
        ("deep-learning-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/ml/deep-learning/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif\\|svg"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/ml/deep-learning/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("deep-learning" :components ("deep-learning-notes" "deep-learning-static" "deep-learning-pic"))
        )
      )

(defun deep-learning-publish (no-cache)
  "Publish deep-learning"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "deep-learning" t)
    (org-publish "deep-learning" nil)))
