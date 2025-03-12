(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("kernel-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/linux/kernel" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/linux/kernel"
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
        ("kernel-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/linux/kernel/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; These are static files (images, pdf, etc)
        ("kernel-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/linux/kernel/pic" ;; Change this to your local dir
         :base-extension "svg\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/linux/kernel/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("kernel" :components ("kernel-notes" "kernel-static" "kernel-pic"))
        )
      )

(defun kernel-publish (no-cache)
  "Publish kernel"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "kernel" t)
    (org-publish "kernel" nil)))
