(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("v2ray-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/v2ray" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/v2ray"
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
        ("v2ray-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/v2ray/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; These are static files (images, pdf, etc)
        ("v2ray-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/v2ray/pic" ;; Change this to your local dir
         :base-extension "svg\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/v2ray/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("v2ray" :components ("v2ray-notes" "v2ray-static" "v2ray-pic"))
        )
      )

(defun v2ray-publish (no-cache)
  "Publish v2ray"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "v2ray" t)
    (org-publish "v2ray" nil)))
