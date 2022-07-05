(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("rust-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/rust" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/rust"
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
        ("rust-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/rust/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; These are static files (images, pdf, etc)
        ("rust-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/rust/pic" ;; Change this to your local dir
         :base-extension "svg\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/rust/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("rust" :components ("rust-notes" "rust-static" "rust-pic"))
        )
      )

(defun rust-publish (no-cache)
  "Publish rust"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "rust" t)
    (org-publish "rust" nil)))
