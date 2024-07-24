(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" default))
 '(display-battery-mode 1)
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-export-backends '(ascii beamer html icalendar latex man md odt))
 '(org-format-latex-header
   "\\documentclass{article}\12\\usepackage[usenames]{color}\12[DEFAULT-PACKAGES]\12\\usepackage{fancyhdr,color}\12\\usepackage{outlines}\12\\usepackage{graphicx}\12\\usepackage{fancyhdr,color}\12\\usepackage{amsmath}\12\\usepackage{listings}\12\\usepackage{amssymb}\12[PACKAGES] \12\12\\pagestyle{empty}             % do not remove\12% The settings below are copied from fullpage.sty\12\\setlength{\\textwidth}{\\paperwidth}\12\\addtolength{\\textwidth}{-3cm}\12\\setlength{\\oddsidemargin}{1.5cm}\12\\addtolength{\\oddsidemargin}{-2.54cm}\12\\setlength{\\evensidemargin}{\\oddsidemargin}\12\\setlength{\\textheight}{\\paperheight}\12\\addtolength{\\textheight}{-\\headheight}\12\\addtolength{\\textheight}{-\\headsep}\12\\addtolength{\\textheight}{-\\footskip}\12\\addtolength{\\textheight}{-3cm}\12\\setlength{\\topmargin}{1.5cm}\12\\addtolength{\\topmargin}{-2.54cm}")
 '(org-latex-default-packages-alist
   '(("" "amsmath" t
      ("lualatex" "xetex"))
     ("" "fontspec" t
      ("lualatex" "xetex"))
     ("AUTO" "inputenc" t
      ("pdflatex"))
     ("T1" "fontenc" t
      ("pdflatex"))
     ("" "graphicx" t nil)
     ("" "longtable" nil nil)
     ("" "wrapfig" nil nil)
     ("" "rotating" nil nil)
     ("normalem" "ulem" t nil)
     ("" "amsmath" t
      ("pdflatex"))
     ("" "amssymb" t
      ("pdflatex"))
     ("" "capt-of" nil nil)
     ("" "hyperref" nil nil)
     ("" "amssymb" nil nil)
     ("" "listings" nil nil)
     ("" "graphicx" nil nil)
     ("" "fancyhdr" nil nil)
     ("" "color" nil nil)))
 '(org-pretty-entities t)
 '(org-toc-follow-mode t)
 '(org-toc-info-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:height 2.0))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.6))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.4))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.3))))
 '(org-level-6 ((t (:inherit outline-5 :height 1.2))))
 '(org-level-7 ((t (:inherit outline-5 :height 1.1))))
 '(org-modern-statistics ((t (:inherit org-checkbox-statistics-todo)))))
(put 'customize-variable 'disabled nil)
