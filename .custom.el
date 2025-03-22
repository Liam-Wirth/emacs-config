(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87"
     "d6b934330450d9de1112cbb7617eaf929244d192c4ffb1b9e6b63ad574784aad"
     "fbf73690320aa26f8daffdd1210ef234ed1b0c59f3d001f342b9c0bbf49f531c"
     "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
     "e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40"
     "a5270d86fac30303c5910be7403467662d7601b821af2ff0c4eb181153ebfc0a"
     "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710"
     "871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8"
     "ba323a013c25b355eb9a0550541573d535831c557674c8d59b9ac6aa720c21d3"
     "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851"
     "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b"
     "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" default))
 '(display-battery-mode 1)
 '(elcord-editor-icon "emacs_icon")
 '(elcord-mode t)
 '(elcord-use-major-mode-as-main-icon t)
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-export-backends '(ascii beamer html icalendar latex man md odt))
 '(org-latex-default-packages-alist
   '(("" "amsmath" t ("lualatex" "xetex")) ("" "fontspec" t ("lualatex" "xetex"))
     ("AUTO" "inputenc" t ("pdflatex")) ("T1" "fontenc" t ("pdflatex"))
     ("" "graphicx" t nil) ("" "longtable" nil nil) ("" "wrapfig" nil nil)
     ("" "rotating" nil nil) ("normalem" "ulem" t nil)
     ("" "amsmath" t ("pdflatex")) ("" "amssymb" t ("pdflatex"))
     ("" "capt-of" nil nil) ("" "hyperref" nil nil) ("" "amssymb" nil nil)
     ("" "listings" nil nil) ("" "graphicx" nil nil) ("" "fancyhdr" nil nil)
     ("" "color" nil nil)))
 '(org-latex-preview-preamble
   "\\documentclass{article}\12\\usepackage[usenames]{color}\12[DEFAULT-PACKAGES]\12\\usepackage{fancyhdr,color}\12\\usepackage{outlines}\12\\usepackage{graphicx}\12\\usepackage{fancyhdr,color}\12\\usepackage{amsmath}\12\\usepackage{listings}\12\\usepackage{amssymb}\12[PACKAGES] \12\12\\pagestyle{empty}             % do not remove\12% The settings below are copied from fullpage.sty\12\\setlength{\\textwidth}{\\paperwidth}\12\\addtolength{\\textwidth}{-3cm}\12\\setlength{\\oddsidemargin}{1.5cm}\12\\addtolength{\\oddsidemargin}{-2.54cm}\12\\setlength{\\evensidemargin}{\\oddsidemargin}\12\\setlength{\\textheight}{\\paperheight}\12\\addtolength{\\textheight}{-\\headheight}\12\\addtolength{\\textheight}{-\\headsep}\12\\addtolength{\\textheight}{-\\footskip}\12\\addtolength{\\textheight}{-3cm}\12\\setlength{\\topmargin}{1.5cm}\12\\addtolength{\\topmargin}{-2.54cm}")
 '(org-pretty-entities t)
 '(org-supertag-sync-directories '("/home/liam/org/roam/"))
 '(org-toc-follow-mode t)
 '(org-toc-info-mode t)
 '(package-selected-packages '(wakatime-mode activity-watch-mode request)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block-begin-line ((t (:background "#282828"))))
 '(org-block-end-line ((t (:background "#282828"))))
 '(org-document-title ((t (:height 2.0))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.6))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.4))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.3))))
 '(org-level-6 ((t (:inherit outline-5 :height 1.2))))
 '(org-level-7 ((t (:inherit outline-5 :height 1.1))))
 '(org-modern-block-border ((t (:inherit org-block-end-line))))
 '(org-modern-block-name ((t (:inherit org-block-begin-line))))
 '(org-modern-statistics ((t (:inherit org-checkbox-statistics-todo))))
 '(org-quote ((t (:foreground "#83a598" :extend t)))))
(put 'customize-variable 'disabled nil)
(put 'customize-group 'disabled nil)
