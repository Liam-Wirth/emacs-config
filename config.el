(add-hook 'doom-first-frame-hook
          (lambda ()
            (unless (daemonp) ; Check if Emacs is already a daemon (server)
              (server-start)))) ; Start the Emacs server if not already running
(custom-set-variables
 '(display-battery-mode 1))

;; -*- lexical-binding: t -*-
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
 (setq user-full-name "Liam Wirth"
       user-mail-address "ltwirth@asu.edu")


;; (setq org-directory "~/org/")
(setq org-directory (file-truename "~/org/")) ;; File truename allows for symbolic link resolution
(setq org-roam-directory (file-truename "~/org/roam/")) ; I like roaming
;; I've been on-and off trying to use the org agenda, and i like the ideas of org-roam-daily as a way to quickly make/maintain daily notes.
;; I thought to myself "why not try to combine the two?" (this isn't working, but daily notes are working for me)
(setq org-agenda-files '("~/org/roam/daily" "~/.config/doom/" "~/org/roam/agenda" ))

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to lose work, I certainly don't, but vim mode makes me :w on every <esc> so it's not too bad
      truncate-string-ellipsis "…"
      password-cache-expiry nil
      scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2                             ; It's nice to maintain a little margin
      display-time-default-load-average nil       ; I don't think I've ever found this useful
      display-line-numbers-type 'relative         ; RelNum ON TOP
      evil-vsplit-window-right t
      evil-split-window-below t
      )

(display-time-mode 1)                             ; Enable time in the mode-line
(global-subword-mode 1)                           ; Iterate through CamelCase words
(blink-cursor-mode -1)
(column-number-mode t)
(transient-mark-mode t)
(pixel-scroll-precision-mode t)                   ; Turn on pixel scrolling

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face

(set-face-attribute 'default nil
                    :font "JetBrains Mono NerdFont"
                    :height 120
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :font "Overpass"
                    :height 130
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono"
                    :height 130
                    :weight 'medium);; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(setq doom-symbol-font (font-spec :family "JuliaMono" :size 22 :weight 'light))
(setq doom-serif-font (font-spec :family "IBM Plex Mono" :size 22 :weight 'light))
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-18"))

(setq-default line-spacing 0.05)

(setq doom-theme 'doom-gruvbox
      doom-themes-treemacs-enable-variable-pitch nil)

(after! doom-modeline
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-icon t)
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-height 45)
  (setq doom-modeline-lsp-icon t)
  (setq doom-modeline-total-line-number t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-modal-modern-icon t)
  (setq doom-modeline-battery t)
  (setq doom-modeline-time t)
  (setq doom-modeline-env-version t)
  (setq doom-modeline-time-clock-size 0.65)
  ;;(setq      doom-modeline-hud nil)
  (setq      doom-themes-padded-modeline t)
  (add-hook! 'doom-modeline-mode-hook
    (progn
      (set-face-attribute 'header-line nil
                          :background (face-background 'mode-line)
                          :foreground (face-foreground 'mode-line))
      ))
  )

(use-package! info-colors
:commands (info-colors-fontify-node))

(use-package wakatime-mode
  :ensure t)

(global-wakatime-mode t)

(after! which-key
  (setq which-key-idle-delay 0.2))

(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))
(setq which-key-allow-multiple-replacements t)

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))):w

;; Custom todo states
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)" "SOMEDAY(s)")))

;; Custom faces for the todo states
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("NEXT" . "orange")
        ("WAITING" . "yellow")
        ("CANCELLED" . (:foreground "blue" :weight bold :strike-through t))
        ("SOMEDAY" . (:foreground "magenta" :weight bold))))

;; Setup org-agenda for that jawn
 (setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((agenda "")
          (todo "TODO")
          (todo "NEXT")
          (todo "WAITING")
          (todo "SOMEDAY")))))

;; Setup Org agenda to by default exclude cancelled stuff
(setq org-agenda-todo-ignore-states '("SOMEDAY" "CANCELLED"))

(setq org-agenda-custom-commands
      '(("S" "Special states"
         ((todo "SOMEDAY|CANCELLED"
                ((org-agenda-overriding-header "Someday/Maybe and Cancelled items:"))))
        ("s" "Someday items"
         ((todo "SOMEDAY"
                ((org-agenda-overriding-header "Someday/Maybe items:"))))
        ("c" "Cancelled items"
         ((todo "CANCELLED"
                ((org-agenda-overriding-header "Cancelled items:"))))
        ("a" "Active TODOs (exclude SOMEDAY and CANCELLED)"
         ((todo ""
                ((org-agenda-todo-ignore-states '("SOMEDAY" "CANCELLED"))
                 (org-agenda-overriding-header "Active TODOs (excluding SOMEDAY and CANCELLED):"))))))))))

(after! org
  (use-package! org-modern
 :config
(setq org-special-ctrl-a/e t)
(setq org-insert-heading-respect-content t)
  ;; ------------------------------------- appearance ----------------------------------------------
  (setq org-modern-radio-target    '("❰" t "❱"))
  (setq org-modern-internal-target '("↪ " t ""))
  (setq org-modern-todo t)
  (setq org-modern-todo-faces
  '(("TODO" :inverse-video t :inherit org-todo)
   ("PROJ" :inverse-video t :inherit +org-todo-project)
   ("STRT" :inverse-video t :inherit +org-todo-active)
   ("[-]"  :inverse-video t :inherit +org-todo-active)
   ("HOLD" :inverse-video t :inherit +org-todo-onhold)
   ("WAIT" :inverse-video t :inherit +org-todo-onhold)
   ("[?]"  :inverse-video t :inherit +org-todo-onhold)
   ("KILL" :inverse-video t :inherit +org-todo-cancel)
   ("NO"   :inverse-video t :inherit +org-todo-cancel)))
  (setq org-modern-footnote (cons nil (cadr org-script-display)))
   (setq org-modern-block-name
   '((t . t)
     ("src" "»" "«")
     ("example" "»–" "–«")
     ("quote" "❝" "❞")
     ("export" "⏩" "⏪")))
   (setq org-modern-priority nil)
   (setq org-modern-progress nil)
   ; org-modern-horizontal-rule (make-string 36 ?─)
   (setq org-modern-horizontal-rule "──────────")
  ; org-modern-hide-stars "·"
   (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶"))
   (setq org-modern-keyword
        '((t . t)
          ("title" . "𝙏")
          ("subtitle" . "𝙩")
          ("author" . "𝘼")
          ("date" . "𝘿")
          ("property" . "☸")
          ("options" . "⌥")
          ("startup" . "⏻")
          ("macro" . "𝓜")
          ("include" . "⇤")
          ("setupfile" . "⇚")
          ("html_head" . "🅷")
          ("html" . "🅗")
          ("latex_class" . "🄻")
          ("latex_header" . "🅻")
          ("latex_header_extra" . "🅻⁺")
          ("latex" . "🅛")
          ("beamer_theme" . "🄱")
          ("beamer_header" . "🅱")
          ("beamer" . "🅑")
          ("attr_latex" . "🄛")
          ("attr_html" . "🄗")
          ("attr_org" . "⒪")
          ("name" . "⁍")
          ("header" . "›")
          ("caption" . "☰")
          ("results" . "🠶")))
  (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo)))
)

(after! org (add-hook 'org-mode-hook #'org-modern-mode))

(custom-set-faces
 '(org-modern-block-name ((t (:inherit org-block-begin-line))))
 '(org-modern-block-border ((t (:inherit org-block-end-line)))))

(after! org-mode
  (custom-set-faces!
    '((org-document-title)
      :foreground ,(face-attribute 'org-document-title :foreground)
      :height 2.0
      :weight bold
      )
    '((org-level-1)
      :height 1.7
      :weight medium
      :foreground ,(face-attribute 'outline-1 :foreground)
      )
    '((org-level-2)
      :height 1.6
      :weight medium
      :foreground ,(face-attribute 'outline-2 :foreground)
      )
    '((org-level-3)
      :height 1.5
      :weight medium
      :foreground ,(face-attribute 'outline-3 :foreground)
      )
    '((org-level-4)
      :height 1.4
      :weight medium
      :foreground ,(face-attribute 'outline-4 :foreground)
      )
    '((org-level-5)
      :height 1.3
      :weight medium
      :foreground ,(face-attribute 'outline-5 :foreground)
      )
    '((org-level-6)
      :height 1.2
      :weight medium
      :foreground ,(face-attribute 'outline-6 :foreground)
      )
    '((org-level-7)
      :height 1.1
      :weight medium
      :foreground ,(face-attribute 'outline-7 :foreground)
      )
    ))

(after! org
(setq org-ellipsis "▾")
(setq org-hide-leading-stars t)
(setq org-priority-highest ?A)
(setq org-priority-lowest ?E)
(setq org-priority-faces
      '((?A . 'nerd-icons-red)
        (?B . 'nerd-icons-orange)
        (?C . 'nerd-icons-yellow)
        (?D . 'nerd-icons-green)
        (?E . 'nerd-icons-blue))))

(appendq! +ligatures-extra-symbols
          (list :list_property "∷"
                :em_dash       "—"
                :ellipses      "…"
                :arrow_right   "→"
                :arrow_left    "←"
                :arrow_lr      "↔"
                :properties    "⚙"
                :end           "∎"
                :priority_a    #("⚑" 0 1 (face nerd-icons-red))
                :priority_b    #("⬆" 0 1 (face nerd-icons-orange))
                :priority_c    #("■" 0 1 (face nerd-icons-yellow))
                :priority_d    #("⬇" 0 1 (face nerd-icons-green))
                :priority_e    #("❓" 0 1 (face nerd-icons-blue))))

(map! :after org
      :map org-mode-map
      :localleader
      :desc "Org-Mark-Ring jump" "gj" #'org-mark-ring-goto
      )
(map! :after org
      :map org-mode-map
      :localleader
      :desc "Org-Mark-Ring Save" "gs" #'org-mark-ring-push)

(defun open-temp-buffer-src ()
"Open Temporary Buffer When Editing Src Blocks"
(interactive)
(org-edit-src-code)
)

(map! :after org
      :map org-mode-map
      :localleader
      :desc "Org Set Property" "O" #'org-set-property)
(map! :after org
      :map org-mode-map
      :localleader
      :n "o" #'org-edit-src-code)

(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "code")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "yes")
        (:tangle . "yes")
        (:comments . "link")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   '(emacs-lisp . t)
   '(mips . t)
   '(python . t)
   '(latex . t)
   '(rust . t)
   '(C . t)
   '(cpp . t)))

(require 'org)
(require 'ob)
(require 'ob-C)

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(defadvice! +org-edit-latex-env-after-insert-a (&rest _)
  :after #'org-cdlatex-environment-indent
  (org-edit-latex-environment))

;; Calibrated based on the TeX font and org-buffer font.
(plist-put org-format-latex-options :zoom 1.93)
(after! org (plist-put org-format-latex-options :scale 3.0))

(after! org
 (setq org-export-backends '(ascii beamer html icalendar latex man md odt))
 )

(use-package! ox-latex
  :config

  ;; Default packages
(setq org-export-headline-levels 8
        org-latex-default-packages-alist
        '(("AUTO" "inputenc" t ("pdflatex" "lualatex"))
          ("T1" "fontenc" t ("pdflatex"))
          ;; Microtype
          ;; - pdflatex: full microtype features, fast, however no fontspec
          ;; - lualatex: good microtype feature support, however slow to compile
          ;; - xelatex: only protrusion support, fast compilation
          ("activate={true,nocompatibility},final,tracking=true,kerning=true,spacing=true,factor=1100,stretch=10,shrink=10"
           "microtype" nil ("pdflatex")         )
          ("activate={true,nocompatibility},final,tracking=true,factor=1100,stretch=10,shrink=10"
           "microtype" nil ("lualatex"))
          ("protrusion={true,nocompatibility},final,factor=1100,stretch=10,shrink=10"
           "microtype" nil ("xelatex"))
          ("dvipsnames,svgnames" "xcolor" nil)  ; Include xcolor package
          ("headings=optiontoheadandtoc,footings=optiontofootandtoc,headlines=optiontoheadandtoc"
           "scrextend" nil)  ; Include scrextend package
          ("colorlinks=true,  citecolor=BrickRed, urlcolor=DarkGreen" "hyperref" nil))))

(after! ox
 ;; Additional LaTeX classes
  (after! ox
    (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass{article}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("koma-letter" "\\documentclass[11pt]{scrletter}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("koma-article" "\\documentclass[11pt]{scrartcl}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("koma-report" "\\documentclass[11pt]{scrreprt}"
                   ("\\part{%s}" . "\\part*{%s}")
                   ("\\chapter{%s}" . "\\chapter*{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
    (add-to-list 'org-latex-classes
                 '("koma-book" "\\documentclass[11pt]{scrbook}"
                   ("\\part{%s}" . "\\part*{%s}")
                   ("\\chapter{%s}" . "\\chapter*{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))


  ;; Table of contents customization
(after! org
  ;; Customize table of contents style
  (setq org-latex-custom-id '("\\usepackage{tocloft}"
                              "\\setlength{\\cftbeforesecskip}{1ex}"
                              "\\setlength{\\cftbeforesubsecskip}{0.5ex}"
                              "\\setlength{\\cftbeforesubsubsecskip}{0.5ex}")))

(after! org
  ;; Define common style for table of contents
  (setq common-toc-style '("\\usepackage{tocloft}"
                           "\\setlength{\\cftbeforesecskip}{1ex}"
                           "\\setlength{\\cftbeforesubsecskip}{0.5ex}"
                           "\\setlength{\\cftbeforesubsubsecskip}{0.5ex}"
                           ("\\tableofcontents" . "\\tableofcontents\\thispagestyle{empty}\\vspace*{\\fill}\\clearpage")))
  ;; Apply the common style to all classes
  (dolist (class org-latex-classes)
    (let ((class-name (car class))
          (class-content (cdr class)))
      ;; Append common style to each class content
      (setcdr class (append class-content common-toc-style)))))

(after! org
  ;; Customize specific class style for table of contents
  (setq org-latex-toc-command "\\tableofcontents\\newpage"))

(after! org
  (add-to-list 'org-latex-classes
        '(("report"
           "\\documentclass{report}"
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))))

(after! ox-latex
  (setq org-latex-src-block-backend 'engraved))

(use-package! ox-chameleon
  :after ox
  :config
  (setq! ox-chameleon-engrave-theme 'doom-gruvbox))

(setq org-latex-pdf-process '("LANGUAGE=en_US.UTF-8 LC_ALL=en_US.UTF-8 latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))

(defun +org-export-latex-fancy-item-checkboxes (text backend info)
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string
     "\\\\item\\[{$\\\\\\(\\w+\\)$}\\]"
     (lambda (fullmatch)
       (concat "\\\\item[" (pcase (substring fullmatch 9 -3) ; content of capture group
                             ("square"   "\\\\checkboxUnchecked")
                             ("boxminus" "\\\\checkboxTransitive")
                             ("boxtimes" "\\\\checkboxChecked")
                             (_ (substring fullmatch 9 -3))) "]"))
     text)))

(add-to-list 'org-export-filter-item-functions
             '+org-export-latex-fancy-item-checkboxes)

(use-package! pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

(after! org
  ;; (setq org-roam-directory  "~/org/roam/")
  (setq org-modern-mode t)
  (setq org-roam-directory (file-truename "~/org/roam/"))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-file-extensions '("org"))
)

(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            #'org-roam-unlinked-references-section

        ))

(defun my/org-roam-toggle-unlinked-references ()
        "Enable unlinked references in org-roam."
        ;; need to update the org-roam-mode-sections list, if it contains "org-roam-unlinked-references-section" remove it, if not, add it
        (if (member 'org-roam-unlinked-references-section org-roam-mode-sections)
            (setq org-roam-mode-sections (remove 'org-roam-unlinked-references-section org-roam-mode-sections))
          (add-to-list 'org-roam-mode-sections 'org-roam-unlinked-references-section))
)

(defun org-roam-repair-broken-links ()
  "For all broken links referencing current note,
   repair incoming links"
  (interactive)

  ;; we first determine if there exists any such broken references
  (when-let* ((title (org-get-title))
	      (query "select links.dest,
                             links.source, links.pos

                      from links where links.dest like $s1")
	      (links (org-roam-db-query query (concat "%" title "%"))))

    ;; for all such - go to those buffers and let org-roam's
    ;; [[roam:*]] replace protocol take over - it would do this on save
    ;; automatically
  (save-excursion
    (mapc (lambda (link)
	    (let ((id (nth 1 link)))
	      (+org-roam-id-goto id)
	      (set-buffer-modified-p t)
	      (save-buffer)))
	  links))))

(defun +org-roam-id-goto (id)
  "Switch to the buffer containing the entry with id ID.
Move the cursor to that entry in that buffer.
Like `org-id-goto', but additionally uses the Org-roam database"
  (interactive "sID: ")
  (let ((m (org-roam-id-find id 'marker)))
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))
    (pop-to-buffer-same-window (marker-buffer m))
    (goto-char m)
    (move-marker m nil)
    (org-fold-show-context)))

(org-link-set-parameters "roam" :face '(:foreground "red"))
(org-link-set-parameters "file" :face '(:foreground "blue"))
(org-link-set-parameters "https" :face '(:foreground "green"))

(use-package! org-similarity
 :after org  ; Ensure it loads after org-mode
 :commands (org-similarity-insert-list
            org-similarity-sidebuffer
            org-similarity-query) ; Autoload commands
 :config
 (setq org-similarity-directory org-roam-directory) ; Or org-directory if not using org-roam
(setq org-similarity-file-extension-pattern "*.org\\|*.md") ;; Can look at markdown >:)
 (setq org-similarity-language "english") ; Or your preferred language
 (setq org-similarity-algorithm "tfidf") ; Or "bm25"
 (setq org-similarity-number-of-documents 10) ; Adjust as desired
 (setq org-similarity-min-chars 0) ; Adjust if needed
 (setq org-similarity-show-scores t) ; Set to t to see similarity scores initially
 (setq org-similarity-threshold 0.05) ; Adjust if needed
 (setq org-similarity-use-id-links t) ; Recommended for org-roam v2
 (setq org-similarity-recursive-search nil) ; Or t for recursive search
 (setq org-similarity-custom-python-interpreter nil) ; Let it manage venv
 (setq org-similarity-remove-first nil)
 (setq org-similarity-heading "** Related notes") ; Customize heading if you like
 (setq org-similarity-prefix "- ") ; Customize prefix if you like
 (setq org-similarity-ignore-frontmatter nil) ; Or t to ignore frontmatter
 )

(use-package! org-roam-timestamps
  :after org-roam
  :config (org-roam-timestamps-mode))
	(after! org-roam
	(setq org-roam-timestamps-parent-file t)
	(setq org-roam-timestamps-remember-timestamps t))

(defadvice! doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
  :around #'doom-modeline-buffer-file-name ; takes no args
  (if (s-contains-p org-roam-directory (or buffer-file-name ""))
      (replace-regexp-in-string
       "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
       "🢔(\\1-\\2-\\3) "
       (subst-char-in-string ?_ ?  buffer-file-name))
    (funcall orig-fun)))

(after! org-cliplink
(map! :leader
      :desc "Org Cliplink"
      "n l" #'org-cliplink)
)

(after! org-roam-dailies
(setq org-roam-dailies-capture-templates
      (let ((head
             (concat "#+title: %<%Y-%m-%d (%a)>\n"
                     "#+startup: showall\n"
                     "#+filetags: :dailies:\n* daily overview\n"
                     "#+export_file_name: ~/org/exported/dailies/"
                     "\n#+begin_src emacs-lisp :results value raw\n"
                     "(/get-daily-agenda \"%<%Y-%m-%d>\")\n"
                     "#+end_src\n"
                     "#+ Last Daily Entry: "
                     "\n* [/] do today\n* [/] maybe do today\n* journal\n* [/] Completed Tasks\n")))
        `(("j" "journal" entry
           "* %<%H:%M> %?"
           :if-new (file+head+olp "%<%Y-%m-%d>.org" ,head ("journal"))
           :empty-lines 1
           :jump-to-captured t)
          ("t" "do today" item
           "[ ] %i%?"
           :if-new (file+head+olp "%<%Y-%m-%d>.org" ,head ("do today"))
           :immediate-finish t
           :empty-lines 1
           :jump-to-captured t)
          ("m" "maybe do today" item
           "[ ] %a"
           :if-new (file+head+olp "%<%Y-%m-%d>.org" ,head ("maybe do today"))
           :immediate-finish t
           :empty-lines 1
           :jump-to-captured t)))))

;; Set up org-agenda-files to include Org Roam dailies directory
(setq org-agenda-files (append org-agenda-files (list "~/org/roam/daily")))

(defun my/org-roam-today-mk-agenda-link ()
  (interactive)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char pos)
        (org-roam-dailies-capture-today)))))

(defun my/get-daily-agenda (&optional date)
  "Return the agenda for the day as a string."
  (interactive)
  (let ((file (make-temp-file "daily-agenda" nil ".txt")))
    (org-agenda nil "d" nil)
    (when date (org-agenda-goto-date date))
    (org-agenda-write file nil nil "*Org Agenda(d)*")
    (kill-buffer)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (kill-line 2)
      (while (re-search-forward "^  " nil t)
        (replace-match "- " nil nil))
      (buffer-string))))

(setq org-agenda-custom-commands
      '(("d" "Org Roam Daily Files"
         ((agenda "" ((org-agenda-files (list "~/org/roam/daily"))))
          (function my/org-roam-today-mk-agenda-link)
          (function my/get-daily-agenda)))))

(setq org-agenda-files (append org-agenda-files
                               (list "~/org/roam/daily")))

(use-package! org-supertag
  :after org
  :config
  (require 'org-supertag)
  (map! :leader
        :prefix "o"
        "s t" #'org-supertag-list-tags
        "s r" #'org-supertag-show-tag-relations
        "s p" #'org-supertag-list-tag-properties
        "s c" #'org-supertag-compare-tags))
(setq org-supertag-directory "~/org/roam")

(after! org-roam
        (map! :leader
                (:prefix ("n r" . "org-roam")
                :desc "Org Extract Subtree" "x" #'org-roam-extract-subtree))
        (map! :leader
                (:prefix ("mm" . "org-roam")
                :desc "Org Extract Subtree" "x" #'org-roam-extract-subtree))
)

(defun my/roamutils/find-notes-with-file-links ()
  "Find Org-roam notes containing 'file:' links to '.org' files, excluding the 'daily' directory."
  (interactive)
  (let* ((org-roam-directory (expand-file-name "~/org/roam")) ; Adjust this path as needed
         (files (directory-files-recursively org-roam-directory "\\.org$"))
         (filtered-files (seq-filter
                          (lambda (file)
                            (message "Checking file: %s" file) ; Print each file being checked
                            (and (not (string-match-p "/daily/" file))
                                 (with-temp-buffer
                                   (insert-file-contents file)
                                   (goto-char (point-min))
                                   (re-search-forward "\\[\\[file:" nil t))))
                          files)))
    (if filtered-files
        (let ((selected-file (completing-read "Select a note: " filtered-files nil t)))
          (find-file selected-file))
      (message "No notes with 'file:' links to '.org' files found."))))

(defun my/roamutils/replace-roam-links-with-id-in-buffer ()
  "Replace all roam: links in the current buffer with id: links if the target exists.
Skips links with no matching node."
  (save-excursion
    (goto-char (point-min))
    (let ((modified nil))
      ;; Search for [[roam:Some Title]]
      (while (re-search-forward "\\[\\[roam:\\([^]]+\\)\\]\\(\\[.*?\\]\\)?\\]" nil t)
        (let* ((title (match-string 1))
               ;; Look up the node in org-roam by title or alias
               (node (org-roam-node-from-title-or-alias title)))
          (if node
              (let* ((id (org-roam-node-id node))
                     ;; If there was a description (optional link text), preserve it
                     (description (or (match-string 2)
                                      (concat "[" (org-roam-node-title node) "]"))))
                ;; Replace the whole match with the id link
                (replace-match (format "[[id:%s]]%s" id description))
                (setq modified t))
            (message "Skipping: No node found for title '%s'" title))))
      modified)))

(defun my/roamutils/batch-replace-roam-links-in-all-files ()
  "Process all org-roam files, replacing roam: links with id: links where possible.
Asks for confirmation before starting, since it may take time."
  (interactive)
  (when (yes-or-no-p "This will scan and modify all Org-roam files. Continue? ")
    (let ((files (org-roam-list-files))
          (total-files 0)
          (modified-files 0))
      (message "Starting batch replace of roam: links with id: links...")
      (dolist (file files)
        (setq total-files (1+ total-files))
        (with-current-buffer (find-file-noselect file)
          (message "Processing file: %s" file)
          (when (my/roamutils/replace-roam-links-with-id-in-buffer)
            (message "Modified and saved: %s" file)
            (setq modified-files (1+ modified-files))
            (save-buffer))))
      (message "Finished processing %d files. Modified %d files."
               total-files modified-files))))

(defun my/roamutils/org-roam-create-and-link-stub ()
  "Prompt for a title, create a stub note, and insert a link to it at point."
  (interactive)
  (let ((org-roam-capture-templates
         '(("s" "stub" plain "%?"
            :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :stub:\n\n:PROPERTIES:\n:STATUS: stub\n:END:\n\n")
            :immediate-finish t
            :unnarrowed t))))

    ;; Run the node insert command with our stub template
    (org-roam-node-insert)))

;; Bind it to a key
(global-set-key (kbd "C-c n l") #'my/roamutils/org-roam-create-and-link-stub)

;; Define the stub link face
(defface my/org-link-stub-face
  '((t (:inherit org-link :foreground "gray" :underline t)))
  "Face for links to stub notes.")

;; Function that determines the face for ID links
(defun my/org-link-id-face (link)
  "Return face for ID link based on whether it points to a stub."
  (let* ((id (org-element-property :path link))
         (node (org-roam-node-from-id id)))
    (if (and node (member "stub" (org-roam-node-tags node)))
        'my/org-link-stub-face
      'org-link)))

;; Apply our custom face function to id links
(org-link-set-parameters "id" :face 'my/org-link-id-face)

;; Force refresh the display to apply changes
(defun my/refresh-stub-link-display ()
  "Refresh the display of links to apply stub formatting."
  (when (derived-mode-p 'org-mode)
    (font-lock-flush)))

;; Run this when loading files or creating links
(add-hook 'org-mode-hook 'my/refresh-stub-link-display)
(advice-add 'org-roam-node-insert :after 'my/refresh-stub-link-display)

(defun my/roamutils/org-roam-toggle-stub-status ()
  "Toggle whether the current org-roam node is a stub."
  (interactive)
  (unless (org-roam-buffer-p)
    (user-error "Not in an Org-roam file"))

  (let* ((node (org-roam-node-at-point))
         (tags (org-roam-node-tags node))
         (is-stub (member "stub" tags)))

    ;; Toggle filetags
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+filetags:\\(.*\\)$" nil t)
          (let* ((file-tags (match-string 1))
                 (updated-tags (if is-stub
                                   ;; Remove stub tag
                                   (replace-regexp-in-string ":stub:" "" file-tags)
                                 ;; Add stub tag
                                 (if (string-match-p ":" file-tags)
                                     (concat file-tags ":stub:")
                                   (concat file-tags " :stub:")))))
            (replace-match (concat "#+filetags:" updated-tags) t t))
        ;; No filetags found, add them
        (unless is-stub
          (goto-char (point-min))
          (forward-line)
          (insert "#+filetags: :stub:\n"))))

    ;; Toggle property status
    (save-excursion
      (goto-char (point-min))
      (let ((found-prop nil))
        ;; First check if properties drawer exists
        (when (re-search-forward ":PROPERTIES:" nil t)
          (let ((drawer-end (save-excursion
                             (re-search-forward ":END:" nil t))))
            (when drawer-end
              ;; Check if STATUS property exists
              (when (re-search-forward "^:STATUS:.*$" drawer-end t)
                (setq found-prop t)
                (replace-match (concat ":STATUS: " (if is-stub "" "stub")) t t))))
          ;; If STATUS not found but drawer exists, add it
          (unless found-prop
            (unless is-stub  ; Only add if making it a stub
              (end-of-line)
              (insert "\n:STATUS: stub"))))

        ;; If no properties drawer found and making it a stub, add it
        (unless (or found-prop is-stub)
          (goto-char (point-min))
          (forward-line 2) ; After title and filetags
          (insert ":PROPERTIES:\n:STATUS: stub\n:END:\n\n"))))

    ;; Force buffer save and database update
    (save-buffer)
    (org-roam-db-sync)

    ;; Give feedback to user
    (message "Node is now %s a stub" (if is-stub "no longer" "marked as"))))

;; Bind to leader key (works with evil, doom, spacemacs)
(with-eval-after-load 'evil-leader
  (evil-leader/set-key "mms" 'my/roamutils/org-roam-toggle-stub-status))

(global-set-key (kbd "C-c n s") 'my/roamutils/org-roam-toggle-stub-status)

(use-package! flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))

(set-file-template! "\\.pro" :trigger "__" :mode 'prolog-mode)

(add-to-list 'auto-mode-alist '("\\.pro\\'" . prolog-mode))

(after! lsp-mode
    (lsp-register-client
     (make-lsp-client
      :new-connection
      (lsp-stdio-connection (list "swipl"
                                  "-g" "use_module(library(lsp_server))."
                                  "-g" "lsp_server:main"
                                  "-t" "halt"
                                  "--" "stdio"))
      :major-modes '(prolog-mode)
      :priority 1
      :multi-root t
      :server-id 'prolog-ls))
    )
(when (not (executable-find "swipl"))
  (warn! "Swipl not found in the system, prolog might not work as expected"))

(add-hook 'find-file-hook #'my-prolog-mode-setup)

(defun my-prolog-mode-setup ()
  "Custom setup for .pro files."
  (when (and (stringp buffer-file-name)
             (string= (file-name-extension buffer-file-name) "pro"))
    (prolog-mode)
    (lsp)))

(use-package! lexic
  :commands lexic-search lexic-list-dictionary
  :config
  (map! :map lexic-mode-map
        :n "q" #'lexic-return-from-lexic
        :nv "RET" #'lexic-search-word-at-point
        :n "a" #'outline-show-all
        :n "h" (cmd! (outline-hide-sublevels 3))
        :n "o" #'lexic-toggle-entry
        :n "n" #'lexic-next-entry
        :n "N" (cmd! (lexic-next-entry t))
        :n "p" #'lexic-previous-entry
        :n "P" (cmd! (lexic-previous-entry t))
        :n "E" (cmd! (lexic-return-from-lexic) ; expand
                     (switch-to-buffer (lexic-get-buffer)))
        :n "M" (cmd! (lexic-return-from-lexic) ; minimise
                     (lexic-goto-lexic))
        :n "C-p" #'lexic-search-history-backwards
        :n "C-n" #'lexic-search-history-forwards
        :n "/" (cmd! (call-interactively #'lexic-search))))

(defadvice! +lookup/dictionary-definition-lexic (identifier &optional arg)
  "Look up the definition of the word at point (or selection) using `lexic-search'."
  :override #'+lookup/dictionary-definition
  (interactive
   (list (or (doom-thing-at-point-or-region 'word)
             (read-string "Look up in dictionary: "))
         current-prefix-arg))
  (lexic-search identifier nil nil t))

(setq-default abbrev-mode t)

(defvar abbrev-fn (expand-file-name "misc/abbrev.el" doom-user-dir))
(setq abbrev-file-name abbrev-fn)

(use-package! jinx
        :defer t
        :init
        (add-hook 'doom-init-ui-hook #'global-jinx-mode)
        :config
        ;; Use my custom dictionary
        (setq jinx-languages "en-custom")
        ;; Extra face(s) to ignore
        (push 'org-inline-src-block
        (alist-get 'org-mode jinx-exclude-faces))
        ;; Take over the relevant bindings.
        (after! ispell
        (global-set-key [remap ispell-word] #'jinx-correct))
        (after! evil-commands
        (global-set-key [remap evil-next-flyspell-error] #'jinx-next)
        (global-set-key [remap evil-prev-flyspell-error] #'jinx-previous))
        ;; I prefer for `point' to end up at the start of the word,
        ;; not just after the end.
        (advice-add 'jinx-next :after (lambda (_) (left-word))))

(after! cdlatex
  (setq cdlatex-env-alist
        '(("bmatrix" "\\begin{bmatrix}\n?\n\\end{bmatrix}" nil)
          ("equation*" "\\begin{equation*}\n?\n\\end{equation*}" nil)))
  (setq ;; cdlatex-math-symbol-prefix ?\; ;; doesn't work at the moment :(
   cdlatex-math-symbol-alist
   '( ;; adding missing functions to 3rd level symbols
     (?_    ("\\downarrow"  ""           "\\inf"))
     (?2    ("^2"           "\\sqrt{?}"     ""     ))
     (?3    ("^3"           "\\sqrt[3]{?}"  ""     ))
     (?^    ("\\uparrow"    ""           "\\sup"))
     (?k    ("\\kappa"      ""           "\\ker"))
     (?m    ("\\mu"         ""           "\\lim"))
     (?c    (""             "\\circ"     "\\cos"))
     (?d    ("\\delta"      "\\partial"  "\\dim"))
     (?D    ("\\Delta"      "\\nabla"    "\\deg"))
     ;; no idea why \Phi isnt on 'F' in first place, \phi is on 'f'.
     (?F    ("\\Phi"))
     ;; now just convenience
     (?.    ("\\cdot" "\\dots"))
     (?:    ("\\vdots" "\\ddots"))
     (?*    ("\\times" "\\star" "\\ast")))
   cdlatex-math-modify-alist
   '( ;; my own stuff
     (?B    "\\mathbb"        nil          t    nil  nil)
     (?a    "\\abs"           nil          t    nil  nil))))

(setq yas-triggers-in-field t)

(after! yasnippet
  (setq yas-triggers-in-field t)  ; Allow nested triggers

  ;; Make sure YASnippet is properly initialized
  (yas-global-mode 1)

  ;; Add org-mode and markdown-mode to the snippet directories
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (add-hook 'markdown-mode-hook #'yas-minor-mode)


;; Add keybindings for YASnippet expansion
(map! :i "TAB" #'yas-expand-from-trigger-key
      :after yas
      :map yas-minor-mode-map
      :i [tab] nil
      :i "TAB" nil
      :i [backtab] #'yas-expand))

(use-package! aas
  :commands aas-mode)
