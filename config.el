;; (setq debug-on-error t)
;; (setq debug-on-interrupt t)
;; (setq debug-init t)

(add-hook 'doom-first-frame-hook
          (lambda ()
            (unless (daemonp) ; Check if Emacs is already a daemon (server)
              (server-start)))) ; Start the Emacs server if not already running

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


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")
(setq org-directory (file-truename "~/org/")) ;; File truename allows for symbolic link resolution
(setq org-roam-directory (file-truename "~/org/roam/"))
(md-roam-mode 1)
(setq md-roam-file-extension "md")
;; I've been on-and off trying to use the org agenda, and i like the ideas of org-roam-daily as a way to quickly make/maintain daily notes.
;; I thought to myself "why not try to combine the two?"
(setq org-agenda-files '("~/org/roam/daily/" "~/org/"))

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2                             ; It's nice to maintain a little margin
      display-time-default-load-average nil       ; I don't think I've ever found this useful
      display-line-numbers-type 'relative         ; RelNum ON TOP
      )


(display-time-mode 1)                             ; Enable time in the mode-line
(global-subword-mode 1)                           ; Iterate through CamelCase words
(pixel-scroll-precision-mode t)                   ; Turn on pixel scrolling



(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t                               ; Stretch cursor to the glyph width
 show-paren-mode 1                                ; Highlight Matching Parenthesis
 abbrev-mode t                                    ; erm..
)

(add-to-list 'default-frame-alist '(width . 92))
(add-to-list 'default-frame-alist '(height . 40))

(setq evil-vsplit-window-right t
      evil-split-window-below t
      ;; evil-split-window-left f
      ;; evil-split-window-above f
        )

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

(setq evil-vsplit-window-right t
      evil-split-window-below t)
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
;;(set-face-attribute 'font-lock-keyword-face nil
;; :slant 'italic)
(set-face-attribute 'doom-serif-font (font-spec :family "IBM Plex Mono" :size 22 :weight 'light))
(set-face-attribute 'doom-symbol-font (font-spec :family "JuliaMono"))
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-18"))

(setq-default line-spacing 0.05)

(setq doom-theme 'doom-gruvbox
      doom-themes-treemacs-enable-variable-pitch nil)

(blink-cursor-mode -1)
(column-number-mode t)
(transient-mark-mode t)

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

(map! :leader
      (:prefix ("e" . "explorer")
       :desc "Toggle Treemacs" "t" #'treemacs))

(use-package! md-roam
  :load-path "packages/md-roam" ; Assuming you install md-roam in ~/.doom.d/packages/md-roam/ (adjust if needed)
  :config
  (md-roam-mode 1)
  (setq md-roam-file-extension "md")) ; Optional, defaults to "md"

(after! org-roam
  (add-to-list 'org-roam-capture-templates
               '("m" "Markdown" plain ""
                 :target (file+head "%<%Y-%m-%dT%H%M%S>.md"
                                   "---\ntitle: ${title}\nid: %<%Y-%m-%dT%H%M%S>\ncategory: \n---\n")
                 :unnarrowed t)))

(defun my/generate-roam-id ()
  (format-time-string "%Y%m%d%H%M%S"))

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
  ;; appearance
  (setq org-modern-radio-target    '("❰" t "❱"))
  (setq org-modern-internal-target '("↪ " t "")) ; TODO: make this not be an emoji, and instead a font lig
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
  (setq org-roam-directory  "~/org/roam/")
  (setq org-modern-mode t)
  (setq org-roam-directory (file-truename "~/org/roam/"))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-file-extensions '("org" "md"))
  (require 'md-roam)
  (md-roam-mode 1)
  (org-roam-db-autosync-mode 1)
)

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

(after! org
(add-to-list 'org-roam-capture-templates
           '(("w" "Web Capture" plain
              "* [[%:link][%:title]] :web: :[[file:tags.org::*KEYWORDS][KEYWORDS]]\n  Captured on: %U\n  Source: %:link\n  \n  %input"
              :unnarrowed t :jump-to-captured t))))

(setq org-roam-capture-ref-templates
  '(("r" "ref" plain "* %U\n
%(zp/org-protocol-insert-selection-dwim \"%i\")%?"
     :target (file+head "web/${slug}.org"
                        "#+title: ${title}\n
#+roam_key: ${ref}\n
#+created: %u\n"  ;; <-- COMMA HERE
                        )
     :unnarrowed t)))

(defadvice! doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
  :around #'doom-modeline-buffer-file-name ; takes no args
  (if (s-contains-p org-roam-directory (or buffer-file-name ""))
      (replace-regexp-in-string
       "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
       "🢔(\\1-\\2-\\3) "
       (subst-char-in-string ?_ ?  buffer-file-name))
    (funcall orig-fun)))

(defvar +org-plot-term-size '(1050 . 650)
  "The size of the GNUPlot terminal, in the form (WIDTH . HEIGHT).")

(after! org-plot
  (defun +org-plot-generate-theme (_type)
    "Use the current Doom theme colours to generate a GnuPlot preamble."
    (format "
fgt = \"textcolor rgb '%s'\" # foreground text
fgat = \"textcolor rgb '%s'\" # foreground alt text
fgl = \"linecolor rgb '%s'\" # foreground line
fgal = \"linecolor rgb '%s'\" # foreground alt line

# foreground colors
set border lc rgb '%s'
# change text colors of  tics
set xtics @fgt
set ytics @fgt
# change text colors of labels
set title @fgt
set xlabel @fgt
set ylabel @fgt
# change a text color of key
set key @fgt

# line styles
set linetype 1 lw 2 lc rgb '%s' # red
set linetype 2 lw 2 lc rgb '%s' # blue
set linetype 3 lw 2 lc rgb '%s' # green
set linetype 4 lw 2 lc rgb '%s' # magenta
set linetype 5 lw 2 lc rgb '%s' # orange
set linetype 6 lw 2 lc rgb '%s' # yellow
set linetype 7 lw 2 lc rgb '%s' # teal
set linetype 8 lw 2 lc rgb '%s' # violet

# border styles
set tics out nomirror
set border 3

# palette
set palette maxcolors 8
set palette defined ( 0 '%s',\
1 '%s',\
2 '%s',\
3 '%s',\
4 '%s',\
5 '%s',\
6 '%s',\
7 '%s' )
"
            (doom-color 'fg)
            (doom-color 'fg-alt)
            (doom-color 'fg)
            (doom-color 'fg-alt)
            (doom-color 'fg)
            ;; colours
            (doom-color 'red)
            (doom-color 'blue)
            (doom-color 'green)
            (doom-color 'magenta)
            (doom-color 'orange)
            (doom-color 'yellow)
            (doom-color 'teal)
            (doom-color 'violet)
            ;; duplicated
            (doom-color 'red)
            (doom-color 'blue)
            (doom-color 'green)
            (doom-color 'magenta)
            (doom-color 'orange)
            (doom-color 'yellow)
            (doom-color 'teal)
            (doom-color 'violet)))

  (defun +org-plot-gnuplot-term-properties (_type)
    (format "background rgb '%s' size %s,%s"
            (doom-color 'bg) (car +org-plot-term-size) (cdr +org-plot-term-size)))

  (setq org-plot/gnuplot-script-preamble #'+org-plot-generate-theme)
  (setq org-plot/gnuplot-term-extra #'+org-plot-gnuplot-term-properties))

(use-package! org-media-note
  :hook (org-mode .  org-media-note-mode)
  :bind (
         ("H-v" . org-media-note-show-interface))  ;; Main entrance
  :config
  (setq org-media-note-screenshot-image-dir "~/Notes/imgs/")  ;; Folder to save screenshot
  )

(after! org-cliplink
(map! :leader
      :desc "Org Cliplink"
      "n l" #'org-cliplink)
)

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

; preface, I stole this straight from the internet, so I dunno even if this will work, and only have a loose Idea as to how it should work
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

;; Customize the default Org agenda command to include Org Roam daily files
(setq org-agenda-custom-commands
      '(("d" "Org Roam Daily Files"
         ((agenda "" ((org-agenda-files (list "~/org/roam/daily"))))
          (function my/org-roam-today-mk-agenda-link)
          (function my/get-daily-agenda)))))

;; Add daily files to the default agenda files list
(setq org-agenda-files (append org-agenda-files
                               (list "~/org/roam/daily")))

(use-package! pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

(use-package! org-noter
  :defer t
  :commands (org-noter)
  ;; Make sure it loads after org and pdf-view are available:
  :after (org pdf-view)
  :config
  ;; Where to store the notes (Org) files if :NOTER_DOCUMENT: is missing:
  (setq org-noter-notes-search-path '("~/org/roam/" "~/org/notes/"))

  ;; By default, new notes buffer spawns below the PDF; change if you want:
  (setq org-noter-notes-window-location 'right
        org-noter-doc-split-fraction '(0.5 . 0.5)
        org-noter-auto-save-last-location t)

  ;; You can also unify it with org-roam, if desired:
  (org-noter-enable-org-roam-integration)

  (map! :map org-noter-doc-mode-map
		:desc "Insert Note"
		:n (kbd "C-i") #'org-noter-insert-note)
	(map! :map org-noter-doc-mode-map
		:n "M-i" #'org-noter-insert-precise-note
		:desc "Insert Precise Note")
)

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

;; Build the agenda list the first time for the session
(my/org-roam-refresh-agenda-list)

(after! org
(defun my/org-roam-create-daily-file-if-needed ()
  "Create the daily file with the specified template if it doesn't exist."
  (let* ((date-string (format-time-string "%Y-%m-%d"))
         (file-name (concat date-string ".org"))
         (file-path (expand-file-name file-name "~/org/roam/daily"))
         (file-exists (file-exists-p file-path))
         (template
             (concat "#+title: %<%Y-%m-%d (%a)>\n"
                           "#+startup: showall\n"
                           "#+Filetags: :dailies:\n* daily overview\n"
                           "#+export_file_name: ~/org/exported/dalies/"
                           "\n#+begin_src emacs-lisp :results value raw\n"
                           "(my/get-daily-agenda \"" (format-time-string "%Y-%m-%d") "\")\n"
                           "#+end_src\n"
                           "#+ Last Daily Entry: "
                           "\n*  [/] do today\n* [/] maybe do today\n* journal\n* [/] Completed Tasks\n")))
        (unless file-exists
      (with-temp-buffer
        (insert template)
        (write-file file-path)))
    file-path)))

(after! org
  (defun my/org-roam-copy-heading-to-today ()
    "Copy the heading of a completed TODO to today's daily file with state-specific formatting."
    (interactive)
    (let* ((today-file (my/org-roam-create-daily-file-if-needed))
           (original-file (buffer-file-name))
           (heading (save-excursion
                     (org-back-to-heading t)
                     (org-get-heading t t t t)))
           (org-roam-id (org-roam-id-at-point))
           (link-to-original (if org-roam-id
                                (org-link-make-string (concat "id:" org-roam-id) heading)
                              (org-link-make-string (concat "file:" (expand-file-name original-file) "::" heading) heading)))
           (current-state (org-get-todo-state))
           (status-prefix
            (cond
             ((string= current-state "DONE") "✓ DONE")
             ((string= current-state "KILL") "✗ KILLED")
             ((string= current-state "WAIT") "⏳ WAITING")
             ((string= current-state "HOLD") "⏸️ HOLD")
             ((string= current-state "PROJ") "📌 PROJECT")
             ((string= current-state "LOOP") "🔄 LOOP")
             ((string= current-state "STRT") "▶️ STARTED")
             ((string= current-state "IDEA") "💡 IDEA")
             ((string= current-state "OKAY") "👍 OKAY")
             ((string= current-state "YES")  "✅ YES")
             ((string= current-state "NO")   "❌ NO")
             ((string= current-state "TODO") "📝 TODO")
             (t current-state)))
           (section-name
            (cond
             ((string= current-state "DONE") "* [/] Completed Tasks")
             ((string= current-state "KILL") "* [/] Killed Tasks")
             ((member current-state '("WAIT" "HOLD")) "* [/] Waiting/Held Tasks")
             ((member current-state '("YES" "OKAY")) "* [/] Approved Tasks")
             ((member current-state '("NO")) "* [/] Rejected Tasks")
             (t "* [/] State Changes")))
           (entry (format "** %s %s [%s]"
                         status-prefix
                         link-to-original
                         (format-time-string "%Y-%m-%d %a %H:%M")))
           (already-added nil))
      ;; Check if the heading is already in the daily file
      (with-current-buffer (find-file-noselect today-file)
        (goto-char (point-min))
        (while (re-search-forward (format "^** [^*]+ %s" (regexp-quote heading)) nil t)
          (setq already-added t)))
      ;; Only append if the heading is not already in the file
      (unless already-added
        (with-current-buffer (find-file-noselect today-file)
          (goto-char (point-min))
          ;; Ensure section heading exists
          (unless (re-search-forward (regexp-quote section-name) nil t)
            (goto-char (point-max))
            (insert "\n" section-name))
          (re-search-forward (regexp-quote section-name) nil t)
          (outline-end-of-subtree)
          (insert "\n" entry)
          (save-buffer)))
      (org-roam-db-sync)))

  (defun my/org-roam-handle-todo ()
    "Handle TODO items by copying their heading to today's daily file when their state changes."
    (interactive)
    (let ((current-state (org-get-todo-state)))
      (when (member current-state
                    '("DONE" "KILL" "WAIT" "HOLD" "YES" "NO" "OKAY"))
        (my/org-roam-copy-heading-to-today))))
  (add-hook 'org-after-todo-state-change-hook #'my/org-roam-handle-todo))

(defun my/org-roam-add-obsidian-compatibility ()
  "Update each Org-roam note with Obsidian-compatible properties for links and tags."
  (interactive)
  ;; Ensure Org-roam is loaded, and its DB is up-to-date.
  (require 'org-roam)
  (org-roam-db-sync)

  (message "Starting Org-roam to Obsidian compatibility updates...")

  ;; Grab a list of all nodes using org-roam-node-list
  (let ((nodes (org-roam-node-list)))
    (dolist (node nodes)
      (let* ((org-file (org-roam-node-file node))
             (org-tags (org-roam-node-tags node))
             (org-filetags (org-roam-node-tags node)) ; Filetags doesn't work so I'm just doin this
             (all-tags (append org-tags org-filetags))
             ;; Convert to Obsidian-style tags (#tag)
             (obsidian-tags (mapcar (lambda (tag) (format "#%s" tag)) all-tags))
             (tags-string (string-join obsidian-tags " ")))

        ;; Only operate on files that actually exist
        (when (and org-file (file-exists-p org-file))
          (with-current-buffer (find-file-noselect org-file)
            (save-excursion
              (goto-char (point-min))
              ;; Check if "File Data" heading already exists in the buffer.
              (let ((file-data-heading-pos
                     (org-find-exact-headline-in-buffer "File Data")))
                (if file-data-heading-pos
                    (goto-char file-data-heading-pos)  ; jump there if found
                  (org-insert-heading)                 ; else create new heading
                  (insert "File Data\n")))

              ;; Collect all Org-roam links in the file
              (let (wikilinks)
                (let ((parsed-org (org-element-parse-buffer)))
                  (org-element-map parsed-org 'link
                    (lambda (link)
                      ;; Try to detect an Org-roam link:
                      (when (my/org-roam-link-p link)
                        (let* ((target-node (my/org-roam-resolve-link link))
                               (target-title (and target-node
                                                  (org-roam-node-title target-node))))
                          (when target-title
                            (push (format "[[%s]]" target-title) wikilinks)))))))
                ;; Store them in a single property if any are found
                (when wikilinks
                  (org-set-property "Obsidian-Links"
                                    (string-join (nreverse wikilinks) " "))))

              ;; Store Obsidian-style tags if any
              (when (and all-tags (not (string= tags-string "")))
                (org-set-property "Obsidian-Tags" tags-string))

              (save-buffer))
            (kill-buffer))))))

  (message "Finished Org-roam to Obsidian compatibility updates."))

(defun my/org-roam-link-p (link)
  "Return non-nil if LINK is recognized as an Org-roam link.
This is a simple check for an 'id' link or a file link that Org-roam can handle.
Adjust if your Org-roam version uses a different convention."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link)))
    ;; If it's an 'id:' link, see if we can resolve a node from that ID.
    (cond
     ((string= type "id")
      (org-roam-node-from-id path))  ;; returns nil if no node is found
     ;; If you also want to catch 'file:' links that point to a roam note:
     ;; ((string= type "file")
     ;;  ...some check to see if it corresponds to a known Roam note...)
     (t nil))))
(defun my/org-roam-resolve-link (link)
  "Resolve an Org-roam LINK to its corresponding node.
Returns the Org-roam node if found, otherwise nil."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link)))
    (when (string= type "id")
      (org-roam-node-from-id path))))

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

(use-package! aas
  :commands aas-mode)

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))):w
