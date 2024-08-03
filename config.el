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
(setq org-directory "~/org/")
;; I've been on-and off trying to use the org agenda, and i like the ideas of org-roam-daily as a way to quickly make/maintain daily notes.
;; I thought to myself "why not try to combine the two?"
(setq org-agenda-files '("~/org/roam/daily/"))

(unless (file-exists-p (expand-file-name "persp" doom-cache-dir))
  (make-directory (expand-file-name "persp/" doom-cache-dir) t))
(defun my/persp-save-session-with-name (name)
  "save the current session with a specified NAME."
  (interactive "sEnter session name: ")
  (persp-save-state-to-file (concat persp-save-dir name)))



(after! persp-mode)
  ;;by default persp save dir is .config/emacs/.local/etc/workspaces I'm chill w/ that

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
      evil-split-window-below t)

(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

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

(global-set-key [remap dabbrev-expand] #'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

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
                    :height 140
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :font "Overpass"
                    :height 120
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono"
                    :height 120
                    :weight 'medium);; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
;;(set-face-attribute 'font-lock-keyword-face nil
;; :slant 'italic)
(set-face-attribute 'doom-serif-font (font-spec :family "IBM Plex Mono" :size 22 :weight 'light))
(set-face-attribute 'doom-symbol-font (font-spec :family "JuliaMono"))
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-15"))

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

(defvar splash-phrase-source-folder
  (expand-file-name "misc/splash-phrases" doom-private-dir)
  "A folder of text files with a fun phrase on each line.")

(defvar splash-phrase-sources
  (let* ((files (directory-files splash-phrase-source-folder nil "\\.txt\\'"))
         (sets (delete-dups (mapcar
                             (lambda (file)
                               (replace-regexp-in-string "\\(?:-[0-9]+-\\w+\\)?\\.txt" "" file))
                             files))))
    (mapcar (lambda (sset)
              (cons sset
                    (delq nil (mapcar
                               (lambda (file)
                                 (when (string-match-p (regexp-quote sset) file)
                                   file))
                               files))))
            sets))
  "A list of cons giving the phrase set name, and a list of files which contain phrase components.")

(defvar splash-phrase--cached-lines nil)

(defvar splash-phrase-set
  (nth (random (length splash-phrase-sources)) (mapcar #'car splash-phrase-sources))
  "The default phrase set. See `splash-phrase-sources'.")

(defun splash-phrase-set-random-set ()
  "Set a new random splash phrase set."
  (interactive)
  (setq splash-phrase-set
        (nth (random (1- (length splash-phrase-sources)))
             (cl-set-difference (mapcar #'car splash-phrase-sources) (list splash-phrase-set))))
  (+doom-dashboard-reload t))

(defun splash-phrase-select-set ()
  "Select a specific splash phrase set."
  (interactive)
  (setq splash-phrase-set (completing-read "Phrase set: " (mapcar #'car splash-phrase-sources)))
  (+doom-dashboard-reload t))

(defun splash-phrase-get-from-file (file)
  "Fetch a random line from FILE."
  (let ((lines (or (cdr (assoc file splash-phrase--cached-lines))
                   (cdar (push (cons file
                                     (with-temp-buffer
                                       (insert-file-contents (expand-file-name file splash-phrase-source-folder))
                                       (split-string (string-trim (buffer-string)) "\n")))
                               splash-phrase--cached-lines)))))
    (nth (random (length lines)) lines)))

(defun splash-phrase (&optional set)
  "Construct a splash phrase from SET. See `splash-phrase-sources'."
  (mapconcat
   #'splash-phrase-get-from-file
   (cdr (assoc (or set splash-phrase-set) splash-phrase-sources))
   " "))

(defun splash-phrase-dashboard-formatted ()
  "Get a splash phrase, flow it over multiple lines as needed, and fontify it."
  (mapconcat
   (lambda (line)
     (+doom-dashboard--center
      +doom-dashboard--width
      (with-temp-buffer
        (insert-text-button
         line
         'action
         (lambda (_) (+doom-dashboard-reload t))
         'face 'doom-dashboard-menu-title
         'mouse-face 'doom-dashboard-menu-title
         'help-echo "Random phrase"
         'follow-link t)
        (buffer-string))))
   (split-string
    (with-temp-buffer
      (insert (splash-phrase))
      (setq fill-column (min 70 (/ (* 2 (window-width)) 3)))
      (fill-region (point-min) (point-max))
      (buffer-string))
    "\n")
   "\n"))

(defun splash-phrase-dashboard-insert ()
  "Insert the splash phrase surrounded by newlines."
  (insert "\n" (splash-phrase-dashboard-formatted) "\n"))

(after! centaur-tabs

  (setq centaur-tabs-height 36
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "o"
        centaur-tabs-close-button "×"
        centaur-tabs-set-bar 'above
        centaur-tabs-gray-out-icons 'buffer)
  )

(use-package! info-colors
:commands (info-colors-fontify-node))

(defvar my-window-alpha 100
  "I like my window transparency opaque by default")
(defun kb/toggle-window-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha-transparency 0))
    (pcase (frame-parameter nil 'alpha-background)
      (alpha-transparency (set-frame-parameter nil 'alpha-background 100))
      (t (set-frame-parameter nil 'alpha-background alpha-transparency)))))
(global-set-key (kbd "<f12>") 'kb/toggle-window-transparency)

(use-package wakatime-mode
  :ensure t)

(global-activity-watch-mode)

(after! which-key
  (setq which-key-idle-delay 0.2))

(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))
(setq which-key-allow-multiple-replacements t)

(use-package! elcord
  :commands elcord-mode
  :config
  (setq elcord-use-major-mode-as-main-icon t))

(defface variable-pitch-serif
    '((t (:family "serif")))
    "A variable-pitch face with serifs."
    :group 'basic-faces)

(defcustom variable-pitch-serif-font (font-spec :family "serif")
  "The font face used for `variable-pitch-serif'."
  :group 'basic-faces
  :type '(restricted-sexp :tag "font-spec" :match-alternatives (fontp))
  :set (lambda (symbol value)
         (set-face-attribute 'variable-pitch-serif nil :font value)
         (set-default-toplevel-value symbol value)))

;;(defvar mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
;;  "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")
;;(defun init-mixed-pitch-h ()
;;  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
;;Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
;;  (when (memq major-mode mixed-pitch-modes)
;;    (mixed-pitch-mode 1))
;;  (dolist (hook mixed-pitch-modes)
;;    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))
;;(add-hook 'doom-init-ui-hook #'init-mixed-pitch-h)
;;
;;(autoload #'mixed-pitch-serif-mode "mixed-pitch"
;;  "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch." t)
;;
;;(setq! variable-pitch-serif-font (font-spec :family "Alegreya" :size 27))
;;
;;(after! mixed-pitch
;;  (setq mixed-pitch-set-height t)
;;  (set-face-attribute 'variable-pitch-serif nil :font variable-pitch-serif-font)
;;  (defun mixed-pitch-serif-mode (&optional arg)
;;    "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch."
;;    (interactive)
;;    (let ((mixed-pitch-face 'variable-pitch-serif))
;;      (mixed-pitch-mode (or arg 'toggle)))))
;;
;;(set-char-table-range composition-function-table ?f '(["\\(?:ff?[fijlt]\\)" 0 font-shape-gstring]))
;;(set-char-table-range composition-function-table ?T '(["\\(?:Th\\)" 0 font-shape-gstring]))

(defvar +zen-serif-p t
  "Whether to use a serifed font with `mixed-pitch-mode'.")
(defvar +zen-org-starhide t
  "The value `org-modern-hide-stars' is set to.")

(after! writeroom-mode
  (defvar-local +zen--original-org-indent-mode-p nil)
  (defvar-local +zen--original-mixed-pitch-mode-p nil)
  (defun +zen-enable-mixed-pitch-mode-h ()
    "Enable `mixed-pitch-mode' when in `+zen-mixed-pitch-modes'."
    (when (apply #'derived-mode-p +zen-mixed-pitch-modes)
      (if writeroom-mode
          (progn
            (setq +zen--original-mixed-pitch-mode-p mixed-pitch-mode)
            (funcall (if +zen-serif-p #'mixed-pitch-serif-mode #'mixed-pitch-mode) 1))
        (funcall #'mixed-pitch-mode (if +zen--original-mixed-pitch-mode-p 1 -1)))))
  (defun +zen-prose-org-h ()
    "Reformat the current Org buffer appearance for prose."
    (when (eq major-mode 'org-mode)
      (setq display-line-numbers nil
            visual-fill-column-width 60
            org-adapt-indentation nil)
      (when (featurep 'org-modern)
        (setq-local org-modern-star '("🙘" "🙙" "🙚" "🙛")
                    ;; org-modern-star '("🙐" "🙑" "🙒" "🙓" "🙔" "🙕" "🙖" "🙗")
                    org-modern-hide-stars +zen-org-starhide)
        (org-modern-mode -1)
        (org-modern-mode 1))
      (setq
       +zen--original-org-indent-mode-p org-indent-mode)
      (org-indent-mode -1)))
  (defun +zen-nonprose-org-h ()
    "Reverse the effect of `+zen-prose-org'."
    (when (eq major-mode 'org-mode)
      (when (bound-and-true-p org-modern-mode)
        (org-modern-mode -1)
        (org-modern-mode 1))
      (when +zen--original-org-indent-mode-p (org-indent-mode 1))))
  (pushnew! writeroom--local-variables
            'display-line-numbers
            'visual-fill-column-width
            'org-adapt-indentation
            'org-modern-mode
            'org-modern-star
            'org-modern-hide-stars)
  (add-hook 'writeroom-mode-enable-hook #'+zen-prose-org-h)
  (add-hook 'writeroom-mode-disable-hook #'+zen-nonprose-org-h))

(defun my/dired-copy-images-links ()
  "Works only in dired-mode, put in kill-ring,
ready to be yanked in some other org-mode file,
the links of marked image files using file-name-base as #+CAPTION.
If no file marked then do it on all images files of directory.
No file is moved nor copied anywhere.
This is intended to be used with org-redisplay-inline-images."
  (interactive)
  (if (derived-mode-p 'dired-mode)                           ; if we are in dired-mode
      (let* ((marked-files (dired-get-marked-files))         ; get marked file list
             (number-marked-files                            ; store number of marked files
              (string-to-number                              ; as a number
               (dired-number-of-marked-files))))             ; for later reference
        (when (= number-marked-files 0)                      ; if none marked then
          (dired-toggle-marks)                               ; mark all files
          (setq marked-files (dired-get-marked-files)))      ; get marked file list
        (message "Files marked for copy")                    ; info message
        (dired-number-of-marked-files)                       ; marked files info
        (kill-new "\n")                                      ; start with a newline
        (dolist (marked-file marked-files)                   ; walk the marked files list
          (when (org-file-image-p marked-file)               ; only on image files
            (kill-append                                     ; append image to kill-ring
             (concat "#+CAPTION: "                           ; as caption,
                     (file-name-base marked-file)            ; use file-name-base
                     "\n[[file:" marked-file "]]\n\n") nil))) ; link to marked-file
        (when (= number-marked-files 0)                      ; if none were marked then
          (dired-toggle-marks)))                             ; unmark all
    (message "Error: Does not work outside dired-mode")      ; can't work not in dired-mode
    (ding)))                                                 ; error sound

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

(after! spell-fu
  (cl-pushnew 'org-modern-tag (alist-get 'org-mode +spell-excluded-faces-alist)))

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
(after! org (plist-put org-format-latex-options :scale 2.0))

(after! org
  (setq org-roam-directory  "~/org/roam/")
  (setq org-modern-mode t)
  (setq org-roam-completion-everywhere t))

(defadvice! doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
  :around #'doom-modeline-buffer-file-name ; takes no args
  (if (s-contains-p org-roam-directory (or buffer-file-name ""))
      (replace-regexp-in-string
       "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
       "🢔(\\1-\\2-\\3) "
       (subst-char-in-string ?_ ?  buffer-file-name))
    (funcall orig-fun)))

(defadvice! doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
  :around #'doom-modeline-buffer-file-name ; takes no args
        (let ((file-name (or buffer-file-name "")))
  (if (s-contains-p org-roam-directory (or buffer-file-name ""))
      (concat "🢔(" (my/org-roam-file-name-without-numbers file-name) ")")
    (funcall orig-fun))))

(defun +yas/org-src-header-p ()
  "Determine whether `point' is within a src-block header or header-args."
  (pcase (org-element-type (org-element-context))
    ('src-block (< (point) ; before code part of the src-block
                   (save-excursion (goto-char (org-element-property :begin (org-element-context)))
                                   (forward-line 1)
                                   (point))))
    ('inline-src-block (< (point) ; before code part of the inline-src-block
                          (save-excursion (goto-char (org-element-property :begin (org-element-context)))
                                          (search-forward "]{")
                                          (point))))
    ('keyword (string-match-p "^header-args" (org-element-property :value (org-element-context))))))

(defun +yas/org-prompt-header-arg (arg question values)
  "Prompt the user to set ARG header property to one of VALUES with QUESTION.
The default value is identified and indicated. If either default is selected,
or no selection is made: nil is returned."
  (let* ((src-block-p (not (looking-back "^#\\+property:[ \t]+header-args:.*" (line-beginning-position))))
         (default
          (or
           (cdr (assoc arg
                       (if src-block-p
                           (nth 2 (org-babel-get-src-block-info t))
                         (org-babel-merge-params
                          org-babel-default-header-args
                          (let ((lang-headers
                                 (intern (concat "org-babel-default-header-args:"
                                                 (+yas/org-src-lang)))))
                            (when (boundp lang-headers) (eval lang-headers t)))))))
           ""))
         default-value)
    (setq values (mapcar
                  (lambda (value)
                    (if (string-match-p (regexp-quote value) default)
                        (setq default-value
                              (concat value " "
                                      (propertize "(default)" 'face 'font-lock-doc-face)))
                      value))
                  values))
    (let ((selection (consult--read values :prompt question :default default-value)))
      (unless (or (string-match-p "(default)$" selection)
                  (string= "" selection))
        selection))))

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

(defadvice! org-html-template-fancier (orig-fn contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options. Adds a few extra things to the body
compared to the default implementation."
  :around #'org-html-template
  (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
      (funcall orig-fn contents info)
    (concat
     (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
       (let* ((xml-declaration (plist-get info :html-xml-declaration))
              (decl (or (and (stringp xml-declaration) xml-declaration)
                        (cdr (assoc (plist-get info :html-extension)
                                    xml-declaration))
                        (cdr (assoc "html" xml-declaration))
                        "")))
         (when (not (or (not decl) (string= "" decl)))
           (format "%s\n"
                   (format decl
                           (or (and org-html-coding-system
                                    (fboundp 'coding-system-get)
                                    (coding-system-get org-html-coding-system 'mime-charset))
                               "iso-8859-1"))))))
     (org-html-doctype info)
     "\n"
     (concat "<html"
             (cond ((org-html-xhtml-p info)
                    (format
                     " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
                     (plist-get info :language) (plist-get info :language)))
                   ((org-html-html5-p info)
                    (format " lang=\"%s\"" (plist-get info :language))))
             ">\n")
     "<head>\n"
     (org-html--build-meta-info info)
     (org-html--build-head info)
     (org-html--build-mathjax-config info)
     "</head>\n"
     "<body>\n<input type='checkbox' id='theme-switch'><div id='page'><label id='switch-label' for='theme-switch'></label>"
     (let ((link-up (org-trim (plist-get info :html-link-up)))
           (link-home (org-trim (plist-get info :html-link-home))))
       (unless (and (string= link-up "") (string= link-home ""))
         (format (plist-get info :html-home/up-format)
                 (or link-up link-home)
                 (or link-home link-up))))
     ;; Preamble.
     (org-html--build-pre/postamble 'preamble info)
     ;; Document contents.
     (let ((div (assq 'content (plist-get info :html-divs))))
       (format "<%s id=\"%s\">\n" (nth 1 div) (nth 2 div)))
     ;; Document title.
     (when (plist-get info :with-title)
       (let ((title (and (plist-get info :with-title)
                         (plist-get info :title)))
             (subtitle (plist-get info :subtitle))
             (html5-fancy (org-html--html5-fancy-p info)))
         (when title
           (format
            (if html5-fancy
                "<header class=\"page-header\">%s\n<h1 class=\"title\">%s</h1>\n%s</header>"
              "<h1 class=\"title\">%s%s</h1>\n")
            (if (or (plist-get info :with-date)
                    (plist-get info :with-author))
                (concat "<div class=\"page-meta\">"
                        (when (plist-get info :with-date)
                          (org-export-data (plist-get info :date) info))
                        (when (and (plist-get info :with-date) (plist-get info :with-author)) ", ")
                        (when (plist-get info :with-author)
                          (org-export-data (plist-get info :author) info))
                        "</div>\n")
              "")
            (org-export-data title info)
            (if subtitle
                (format
                 (if html5-fancy
                     "<p class=\"subtitle\" role=\"doc-subtitle\">%s</p>\n"
                   (concat "\n" (org-html-close-tag "br" nil info) "\n"
                           "<span class=\"subtitle\">%s</span>\n"))
                 (org-export-data subtitle info))
              "")))))
     contents
     (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
     ;; Postamble.
     (org-html--build-pre/postamble 'postamble info)
     ;; Possibly use the Klipse library live code blocks.
     (when (plist-get info :html-klipsify-src)
       (concat "<script>" (plist-get info :html-klipse-selection-script)
               "</script><script src=\""
               org-html-klipse-js
               "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
               org-html-klipse-css "\"/>"))
     ;; Closing document.
     "</div>\n</body>\n</html>")))

(defadvice! org-html-toc-linked (depth info &optional scope)
  "Build a table of contents.

Just like `org-html-toc', except the header is a link to \"#\".

DEPTH is an integer specifying the depth of the table.  INFO is
a plist used as a communication channel.  Optional argument SCOPE
is an element defining the scope of the table.  Return the table
of contents as a string, or nil if it is empty."
  :override #'org-html-toc
  (let ((toc-entries
         (mapcar (lambda (headline)
                   (cons (org-html--format-toc-headline headline info)
                         (org-export-get-relative-level headline info)))
                 (org-export-collect-headlines info depth scope))))
    (when toc-entries
      (let ((toc (concat "<div id=\"text-table-of-contents\">"
                         (org-html--toc-text toc-entries)
                         "</div>\n")))
        (if scope toc
          (let ((outer-tag (if (org-html--html5-fancy-p info)
                               "nav"
                             "div")))
            (concat (format "<%s id=\"table-of-contents\">\n" outer-tag)
                    (let ((top-level (plist-get info :html-toplevel-hlevel)))
                      (format "<h%d><a href=\"#\" style=\"color:inherit; text-decoration: none;\">%s</a></h%d>\n"
                              top-level
                              (org-html--translate "Table of Contents" info)
                              top-level))
                    toc
                    (format "</%s>\n" outer-tag))))))))

(setq org-html-style-plain org-html-style-default
      org-html-htmlize-output-type 'css
      org-html-doctype "html5"
     )

;(defun my/insert-previous-daily-link ()
;  "insert link to the previous daily note, if available."
;  (interactive)
;  (let ((prev-note (org-roam-dailies-find-previous-note)))
;    (when prev-note
;      (insert (format "[[%s][previous daily note]]\n" prev-note)))))

(setq org-roam-dailies-capture-templates
      (let ((head
             (concat "#+title: %<%y-%m-%d (%A)>\n"
                     "#+startup: showall\n"
                     "#+filetags: dailies\n* daily overview\n"
                     "#+export_file_name: ~/org/exported/dalies/"
                     "\n#+begin_src emacs-lisp :results value raw\n"
                     "(as/get-daily-agenda \"%<%Y-%m-%d>\")\n"
                     "#+end_src\n"
                     "#+ Last Daily Entry: "
                     "\n* [/] do today\n* [/] maybe do today\n* journal\n* [/] Tasks\n")))
        `(("j" "journal" entry
           "* %<%H:%M> %?"
           :if-new (file+head+olp "%<%y-%m-%d>.org" ,head ("journal"))
           :empty-lines 1
           :jump-to-captured t)
          ("t" "do today" item
           "[ ] %i%?"
           :if-new (file+head+olp "%<%y-%m-%d>.org" ,head ("do today"))
           :immediate-finish t
           :empty-lines 1
           :jump-to-captured t)
          ("m" "maybe do today" item
           "[ ] %a"
           :if-new (file+head+olp "%<%y-%m-%d>.org" ,head ("maybe do today"))
           :immediate-finish t
           :empty-lines 1
           :jump-to-captured t))))

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

(use-package! org
:config
(setq org-fontify-quote-and-verse-blocks t
org-highlight-latex-and-related '(native script entities)
org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))
;(setq org-export-directory "~/org/exported")

(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t))))
  (custom-set-faces!
    `((org-quote)
      :foreground ,(doom-color 'blue) :extend t)
    `((org-block-begin-line org-block-end-line)
      :background ,(doom-color 'bg)))
  ;; Change how LaTeX and image previews are shown
  (setq org-highlight-latex-and-related '(native entities script)
        org-image-actual-width (min (/ (display-pixel-width) 3) 800))

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

(defun my/org-roam-create-daily-file-if-needed ()
  "Create the daily file with the specified template if it doesn't exist."
  (let* ((date-string (format-time-string "%y-%m-%d"))
         (file-name (concat date-string ".org"))
         (file-path (expand-file-name file-name "~/org/roam/daily"))
         (file-exists (file-exists-p file-path))
         (template (concat "#+title: " date-string " (%A)\n"
                           "#+startup: showall\n"
                           "#+filetags: dailies\n* daily overview\n"
                           "#+export_file_name: ~/org/exported/dalies/"
                           "\n#+begin_src emacs-lisp :results value raw\n"
                           "(as/get-daily-agenda \"" (format-time-string "%Y-%m-%d") "\")\n"
                           "#+end_src\n"
                           "#+ Last Daily Entry: "
                           "\n* [/] do today\n* [/] maybe do today\n* journal\n* [/] Tasks\n")))
        (unless file-exists
      (with-temp-buffer
        (insert template)
        (write-file file-path)))
    file-path))

(defun my/org-roam-copy-heading-to-today ()
  "Copy the heading of a completed TODO to today's daily file with 'DONE' before it and link back to the original, avoiding duplicates."
  (interactive)
  (let* ((today-file (my/org-roam-create-daily-file-if-needed))
         (original-file (buffer-file-name))
         (heading (save-excursion
                    (org-back-to-heading t)
                    (org-get-heading t t t t)))
         (link-to-original (org-link-make-string (concat "file:" (expand-file-name original-file)) heading))
         (entry (format "** DONE %s\n   %s" heading link-to-original))
         (already-added nil))
    ;; Check if the heading is already in the daily file
    (with-current-buffer (find-file-noselect today-file)
      (goto-char (point-min))
      (while (re-search-forward (format "^\*\* DONE %s" (regexp-quote heading)) nil t)
        (setq already-added t)))

    ;; Only append if the heading is not already in the file
    (unless already-added
      (with-current-buffer (find-file-noselect today-file)
        (goto-char (point-max))
        ;; Ensure "Tasks" heading exists
        (unless (re-search-forward "^\* \\[\\/] Tasks" nil t)
          (goto-char (point-max))
          (insert "\n* [1/1] Tasks\n"))
        (goto-char (point-max))
        ;; Insert only the heading and link
        (insert (format "\n%s\n" entry))
        (save-buffer)))))

(defun my/org-roam-handle-todo ()
  "Handle TODO items by copying their heading to today's daily file when their state changes."
  (interactive)
  (when (member org-state '("DONE" "TODO"))
    (my/org-roam-copy-heading-to-today)))

(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (my/org-roam-copy-heading-to-today))))

(use-package! flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))
(ispell-change-dictionary "en_US" t)

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

(use-package! graphviz-dot-mode
  :commands graphviz-dot-mode
  :mode '("\\.dot\\'" . graphviz-dot-mode)
  :init
  (after! org
    (setcdr (assoc "dot" org-src-lang-modes)
            'graphviz-dot)))

(use-package! company-graphviz-dot
  :after graphviz-dot-mode)

(setq yas-triggers-in-field t)

(use-package! aas
  :commands aas-mode)

;;"A variable-pitch face with serifs."
;;:group 'basic-faces)
;;
;;(defcustom variable-pitch-serif-font (font-spec :family "serif")
;;"The font face used for `variable-pitch-serif'."
;;:group 'basic-faces
;;:set (lambda (symbol value)
;;(set-face-attribute 'variable-pitch-serif nil :font value)
;;(set-default-toplevel-value symbol value)))
;;(setq org-pretty-mode t)








;;(after!
;;:and (org flycheck)
;;(defconst flycheck-org-lint-form
;;  (flycheck-prepare-emacs-lisp-form
;;    (require 'org)
;;    (require 'org-lint)
;;    (require 'org-attach)
;;    (let ((source (car command-line-args-left))
;;          (process-default-directory default-directory))
;;      (with-temp-buffer
;;        (insert-file-contents source 'visit)
;;        (setq buffer-file-name source)
;;        (setq default-directory process-default-directory)
;;        (delay-mode-hooks (org-mode))
;;        (setq delayed-mode-hooks nil)
;;        (dolist (err (org-lint))
;;          (let ((inf (cl-second err)))
;;            (princ (elt inf 0))
;;            (princ ": ")
;;            (princ (elt inf 2))
;;            (terpri)))))))
;;
;;(defconst flycheck-org-lint-variables
;;  '(org-directory
;;    org-id-locations
;;    org-id-locations-file
;;    org-attach-id-dir
;;    org-attach-use-inheritance
;;    org-attach-id-to-path-function-list
;;    org-link-parameters)
;;  "Variables inherited by the org-lint subprocess.")
;;
;;(defun flycheck-org-lint-variables-form ()
;;  (require 'org-attach)  ; Needed to make variables available
;;  `(progn
;;     ,@(seq-map (lambda (opt) `(setq-default ,opt ',(symbol-value opt)))
;;                (seq-filter #'boundp flycheck-org-lint-variables))))
;;
;;(eval ; To preveant eager macro expansion form loading flycheck early.
;; '(flycheck-define-checker org-lint
;;   "Org buffer checker using `org-lint'."
;;   :command ("emacs" (eval flycheck-emacs-args)
;;             "--eval" (eval (concat "(add-to-list 'load-path \""
;;                                    (file-name-directory (locate-library "org"))
;;                                    "\")"))
;;             "--eval" (eval (flycheck-sexp-to-string
;;                             (flycheck-org-lint-variables-form)))
;;             "--eval" (eval (flycheck-sexp-to-string
;;                             (flycheck-org-lint-customisations-form)))
;;             "--eval" (eval flycheck-org-lint-form)
;;             "--" source)
;;   :error-patterns
;;   ((error line-start line ": " (message) line-end))
;;   :modes org-mode))
;;
;;(add-to-list 'flycheck-checkers 'org-lint)
;;
;;(defun flycheck-org-lint-customisations-form ()
;;  `(progn
;;     (require 'ox)
;;     (cl-pushnew '(:latex-cover-page nil "coverpage" nil)
;;                 (org-export-backend-options (org-export-get-backend 'latex)))
;;     (cl-pushnew '(:latex-font-set nil "fontset" nil)
;;                 (org-export-backend-options (org-export-get-backend 'latex))))))


