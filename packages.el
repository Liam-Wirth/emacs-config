;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)
(package! mips-mode)
(package! rainbow-delimiters)
(package! hl-todo)
(package! all-the-icons)
(package! elpaca)
(package! company)
(package! company-box)
(package! rainbow-mode)
(package! evil-collection)
(package! evil-tutor)
(package! counsel)
(package! ivy)
(package! ivy-rich)
(package! pdf-tools)
(package! company-math)
(package! flycheck-aspell)
(package! calfw)
(package! calfw-org)
(package! dired-open)
(package! dired-subtree)
(package! dirvish)
(package! ednc)
(package! emojify)
(package! ivy-posframe)
(package! ivy-hydra)
(package! mw-thesaurus)
(package! ox-gemini)
(package! peep-dired)
(package! highlight-symbol)
(package! resize-window)
(package! tldr)
(package! wc-mode)
(package! beacon)
(package! clippy)
(package! minimap)
(package! olivetti)
;; extra packages
(package! smooth-scrolling)
(package! good-scroll)
(package! scroll-on-jump)
(package! iscroll :recipe (:host github :repo "casouri/iscroll"))
                                        ;(package! yascroll)
(package! focus)
(package! suggest)
(package! company-fuzzy)
(package! ivy-bibtex)
(package! jinx)
(package! keycast)
(package! gruvbox-theme)
(package! vlf)
(package! calctex :recipe (:host github :repo "johnbcoughlin/calctex"
                           :files ("*.el" "calctex/*.el" "calctex-contrib/*.el" "org-calctex/*.el" "vendor")))

(package! lexic)
(package! string-inflection)
(package! org :recipe
  (:host nil :repo "https://code.tecosaur.net/mirrors/org-mode.git" :remote "mirror" :fork
   (:host nil :repo "https://code.tecosaur.net/tec/org-mode.git" :branch "dev" :remote "tecosaur")
   :files
   (:defaults "etc")
   :build t :pre-build
   (with-temp-file "lisp/org-version.el"
     (require 'lisp-mnt)
     (let
         ((version
           (with-temp-buffer
             (insert-file-contents "lisp/org.el")
             (lm-header "version")))
          (git-version
           (string-trim
            (with-temp-buffer
              (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
              (buffer-string)))))
       (insert
        (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
        (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
        "(provide 'org-version)\n"))))
  :pin nil)

(unpin! org) ; there be bugs
(package! org-contrib
  ;; The `sr.ht' repo has been a bit flaky as of late.
  :recipe (:host github :repo "emacsmirror/org-contrib"
           :files ("lisp/*.el"))
  :pin "351c71397d893d896a47ad7e280607b4d59b84e4")
(package! org-roam-ui)
(package! org-fragtog)
(package! org-auto-tangle)
(package! engrave-faces)
(package! org-web-tools)
(package! org-roam)
(package! org-modern)
(package! toc-org)
(package! org-appear)
(package! graphviz-dot-mode :pin "8ff793b13707cb511875f56e167ff7f980a31136")
(package! dimmer)
(package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree")
  :pin "207c748aa5fea8626be619e8c55bdb1c16118c25")
(package! aas :recipe (:host github :repo "ymarco/auto-activating-snippets")
  :pin "ddc2b7a58a2234477006af348b30e970f73bc2c1")
(package! info-colors :pin "2e237c301ba62f0e0286a27c1abe48c4c8441143")
(package! doct
  :recipe (:host github :repo "progfolio/doct")
  :pin "5cab660dab653ad88c07b0493360252f6ed1d898")
(package! zoxide)
(package! svg-tag-mode
  :recipe (:host github :repo "rougier/svg-tag-mode"))

(package! ox-chameleon
  :recipe (:host github :repo "tecosaur/ox-chameleon"))
(package! org-tree-slide
  :recipe (:host github :repo "takaxp/org-tree-slide"))

(package! wakatime-mode)

(package! ox-ioslide)


(package! org-cliplink
  :pin "13e0940")
;; The :pin is optional. It references a commit so the version doesn’t drift.


(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

(package! md-roam
  :recipe (:host github :repo "nobiot/md-roam"))

(package! org-similarity :recipe (:host github :repo "brunoarine/org-similarity"
   :branch "main"))

(package! org-roam-timestamps)

(package! copilot-chat)



(provide 'packages)



;;; packages.el ends here
