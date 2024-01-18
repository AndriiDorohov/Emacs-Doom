;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


;;(package! all-the-icons-ivy-rich)
(package! all-the-icons-dired)
(package! all-the-icons)
;;(package! vscode-icon)
;;(package! all-the-icons-completion)
;;(package! treemacs-all-the-icons)

(package! ivy-posframe)
(package! bm)
(package! rainbow-mode)
(package! nyan-mode)
(package! tree-edit)

;;NAVIGATION
(package! treemacs-projectile)
(package! treemacs-evil)
(package! treemacs-icons-dired)
(package! treemacs-magit)
(package! treemacs-persp)
(package! treemacs-tab-bar)
(package! dirvish)
(package! dired-rainbow)


;;; VISUAL
(package! centaur-tabs)
(package! highlight-indent-guides)

;;; AUTOCOMPLETE
(package! corfu)

;;; JS/TS
(package! tide)
(package! typescript-mode)
(package! npm)
(package! ng2-mode)
(package! nodejs-repl)
(package! ts-docstr
  :recipe (:host github :repo "emacs-vs/ts-docstr" :files (:defaults "langs/*.el")))

;;; Treesitter
(package! tree-sitter)
(package! tree-sitter-langs)
(package! eask)
(package! tree-edit)
(package! evil-tree-edit)

;;FORMAT
(package! emmet-mode)
(package! prettier)
(package! format-all)

;;DEBUG
(package! dap-mode)

;;GIT
(package! msgu :recipe (:host github :repo "jcs-elpa/msgu"))
(package! forge)
(package! blamer)
;;(package! sideline :recipe (:host github :repo "emacs-sideline/sideline"))
;;(package! sideline-blame :recipe (:host github :repo "emacs-sideline/sideline-blame"))
;;(package! sideline-lsp)
;;(package! sideline-flycheck)
(package! gist)
(package! git-messenger)

;;TERMINAL
(package! multi-vterm)
(package! vterm-toggle)

;;;; Jinja
(package! jinja2-mode)

;;;; Python
(package! pyvenv)
(package! python-mode)
(package! auto-complete)
(package! lsp-python-ms)
(package! lsp-pyright)
(package! lsp-jedi)
(package! lsp-ui)
;;(package! poetry)

;;; Markup
(package! pug-mode)
(package! auto-rename-tag)

;;KEYS
(package! evil-leader)
(package! evil-matchit)
(package! hydra)

;;HTTP
(package! websocket)
(package! restclient)

;;EDIT
(package! move-text)

;;EMAIL
(package! mu4e-alert)
(package! mu4e-views)
(package! xwwp)
(package! xwidgets-reuse)

;;;; Vue volar lsp
(package! lsp-volar :recipe (:host github :repo "jadestrong/lsp-volar"))
(package! web-mode)

;;; Undo
(package! vundo)
(package! undo-fu-session)
(package! undo-tree :disable t)

;;OTHERS
(package! turbo-log :recipe (:host github :repo "artawower/turbo-log"))
(package! reverse-im)
(package! grip-mode)
(package! origami)
(package! lsp-grammarly)
(package! outline-minor-faces)
(package! google-translate)
(package! telega)
(package! minions)

(package! vlf :recipe (:host github :repo "emacs-straight/vlf" :files ("*.el"))
  :pin "cacdb359f8c37c6e7e4c7937462b632d22462130")
(package! aas :recipe (:host github :repo "ymarco/auto-activating-snippets")
  :pin "e92b5cffa4e87c221c24f3e72ae33959e1ec2b68")

(package! screenshot :recipe (:host github :repo "tecosaur/screenshot"))

(package! etrace :recipe (:host github :repo "aspiers/etrace")
  :pin "2291ccf2f2ccc80a6aac4664e8ede736ceb672b7")
(package! theme-magic :pin "844c4311bd26ebafd4b6a1d72ddcc65d87f074e3")
(package! string-inflection :pin "50ad54970b3cc79b6b83979bde9889ad9a9e1a9c")
(package! info-colors :pin "2e237c301ba62f0e0286a27c1abe48c4c8441143")
(package! keycast :pin "72d9add8ba16e0cae8cfcff7fc050fa75e493b4e")

(package! gif-screencast :pin "adec408e6adab2e8e057fe0ad828749f473bfb83")
(package! page-break-lines :recipe (:host github :repo "purcell/page-break-lines")
  :pin "79eca86e0634ac68af862e15c8a236c37f446dcd")




(package! tsi :recipe (:host github :repo "orzechowskid/tsi.el"))

;;; Disable doom crap
(package! evil-goggles :disable t)

(package! quicktype :recipe (:host github :repo "artawower/quicktype.el"))

;;; Org mode
;; (package! polymode)
;; (package! poly-org)
(package! ox-gfm)
(package! ob-dart)
(package! oauth2)
(package! org-caldav)
(package! org-superstar)
(unpin! org-roam)
(package! web-roam
  :recipe (:host github :repo "artawower/web-roam.el"))
(package! org-roam)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! org-fancy-priorities)
;; (package! org-modern)
;;;; Org babel
(package! ob-restclient)
(package! ob-async)
;;; Reading
;;;; Rss
(package! elfeed-score)
;;;; Pocket
(package! pocket-reader)
;;; Agenda
