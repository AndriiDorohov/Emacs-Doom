;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


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

;;:------------------------
;;; Personal Information
;;:------------------------

(setq user-full-name "AndriiDorohov"
      user-mail-address "wedpositive@gmail.com")
(setq auth-sources '("~/.authinfo")
      auth-source-cache-expiry nil) ; default is 7200 (2h)

;;:------------------------
;;; Better defaults
;;:------------------------


(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 67108864                         ; Raise undo-limit to 64mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      ;; scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2                             ; It's nice to maintain a little margin
      display-time-default-load-average nil      ; I don't think I've ever found this useful
      warning-minimum-level :emergency
      gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)	 ;;1MB
      inhibit-compacting-font-caches t	 	 ;;Inhibit compacting font cache
      ad-redefinition-action 'accept)		;;Turn off ad-handle-definition: `tramp-read-passwd’ got redefined

;;(display-time-mode 1)                             ; Enable time in the mode-line
(global-subword-mode 1)                           ; Iterate through CamelCase words
;;(display-battery-mode 1)

(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))
(setq evil-vsplit-window-right t
      evil-split-window-below t)
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
;; (setq-default major-mode 'org-mode)

;;:------------------------
;;; Doom
;;:------------------------


(setq doom-font (font-spec :family "JetBrains Mono" :size 16)
      doom-big-font (font-spec :family "JetBrains Mono" :size 26)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 16)
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-emoji-font (font-spec :family "Twitter Color Emoji")      
      doom-serif-font (font-spec :family "IBM Plex Mono" :size 12 :weight 'light))

(setq +m-color-main "#61AFEF"
      +m-color-secondary "red")

(setq doom-theme
      (if (getenv "DOOM_THEME")
          (intern (getenv "DOOM_THEME"))
        'doom-vibrant))
(delq! t custom-theme-load-path)
(remove-hook 'window-setup-hook #'doom-init-theme-h)
(add-hook 'after-init-hook #'doom-init-theme-h 'append)
(setq display-line-numbers-type 'relative)
(evil-define-command +evil-buffer-org-new (count file)
  "Creates a new ORG buffer replacing the current window, optionally
   editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new org*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (org-mode)
        (setq-local doom-real-buffer-p t)))))

(map! :leader
      (:prefix "b"
       :desc "New empty Org buffer" "o" #'+evil-buffer-org-new))



(defconst jetbrains-ligature-mode--ligatures
  '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
    "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
    "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
    "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
    "[<" ">]" ":?>" ":?" "/=" "[||]" "!!" "?:" "?." "::"
    "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
    "<:" ":<" ":>" ">:" "<>" "***" ";;" "/==" ".=" ".-" "__"
    "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
    ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
    "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
    "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
    "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
    "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
    "&="))

(sort jetbrains-ligature-mode--ligatures (lambda (x y) (> (length x) (length y))))

(dolist (pat jetbrains-ligature-mode--ligatures)
  (set-char-table-range composition-function-table
                        (aref pat 0)
                        (nconc (char-table-range composition-function-table (aref pat 0))
                               (list (vector (regexp-quote pat)
                                             0
                                             'compose-gstring-for-graphic)))))

(defun correct-my-fringe (&optional ignore)
  (unless (eq fringe-mode '16)
    (fringe-mode '16)))

(add-hook 'after-init-hook #'correct-my-fringe)
(add-hook 'buffer-list-update-hook #'correct-my-fringe)

(setq evil-normal-state-cursor '(box "#41a7fc")
      evil-insert-state-cursor '(bar "#00AEE8")
      evil-visual-state-cursor '(hollow "#c75ae8"))

;;:------------------------
;;; Transparency
;;:------------------------

(progn
  (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
  (add-to-list 'default-frame-alist '(alpha . (100 . 100))))

;;:------------------------
;;; Toggle transparency
;;:------------------------

(setq my-transparency-disabled-p t)
(defun my-toggle-transparency ()
  "Toggle transparency"
  (interactive)
  (let* ((not-transparent-p (and (boundp 'my-transparency-disabled-p) my-transparency-disabled-p))
         (alpha (if not-transparent-p 100 85)))
    (setq my-transparency-disabled-p (not not-transparent-p))
    (message "%s" alpha)
    (progn
      (set-frame-parameter (selected-frame) 'alpha `(,alpha . ,alpha))
      (add-to-list 'default-frame-alist `(alpha . (,alpha . ,alpha))))))


;;:------------------------
;;; !Pkg Doom modeline
;;:------------------------

;; This block defines `doom-modeline-update-pdf-pages',
;; `+doom-modeline-micro-clock', `+doom-modeline--clock-text',
;; `+doom-modeline-clock-text-format',
;; `+doom-modeline-micro-clock--cache',
;; `+doom-modeline-micro-clock-inverse-size',
;; `+doom-modeline-micro-clock-minute-resolution', `micro-clock-svg',
;; `micro-clock-minute-hand-ratio', `micro-clock-hour-hand-ratio', and
;; `doom-modeline-conditional-buffer-encoding'.


(with-eval-after-load 'doom-modeline
  (custom-set-faces!
    '(doom-modeline-buffer-modified :foreground "orange"))
  (setq doom-modeline-height 45)

  (defun doom-modeline-conditional-buffer-encoding ()
    "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
    (setq-local doom-modeline-buffer-encoding
                (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                   '(coding-category-undecided coding-category-utf-8))
                             (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                  t))
    )
  (add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)



  (doom-modeline-def-segment buffer-name
    "Display the current buffer's name, without any other information."
    (concat
     doom-modeline-spc
     (doom-modeline--buffer-name))
  )
  (doom-modeline-def-segment pdf-icon
    "PDF icon from nerd-icons."
    (concat
     doom-modeline-spc
     (doom-modeline-icon sucicon "nf-seti-pdf" nil nil
                     :face (if (doom-modeline--active)
                               'nerd-icons-red
                             'mode-line-inactive)
                     :v-adjust 0.02))
  )
  (defun doom-modeline-update-pdf-pages ()
    "Update PDF pages."
    (setq doom-modeline--pdf-pages
          (let ((current-page-str (number-to-string (eval `(pdf-view-current-page))))
                (total-page-str (number-to-string (pdf-cache-number-of-pages))))
            (concat
             (propertize
              (concat (make-string (- (length total-page-str) (length current-page-str)) ? )
                      " P" current-page-str)
              'face 'mode-line)
             (propertize (concat "/" total-page-str) 'face 'doom-modeline-buffer-minor-mode)))
    )
  )

  (doom-modeline-def-segment pdf-pages
    "Display PDF pages."
    (if (doom-modeline--active) doom-modeline--pdf-pages
      (propertize doom-modeline--pdf-pages 'face 'mode-line-inactive))
  )
  (doom-modeline-def-modeline 'pdf
    '(bar window-number pdf-pages pdf-icon buffer-name)
    '(misc-info matches major-mode process vcs)
  )
)

(custom-set-faces
  '(mode-line ((t (:family "JetBrains Mono" :height 0.98))))
  '(mode-line-active ((t (:family "JetBrains Mono" :height 0.98)))) ; For 29+
  '(mode-line-inactive ((t (:family "JetBrains Mono" :height 0.98)))))


(use-package! doom-modeline
  :hook (after-init . doom-modeline-mode)
  :defer t
  :config
  (setq doom-modeline-buffer-file-name-style 'file-name))

(require 'doom-modeline)

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))
(setq doom-modeline-height 45)
(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(doom-modeline-def-segment buffer-name
  "Display the current buffer's name, without any other information."
  (concat
   doom-modeline-spc
   (doom-modeline--buffer-name)))

(doom-modeline-def-segment pdf-icon
  "PDF icon from nerd-icons."
  (concat
   doom-modeline-spc
   (doom-modeline-icon sucicon "nf-seti-pdf" nil nil
                       :face (if (doom-modeline--active)
                                 'nerd-icons-red
                               'mode-line-inactive)
                       :v-adjust 0.02)))

(defun doom-modeline-update-pdf-pages ()
  "Update PDF pages."
  (setq doom-modeline--pdf-pages
        (let ((current-page-str (number-to-string (eval `(pdf-view-current-page))))
              (total-page-str (number-to-string (pdf-cache-number-of-pages))))
          (concat
           (propertize
            (concat (make-string (- (length total-page-str) (length current-page-str)) ? )
                    " P" current-page-str)
            'face 'mode-line)
           (propertize (concat "/" total-page-str) 'face 'doom-modeline-buffer-minor-mode)))))

(doom-modeline-def-segment pdf-pages
  "Display PDF pages."
  (if (doom-modeline--active) doom-modeline--pdf-pages
    (propertize doom-modeline--pdf-pages 'face 'mode-line-inactive)))

(doom-modeline-def-modeline 'pdf
  '(bar window-number pdf-pages pdf-icon buffer-name)
  '(misc-info matches major-mode process vcs))


;;; config--pkg-doom-modeline.el ends here



;;:------------------------
;;; !Pkg avy
;;:------------------------

(use-package! avy
  :defer t
  :bind (:map evil-normal-state-map
              ("SPC k l" . avy-kill-whole-line)
              ("SPC k r" . avy-kill-region))
  :custom
  (avy-single-candidate-jump t)
  (avy-keys '(?w ?e ?r ?t ?y ?u ?i ?o ?p ?a ?s ?d ?f ?g ?h ?j ?k ?l ?z ?x ?c ?v ?b ?n ?m)))


;;:------------------------
;;; !Pkg Centaur Tabs
;;:------------------------

(after! centaur-tabs
  (centaur-tabs-mode -1)
  (setq centaur-tabs-height 36
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "o"
        centaur-tabs-close-button "×"
        centaur-tabs-set-bar 'above
        centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-change-fonts "P22 Underground Book" 160))
;; (setq x-underline-at-descent-line t)

;;:------------------------
;;; Markdown
;;:------------------------

(add-hook! (gfm-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill)
(custom-set-faces!
  '(markdown-header-face-1 :height 1.25 :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-2 :height 1.15 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-3 :height 1.08 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-4 :height 1.00 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-5 :height 0.90 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-6 :height 0.75 :weight extra-bold :inherit markdown-header-face))


;;:------------------------
;;; Python
;;:------------------------


(use-package! python-mode
  :defer t
  :hook (python-mode . format-all-mode)
  :config
  (setq python-indent-level 4)
;;  (setq python-shell-virtualenv-path "/Users/apogee/.local/share/virtualenvs/myenv/")
  (add-hook 'python-mode-hook
            (lambda ()
              (require 'lsp-pyright)
              (lsp-deferred)
              (setq indent-tabs-mode nil)
              (setq tab-width 4)))
;;  (pyvenv-activate "/Users/apogee/.local/share/virtualenvs/myenv")
  )

;;PYENV

;;(use-package! pipenv
;;  :defer t
;;  :hook (python-mode . pipenv-mode)
;;  :config
;;  (pyvenv-workon "/Users/apogee/.local/share/virtualenvs/myenv")  
;;  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

;;Add myvenv
;;(defun my-activate-virtualenv ()
;;  (interactive)
;;  (let ((venv-path "/Users/apogee/.local/share/virtualenvs/myenv/bin/activate"))
;;    (when (file-exists-p venv-path)
;;      (vterm-send-string (format "source %s" venv-path))
;;     (vterm-send-return))))


;;(use-package pyvenv
;;  :config
;;  (pyvenv-mode 1)
;;  (pyvenv-workon "myenv"))

;;:------------------------
;;; DEBUG
;;:------------------------

(require 'dap-python)
(require 'dap-js)
(require 'dap-chrome)

(setq dap-python-debugger 'debugpy)

(dap-register-debug-template "Debug Current Python Buffer"
  (list :type "python"
        :name "Debug Current Python Buffer"
        :request "launch"
        :program nil)) 
(use-package! dap-mode
  :defer t
  :bind (:map evil-normal-state-map
              ("SPC d n" . dap-next)
              ("SPC d i" . dap-step-in)
              ("SPC d o" . dap-step-out)
              ("SPC d c" . dap-continue)
              ("SPC d Q" . dap-disconnect)
              ("SPC d q" . dap-disconnect)
              ("SPC d d" . (lambda () (interactive)
                             (call-interactively #'dap-debug)
                             (set-window-buffer nil (current-buffer))))
              ("SPC d r" . dap-debug-recent)
              ("SPC d l" . dap-ui-locals)
              ("SPC d b" . dap-ui-breakpoints)
              ("SPC d s" . dap-ui-sessions)
              ("SPC d e" . dap-debug-last)
              ("SPC d p" . (lambda () (interactive)
                             (set-window-buffer nil (current-buffer))
                             (dap-breakpoint-toggle)))
              ("SPC d e" . dap-debug-edit-template))
  :init
  (dap-mode 1)
  (dap-ui-controls-mode 1) 
  (setq dap-auto-configure-features '(sessions locals)))


;;:------------------------
;;; WEB
;;:------------------------

;; Enable / disable features for web-mode
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-interpolation t)
(custom-set-faces
  '(web-mode-block-face ((t (:background "#373742")))))


(use-package! web-mode
  :defer t
  :mode (("\\.vue\\'" . web-mode)
         ("\\.tsx\\'" . typescript-tsx-mode)
         ("\\.jsx\\'" . js2-jsx-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.api\\'" . web-mode)
         ("/some/react/path/.*\\.js[x]?\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-comment-formats
        '(("java"       . "/*")
          ("javascript" . "//")
          ("typescript" . "//")
          ("vue"        . "//")
          ("php"        . "/*")
          ("pug"        . "//")
          ("css"        . "/*")))
  (setq web-mode-markup-indent-offset 2)  
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0)
  (setq web-mode-comment-style 2)  
  (setq web-mode-content-types-alist
        '(("json" . "/some/path/.*\\.api\\'")
          ("xml"  . "/other/path/.*\\.api\\'")
          ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")))

  ;; Enable Tide for TypeScript in web-mode  
  (add-hook 'web-mode-hook
          (lambda ()
            (when (or (string-equal "tsx" (file-name-extension buffer-file-name))
                      (string-equal "jsx" (file-name-extension buffer-file-name)))
              (setup-tide-mode)))))  

  ;; enable typescript-tslint checker
;;  (flycheck-add-mode 'typescript-tslint 'web-mode)
;;  (flycheck-add-mode 'javascript-eslint 'web-mode)
;;  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)


;;:------------------------
;;; TS
;;:------------------------

(use-package! typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx))

  (setq typescript-indent-level 2))


(use-package tide
  :after (company flycheck)
  :hook ((typescript-ts-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))


;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
(use-package tsi
  :after tree-sitter
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :config
  (apheleia-global-mode +1))

(use-package eglot)


;;:------------------------
;;; JS
;;:------------------------

(use-package! js2-mode
  :defer t
  :hook (js2-mode . js2-highlight-unused-variables-mode))

;;:------------------------
;;; VUE
;;:------------------------

(use-package! lsp-volar
  :after lsp-mode)

;;:------------------------
;;; CSS
;;:------------------------

(use-package! css-mode
  :defer t
  :config
  (defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

  (defun run-sass-auto-fix ()
    "Run sass auto fix if cli tool exist"
    (interactive)
    (save-window-excursion
      (let ((default-directory (file-name-directory buffer-file-name)))
        (async-shell-command "sass-lint-auto-fix")
        ;; (revert-buffer-no-confirm)
        (message "SASS FORMATTED"))))
  ;; (add-hook 'scss-mode-hook '(lambda () (add-hook 'after-save-hook #'run-sass-auto-fix t t)))
  (add-hook 'scss-mode-hook #'(lambda () (add-hook 'before-save-hook #'format-all-buffer nil t))))


;;:------------------------
;;; Styled-components NPM
;;:------------------------

(use-package! polymode
   :after rjsx-mode
   :config
   (define-hostmode poly-rjsx-hostmode nil
     "RJSX hostmode."
     :mode 'rjsx-mode)
   (define-innermode poly-rjsx-cssinjs-innermode nil
     :mode 'css-mode
     :head-matcher "\\(styled\\|css\\)[.()<>[:alnum:]]?+`"
     :tail-matcher "\`"
     :head-mode 'host
     :tail-mode 'host)
   (define-polymode poly-rjsx-mode
     :hostmode 'poly-rjsx-hostmode
     :innermodes '(poly-rjsx-cssinjs-innermode))
   (add-to-list 'auto-mode-alist '("\\.js\\'" . poly-rjsx-mode)))

;;:------------------------
;;; EMMET
;;:------------------------

(use-package emmet-mode
  :hook ((scss-mode . emmet-mode)
         (css-mode . emmet-mode)
         (ng2-html-mode . emmet-mode)
         (html-mode . emmet-mode))
  :defer 5 
  :config
  (setq emmet-indent-after-insert nil
        emmet-indentation 2
        emmet-move-cursor-between-quotes t
        emmet-move-cursor-after-expanding nil
        emmet-self-closing-tag-style " /")
  :custom
  (emmet-expand-jsx-className? t)
  :init
  (defvar emmet-jsx-major-modes '(jsx-mode
                                  rjsx-mode
                                  js-jsx-mode
                                  js2-jsx-mode
                                  js-mode))
  (dolist (mode emmet-jsx-major-modes)
    (add-to-list 'emmet-jsx-major-modes mode)))

;;:------------------------
;;; JSON
;;:------------------------


(use-package! json-mode
  :defer t
  :hook (json-mode . format-all-mode))



;;:------------------------
;;; SASS AUTOFIX
;;:------------------------


(defun my-run-sass-auto-fix ()
  "Run sass auto fix if cli tool exist"
  (interactive)
  (save-window-excursion
    (let ((default-directory (file-name-directory buffer-file-name)))
      (async-shell-command "sass-lint-auto-fix")
      ;; (revert-buffer-no-confirm)
      (message "SASS FORMATTED"))))

;;:------------------------
;;; Flycheck
;;:------------------------

(use-package! flycheck
  :defer 2
  :bind (:map evil-normal-state-map
              ("SPC f ]" . flycheck-next-error)
              ("SPC f [" . flycheck-previous-error)
              ("SPC e l" . flycheck-list-errors)))
(global-flycheck-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;:------------------------
;;; Formatters
;;:------------------------

(use-package format-all
  :preface
  (defun ian/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (if (derived-mode-p 'prolog-mode)
        (prolog-indent-buffer)
      (format-all-buffer)))
  :config
  (global-set-key (kbd "M-F") #'ian/format-code)
  (add-hook 'prog-mode-hook #'format-all-ensure-formatter))



;;:------------------------
;;; PRETTIER
;;:------------------------

(use-package! prettier
  :defer 5
  :hook ((js2-mode
    js-mode
    rjsx-mode
    typescript-mode
    typescript-tsx-mode
    web-mode
    css-mode
    scss-mode
    less-css-mode
    json-mode
    yaml-mode
    markdown-mode
    vue-mode
    graphql-mode
    glimmer-mode
    hbs-mode
    xml-mode
    html-mode)
   . prettier-mode))

;;:------------------------
;;; LSP
;;:------------------------

(setq lsp-pyright-multi-root nil)
(use-package! lsp-pyright
  :defer t
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-auto-search-paths t)
  (setq lsp-pyright-log-level "trace")
  (setq lsp-pyright-multi-root nil)
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-pyright-diagnostic-mode "workspace"))

(use-package! lsp
  :hook ((js2-mode
          js-mode
          rjsx-mode
          typescript-mode
          typescript-tsx-mode
          web-mode
          css-mode
          scss-mode
          less-css-mode
          json-mode
          yaml-mode
          markdown-mode
          vue-mode
          graphql-mode
          glimmer-mode
          hbs-mode
          xml-mode
          html-mode
          some-other-mode
          another-mode
          ) . lsp-deferred)
  :bind (:map evil-normal-state-map
              ("SPC f n" . flycheck-next-error)
              ("g i" . lsp-goto-implementation)
              ("SPC l a" . lsp-execute-code-action)
              ("SPC l r" . lsp-find-references)
              ("SPC l w" . lsp-restart-workspace)
              ("SPC r l" . lsp))
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay 0.3)
  (lsp-enable-on-type-formatting nil)
  (lsp-eldoc-render-all nil)
  (lsp-prefer-flymake nil)
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-yaml-schemas '((kubernetes . ["/auth-reader.yaml", "/deployment.yaml"])))
  ;; Disable bottom help info
  (lsp-signature-render-documentation nil)
  (lsp-signature-auto-activate nil)
  ;; (lsp-use-plists t)
  (lsp-enable-file-watchers nil)
  (lsp-file-watch-threshold 5000)
  :config
  (setq lsp-javascript-display-return-type-hints t)
  (setq lsp-json-schemas
        `[
          (:fileMatch ["ng-openapi-gen.json"] :url "https://raw.githubusercontent.com/cyclosproject/ng-openapi-gen/master/ng-openapi-gen-schema.json")
          (:fileMatch ["package.json"] :url "http://json-schema.org/draft-07/schema")
          ])
  (set-face-attribute 'lsp-face-highlight-read nil :background "#61AFEF")
  ;; Flycheck patch checkers
  (require 'flycheck)
  (require 'lsp-diagnostics)
  (lsp-diagnostics-flycheck-enable)
  ;; Golang
  (defun lsp-go-install-save-hooks ()
    (flycheck-add-next-checker 'lsp '(warning . go-gofmt) 'append)
    (flycheck-add-next-checker 'lsp '(warning . go-golint))
    (flycheck-add-next-checker 'lsp '(warning . go-errcheck))
    (flycheck-add-next-checker 'lsp '(warning . go-staticcheck))

    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
        lsp-pyls-plugins-flake8-enabled nil)

  (setq lsp-disabled-clients '(html html-ls))
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\venv\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\pyenv\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.cache\\'")
  (set-face-attribute 'lsp-face-highlight-textual nil :background "#c0caf5")
  (setq lsp-eldoc-hook nil))



;;LSP ui

(use-package! lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-diagnostic-max-line-length 100
        lsp-ui-sideline-diagnostic-max-lines 8
        lsp-ui-doc-delay 2
        lsp-ui-doc-position 'top
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-border +m-color-main))

;;:------------------------
;;; Tree sitter (AST)
;;:------------------------

(use-package! eask)

(use-package! tree-sitter-langs
  :after tree-sitter)


(use-package! tree-sitter
  :after (tree-sitter-langs spell-fu)
:hook ((js2-mode
          js-mode
          rjsx-mode
          typescript-mode
          typescript-tsx-mode
          web-mode
          css-mode
          scss-mode
          less-css-mode
          json-mode
          yaml-mode
          markdown-mode
          vue-mode
          graphql-mode
          glimmer-mode
          hbs-mode
          xml-mode
          html-mode
          some-other-mode
          another-mode) . tree-sitter-hl-mode)
  :init
  (setq tsc-dyn-get-from nil)
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (setq tsc-dyn-get-from '(:github))
  (setq tsc-dyn-get-from nil)
  (advice-add 'tree-sitter-hl-mode :before 'my-set-spellfu-faces)
  (push '(ng2-html-mode . html) tree-sitter-major-mode-language-alist)
  (push '(ng2-ts-mode . typescript) tree-sitter-major-mode-language-alist)
  (push '(scss-mode . css) tree-sitter-major-mode-language-alist)
  (push '(scss-mode . typescript) tree-sitter-major-mode-language-alist)
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(use-package! tree-edit
  :defer t)

(use-package! evil-tree-edit
  :after tree-edit)

;;Edit throught emacs

(use-package! tree-edit
  :defer t)

(use-package! evil-tree-edit
  :after tree-edit)

;;TEST Tree sitter docs
(use-package ts-docstr
  :after tree-sitter
  :config
  (setq ts-docstr-key-support t)
  (setq ts-docstr-ask-on-enable t))

;;:------------------------
;;; Automatic rename html/xml tags
;;:------------------------

(use-package! auto-rename-tag
  :defer t
  :hook ((html-mode ng2-html-mode-hook vue-mode web-mode) . auto-rename-tag-mode)
  :config
  (auto-rename-tag-mode 1))
;;:------------------------
;;; Allow to transform PASCAL_CASE -> camelCase -> snake_case
;;:------------------------

(use-package! string-inflection
  :defer t
  :bind ("C-s-c" . string-inflection-all-cycle))

;;:------------------------
;;; Type from json
;;:------------------------

(use-package! quicktype
  :defer t
  :bind (("C-x j v" . quicktype-json-to-type)
         ("C-x j p" . quicktype-paste-json-as-type)
         ("C-x j q" . quicktype)))

;;:------------------------
;;; Treemacs
;;:------------------------

(use-package treemacs)

;;:------------------------
;;; Sync treemacs+LSP
;;:------------------------

(lsp-treemacs-sync-mode 1)


;;:------------------------
;;; Yasnippets
;;:------------------------

(add-to-list 'load-path
              "~/.config/doom/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)


;;:------------------------
;;; Keys
;;:------------------------


(setq python-mode-map
      (let ((map (make-sparse-keymap)))
        ;; Movement
        (define-key map [remap backward-sentence] 'python-nav-backward-block)
        (define-key map [remap forward-sentence] 'python-nav-forward-block)
        (define-key map [remap backward-up-list] 'python-nav-backward-up-list)
        (define-key map [remap mark-defun] 'python-mark-defun)
        (define-key map "\C-c\C-j" 'imenu)
        ;; Indent specific
        (define-key map "\177" 'python-indent-dedent-line-backspace)
        (define-key map (kbd "<backtab>") 'python-indent-dedent-line)
        (define-key map "\C-c<" 'python-indent-shift-left)
        (define-key map "\C-c>" 'python-indent-shift-right)
        ;; Skeletons
        (define-key map "\C-c\C-tc" 'python-skeleton-class)
        (define-key map "\C-c\C-td" 'python-skeleton-def)
        (define-key map "\C-c\C-tf" 'python-skeleton-for)
        (define-key map "\C-c\C-ti" 'python-skeleton-if)
        (define-key map "\C-c\C-tm" 'python-skeleton-import)
        (define-key map "\C-c\C-tt" 'python-skeleton-try)
        (define-key map "\C-c\C-tw" 'python-skeleton-while)
        ;; Shell interaction
        (define-key map "\C-c\C-p" 'run-python)
        (define-key map "\C-c\C-s" 'python-shell-send-string)
        (define-key map "\C-c\C-e" 'python-shell-send-statement)
        (define-key map "\C-c\C-r" 'python-shell-send-region)
        (define-key map "\C-\M-x" 'python-shell-send-defun)
        (define-key map "\C-c\C-c" 'python-shell-send-buffer)
        (define-key map "\C-c\C-l" 'python-shell-send-file)
        (define-key map "\C-c\C-z" 'python-shell-switch-to-shell)
        ;; Some util commands
        (define-key map "\C-c\C-v" 'python-check)
        (define-key map "\C-c\C-f" 'python-eldoc-at-point)
        (define-key map "\C-c\C-d" 'python-describe-at-point)
        ;; Utilities
        (substitute-key-definition 'complete-symbol 'completion-at-point
                                   map global-map)
        (easy-menu-define python-menu map "Python Mode menu"
          '("Python"
            :help "Python-specific Features"
            ["Shift region left" python-indent-shift-left :active mark-active
             :help "Shift region left by a single indentation step"]
            ["Shift region right" python-indent-shift-right :active mark-active
             :help "Shift region right by a single indentation step"]
            "-"
            ["Start of def/class" beginning-of-defun
             :help "Go to start of outermost definition around point"]
            ["End of def/class" end-of-defun
             :help "Go to end of definition around point"]
            ["Mark def/class" mark-defun
             :help "Mark outermost definition around point"]
            ["Jump to def/class" imenu
             :help "Jump to a class or function definition"]
            "--"
            ("Skeletons")
            "---"
            ["Start interpreter" run-python
             :help "Run inferior Python process in a separate buffer"]
            ["Switch to shell" python-shell-switch-to-shell
             :help "Switch to running inferior Python process"]
            ["Eval string" python-shell-send-string
             :help "Eval string in inferior Python session"]
            ["Eval buffer" python-shell-send-buffer
             :help "Eval buffer in inferior Python session"]
            ["Eval statement" python-shell-send-statement
             :help "Eval statement in inferior Python session"]
            ["Eval region" python-shell-send-region
             :help "Eval region in inferior Python session"]
            ["Eval defun" python-shell-send-defun
             :help "Eval defun in inferior Python session"]
            ["Eval file" python-shell-send-file
             :help "Eval file in inferior Python session"]
            ["Debugger" pdb :help "Run pdb under GUD"]
            "----"
            ["Check file" python-check
             :help "Check file for errors"]
            ["Help on symbol" python-eldoc-at-point
             :help "Get help on symbol at point"]
            ["Complete symbol" completion-at-point
             :help "Complete symbol before point"]))
        map))

(global-set-key (kbd "C-S-k") 'shrink-window)
(global-set-key (kbd "s-y") 'yas-expand)
(global-set-key (kbd "<C-S-up>") 'shrink-window)
(global-set-key (kbd "C-S-j") 'enlarge-window)
(global-set-key (kbd "<C-S-down>") 'enlarge-window)
(global-set-key (kbd "C-S-l") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-h") 'shrink-window-horizontally)
(global-set-key (kbd "C-c l") 'smerge-keep-lower)
(global-set-key (kbd "C-c u") 'smerge-keep-upper)
(global-set-key (kbd "C-c a") 'smerge-keep-all)
(global-set-key (kbd "C-c j") 'smerge-next)
(global-set-key (kbd "C-c k") 'smerge-prev)

(global-set-key (kbd "s-e") 'emmet-expand-line)
(global-set-key (kbd "C-s") 'save-buffer)
(define-key evil-normal-state-map (kbd "SPC w w") 'ace-window)


(global-set-key (kbd "C-c C-x") 'run-python)



;;:------------------------
;;; Vterm
;;:------------------------

(use-package! vterm-toggle
  :defer t
  :bind (:map evil-normal-state-map
              ("SPC t ]" . vterm-toggle-forward)
              ("SPC t [" . vterm-toggle-backward)
              ("SPC t n" . (lambda () (interactive)
                             (let ((current-buffer-name (buffer-name)))
                               (vterm-toggle--new)
                               (delete-window)
                               (display-buffer current-buffer-name)
                               (vterm-toggle-forward))))
              ("SPC t x" . (lambda (args) (interactive "P")
                             (when (string-match "vterm" (buffer-name))
                               (let ((kill-buffer-query-functions nil))
                                 (kill-this-buffer)
                                 (+vterm/toggle args)))))
              ("SPC o h" . (lambda () (interactive)
                             (+vterm/toggle t)))
              ("SPC t h" . vterm-toggle-hide)
              ("SPC t k" . my-open-kitty-right-here))
  :config
  (setq vterm-kill-buffer-on-exit nil)
  (setq vterm-toggle-scope 'project))



;;:------------------------
;;; ORG
;;:------------------------

;; Add additional space before link insert
(defun my-add-additional-space-when-not-exist (_)
  "Add additional sapce if previous char is not space!"
  (unless (eq (char-before) ? )
    (insert " ")))

(advice-add 'org-insert-link :before 'my-add-additional-space-when-not-exist)

;; Format org mode block

(defun format-org-mode-block ()
  "Format org mode code block"
  (interactive "p")
  (org-edit-special)
  (format-all-ensure-formatter)
  (format-all-buffer)
  (org-edit-src-exit))

;; Crypt

(use-package! prg-crypt
  :defer t)

;; Org package

(use-package! org
  :mode (("\\.org$" . org-mode))
  :defer t
  ;; :demand t
  :bind (:map evil-normal-state-map
              ("SPC h ]" . org-next-visible-heading)
              ("SPC h [" . org-previous-visible-heading))
  :config
  (progn
;;    (define-key org-mode-map "\C-x a f" "\C-x h \C-M-\\ \C-c")
    (define-key org-mode-map (kbd "C-x a f") (kbd "C-x h C-M-\\ C-c"))
    (custom-set-faces
     '(org-document-title ((t (:inherit outline-1 :height 2.5))))
     '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
     '(org-level-2 ((t (:inherit outline-2 :height 1.25))))
     '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
     '(org-level-4 ((t (:inherit outline-4 :height 0.95))))
     '(org-level-5 ((t (:inherit outline-5 :height 0.7)))))

    (setq org-hide-emphasis-markers t)

    (add-to-list 'org-tag-faces '("@.*" . (:foreground "red")))

    ;; Increase priorities count
    (setq org-highest-priority ?A
          org-default-priority ?C
          org-lowest-priority ?E)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((typescript . t)
       (js . t)
       (restclient . t)))

    (defun org-babel-execute:typescript (body params)
      (let ((org-babel-js-cmd "npx ts-node < "))
        (org-babel-execute:js body params)))

    (defvar org-babel-js-function-wrapper
      ""
      "Javascript code to print value of body.")
;; Applications for opening from org files
(if (assoc "\\.pdf\\'" org-file-apps)
         (setcdr (assoc "\\.pdf\\'" org-file-apps) 'emacs)
       (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs) t))))


;; Pretty tags
;; Replace all tags inside org document to pretty svg buttons

(use-package! svg-tag-mode
  :defer t
  :hook (org-mode . svg-tag-mode)
  :config
  (setq svg-tag-tags
        '(("\\(:[A-Z]+:\\)" . ((lambda (tag)
                                 (svg-tag-make tag :beg 1 :end -1)))))))

;; Org todo keywords

(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"     ; A task that needs doing & is ready to do
           "PROJ(p)"     ; A project, which usually contains other tasks
           "IDEA(i)"     ; Idea
           "PROGRESS(s)" ; A task that is in progress
           "WAIT(w)"     ; Something external is holding up this task
           "TEST(c)"     ; In TEST statement
           "BLOCK(b)"    ; task blocked
           "REJECTED(x)" ; somebody rejected idea :(
           "FEEDBACK(f)" ; Feedback required
           "REVIEW(r)"   ; Somebody reviewed your feature
           "HOLD(h)"     ; This task is paused/on hold because of me
           "|"
           "DONE(d)"     ; Task successfully completed
           "KILL(k)")    ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"      ; A task that needs doing
           "[-](S)"      ; Task is in progress
           "[?](W)"      ; Task is being held up or paused
           "|"
           "[X](D)"))    ; Task was completed
        org-todo-keyword-faces
        '(("[-]"        . +org-todo-active)
          ("PROGRESS"   . org-todo)
          ("DONE"       . org-todo)
          ("IDEA"       . org-todo)
          ("[?]"        . +org-todo-onhold)
          ("WAIT"       . +org-todo-onhold)
          ("TEST"       . +org-todo-active)
          ("FEEDBACK"   . +org-todo-onhold)
          ("REVIEW"     . +org-todo-onhold)
          ("HOLD"       . +org-todo-onhold)
          ("PROJ"       . +org-todo-project)
          ("BLOCK"       . +org-todo-cancel)
          ("REJECTED"       . +org-todo-cancel)
          ("KILL"       . +org-todo-cancel))))

;; Ligatures for org mode

(add-hook 'org-mode-hook (lambda ()
                           "Beautify Org Checkbox Symbol"
                           (push '("[ ]" .  "☐") prettify-symbols-alist)
                           (push '("[X]" . "☑" ) prettify-symbols-alist)
                           (push '("[-]" . "❍" ) prettify-symbols-alist)
                           (push '("#+BEGIN_SRC" . "↦" ) prettify-symbols-alist)
                           (push '("#+END_SRC" . "⇤" ) prettify-symbols-alist)
                           (push '("#+BEGIN_EXAMPLE" . "↦" ) prettify-symbols-alist)
                           (push '("#+END_EXAMPLE" . "⇤" ) prettify-symbols-alist)
                           (push '("#+BEGIN_QUOTE" . "↦" ) prettify-symbols-alist)
                           (push '("#+END_QUOTE" . "⇤" ) prettify-symbols-alist)
                           (push '("#+begin_quote" . "↦" ) prettify-symbols-alist)
                           (push '("#+end_quote" . "⇤" ) prettify-symbols-alist)
                           (push '("#+begin_example" . "↦" ) prettify-symbols-alist)
                           (push '("#+end_example" . "⇤" ) prettify-symbols-alist)
                           (push '("#+begin_src" . "↦" ) prettify-symbols-alist)
                           (push '("#+end_src" . "⇤" ) prettify-symbols-alist)
                           (push '("#+TITLE:" . "") prettify-symbols-alist)
                           (push '("#+DESCRIPTION:" . "") prettify-symbols-alist)
                           (push '("#+ID:" . "") prettify-symbols-alist)
                           (push '("#+FILETAGS:" . "") prettify-symbols-alist)
                           (push '("#+STARTUP:" . "") prettify-symbols-alist)
                           (push '("#+ACTIVE:" . "") prettify-symbols-alist)
                           (push '("#+START_SPOILER" . "") prettify-symbols-alist)
                           (push '("#+CLOSE_SPOILER" . "") prettify-symbols-alist)
                           (push '("#+BEGIN_HIDDEN" . "") prettify-symbols-alist)
                           (push '("#+END_HIDDEN" . "") prettify-symbols-alist)
                           (prettify-symbols-mode)))

;; HOLD Org calendar
;; Sync calendar and agenda with you google account

(use-package! org-caldav
  :defer t
  :config
  (require 'oauth2)
  (setq org-caldav-oauth2-client-secret +m-google-calendar-client-secret)
  (setq org-caldav-oauth2-client-id +m-google-calendar-client-id)
  (setq org-icalendar-timezone "Equrope/Stocholm")
  (setq plstore-cache-passphrase-for-symmetric-encryption t)
  (setq org-caldav-url 'google))



;; Prettify org priority

(use-package! org-fancy-priorities
  :after org
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '((?A . "🔥")
                                    (?B . "⬆")
                                    (?C . "❗")
                                    (?D . "⬇")
                                    (?E . "❓")
                                    (?1 . "🔥")
                                    (?2 . "⚡")
                                    (?3 . "⮮")
                                    (?4 . "☕")
                                    (?I . "Important"))))

;; Org indent

(use-package! org-indent
  :defer 8
  :init
  (add-hook 'org-mode-hook 'org-indent-mode))

;; Pretty org stars

(use-package! org-superstar
  :defer 5
  :hook (org-mode . org-superstar-mode))



;; Org roam
;; One of the best Zettelkasten implementation

(use-package! org-roam
  :after org
  :bind (:map evil-normal-state-map
               ("SPC n r i" . org-roam-node-insert))
  :init
  (setq org-roam-v2-ack t)
  :config
  ;; (org-roam-db-autosync-enable)
  (cl-defmethod org-roam-node-mtitle ((node org-roam-node))
    "Return customized title of roam node"
    (let* ((tags (org-roam-node-tags node))
           (title (org-roam-node-title node)))
      (if (not tags)
          title
        (setq joined-text (string-join tags ", "))
        (concat (propertize (format "(%s) " joined-text) 'face `(:foreground ,+m-color-main :weight bold :slant italic)) title))))
  ;; (setq org-roam-completion-system 'ivy)
  (setq org-roam-completion-system 'vertico)
  (setq org-roam-node-display-template "${mtitle:100}")
  (setq org-roam-directory (file-truename "~/org-roam")))

(use-package! websocket
  :after org-roam)




;; Visual roam ui nodes

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t
        org-roam-ui-browser-function #'xwidget-webkit-browse-url))



;; Image inserting to org documents

(use-package! org-yt
  :defer 20
  :config
  (defun org-image-link (protocol link _description)
    "Interpret LINK as base64-encoded image data."
    (cl-assert (string-match "\\`img" protocol) nil
               "Expected protocol type starting with img")
    (let ((buf (url-retrieve-synchronously (concat (substring protocol 3) ":" link))))
      (cl-assert buf nil
                 "Download of image \"%s\" failed." link)
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "\r?\n\r?\n")
        (buffer-substring-no-properties (point) (point-max)))))

  (org-link-set-parameters
   "imghttp"
   :image-data-fun #'org-image-link)

  (org-link-set-parameters
   "imghttps"
   :image-data-fun #'org-image-link))



;; Roam publisher
;; My own package for publish roam files

(use-package! web-roam
  :defer t
  :bind (:map evil-normal-state-map
              ("SPC n p" . web-roam-publish-file)))

;; Pretty agenda
(use-package! pretty-agenda
  :load-path "~/.config/doom/"
  :defer 15)

