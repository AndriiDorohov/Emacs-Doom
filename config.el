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

;; (setq default-directory "/Users/apogee/repositories/")


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

;; This block defines `+emoji-rx' and `+emoji-set-font'.

(setq doom-font (font-spec :family "JetBrains Mono" :size 16)
      doom-big-font (font-spec :family "JetBrains Mono" :size 26)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 16)
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-emoji-font (font-spec :family "Twitter Color Emoji")      
      doom-serif-font (font-spec :family "IBM Plex Mono" :size 12 :weight 'light))
(add-hook! 'after-setting-font-hook
  (defun +emoji-set-font ()
    (set-fontset-font t 'emoji doom-emoji-font nil 'prepend)))

(defvar +emoji-rx
  (let (emojis)
    (map-char-table
     (lambda (char set)
       (when (eq set 'emoji)
         (push (copy-tree char) emojis)))
     char-script-table)
    (rx-to-string `(any ,@emojis)))
  "A regexp to find all emoji-script characters.")
(setq emoji-alternate-names
      '(("🙂" ":)")
        ("😄" ":D")
        ("😉" ";)")
        ("🙁" ":(")
        ("😆" "laughing face" "xD")
        ("🤣" "ROFL face")
        ("😢" ":'(")
        ("🥲" ":')")
        ("😮" ":o")
        ("😑" ":|")
        ("😎" "cool face")
        ("🤪" "goofy face")
        ("🤥" "pinnochio face" "liar face")
        ("😠" ">:(")
        ("😡" "angry+ face")
        ("🤬" "swearing face")
        ("🤢" "sick face")
        ("😈" "smiling imp")
        ("👿" "frowning imp")
        ("❤️" "<3")
        ("🫡" "o7")
        ("👍" "+1")
        ("👎" "-1")
        ("👈" "left")
        ("👉" "right")
        ("👆" "up")
        ("💯" "100")
        ("💸" "flying money")))
(when (>= emacs-major-version 29)
  (map! :leader
        (:prefix ("e" . "Emoji")
         :desc "Search" "s" #'emoji-search
         :desc "Recent" "r" #'emoji-recent
         :desc "List" "l" #'emoji-list
         :desc "Describe" "d" #'emoji-describe
         :desc "Insert" "i" #'emoji-insert
         :desc "Insert" "e" #'emoji-insert)))

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
;;; Dashboard
;;:------------------------

;; This block defines `+doom-dashboard-tweak',
;; `+doom-dashboard-benchmark-line',
;; `+doom-dashboard-setup-modified-keymap', and
;; `doom-dashboard-draw-ascii-emacs-banner-fn'.



(defun doom-dashboard-draw-ascii-emacs-banner-fn ()
  (let* ((banner
          '(",---.,-.-.,---.,---.,---."
            "|---'| | |,---||    `---."
            "`---'` ' '`---^`---'`---'"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))
(unless (display-graphic-p) ; for some reason this messes up the graphical splash screen atm
  (setq +doom-dashboard-ascii-banner-fn #'doom-dashboard-draw-ascii-emacs-banner-fn))
(defun +doom-dashboard-setup-modified-keymap ()
  (setq +doom-dashboard-mode-map (make-sparse-keymap))
  (map! :map +doom-dashboard-mode-map
        :desc "Find file" :ng "f" #'find-file
        :desc "Recent files" :ng "r" #'consult-recent-file
        :desc "Config dir" :ng "C" #'doom/open-private-config
        :desc "Open config.org" :ng "c" (cmd! (find-file (expand-file-name "config.org" doom-user-dir)))
        :desc "Open org-mode root" :ng "O" (cmd! (find-file (expand-file-name "lisp/org/" doom-user-dir)))
        :desc "Open dotfile" :ng "." (cmd! (doom-project-find-file "~/.config/"))
        :desc "Notes (roam)" :ng "n" #'org-roam-node-find
        :desc "Switch buffer" :ng "b" #'+vertico/switch-workspace-buffer
        :desc "Switch buffers (all)" :ng "B" #'consult-buffer
        :desc "IBuffer" :ng "i" #'ibuffer
        :desc "Previous buffer" :ng "p" #'previous-buffer
        :desc "Set theme" :ng "t" #'consult-theme
        :desc "Quit" :ng "Q" #'save-buffers-kill-terminal
        :desc "Show keybindings" :ng "h" (cmd! (which-key-show-keymap '+doom-dashboard-mode-map))))

(add-transient-hook! #'+doom-dashboard-mode (+doom-dashboard-setup-modified-keymap))
(add-transient-hook! #'+doom-dashboard-mode :append (+doom-dashboard-setup-modified-keymap))
(add-hook! 'doom-init-ui-hook :append (+doom-dashboard-setup-modified-keymap))
(map! :leader :desc "Dashboard" "d" #'+doom-dashboard/open)
(defun +doom-dashboard-benchmark-line ()
  "Insert the load time line."
  (when doom-init-time
    (insert
     "\n\n"
     (propertize
      (+doom-dashboard--center
       +doom-dashboard--width
       (doom-display-benchmark-h 'return))
      'face 'doom-dashboard-loaded))))
(remove-hook 'doom-after-init-hook #'doom-display-benchmark-h)
(setq +doom-dashboard-functions
      (list #'doom-dashboard-widget-banner
            #'+doom-dashboard-benchmark-line))
(defun +doom-dashboard-tweak (&optional _)
  (when-let ((dashboard-buffer (get-buffer +doom-dashboard-name)))
    (with-current-buffer dashboard-buffer
      (setq-local line-spacing 0.2
                  mode-line-format nil
                  evil-normal-state-cursor (list nil)))))
(add-hook '+doom-dashboard-mode-hook #'+doom-dashboard-tweak)
(add-hook 'doom-after-init-hook #'+doom-dashboard-tweak 1)
(setq +doom-dashboard-name "► Doom"
      doom-fallback-buffer-name +doom-dashboard-name)
;;; config-dashboard.el ends here



;;:------------------------
;;; Better jumper mouse
;;:------------------------

(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)

;;:------------------------
;;; Frame title
;;:------------------------

(setq frame-title-format
      '(""
        (:eval
         (if (string-match-p (regexp-quote (or (bound-and-true-p org-roam-directory) "\u0000"))
                             (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?\s buffer-file-name))
           "%b"))
        (:eval
         (when-let ((project-name (and (featurep 'projectile) (projectile-project-name))))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))


;;:------------------------
;;; Emacs daemon setup
;;:------------------------

;; This block defines `greedily-do-daemon-setup'.

(defun greedily-do-daemon-setup ()
  (require 'org)
  (when (require 'mu4e nil t)
    (setq mu4e-confirm-quit t)
    (setq +mu4e-lock-greedy t)
    (setq +mu4e-lock-relaxed t)
    (when (+mu4e-lock-available t)
      (mu4e--start)))
  (when (require 'elfeed nil t)
    (run-at-time nil (* 8 60 60) #'elfeed-update)))

(when (daemonp)
  (add-hook 'emacs-startup-hook #'greedily-do-daemon-setup)
  (add-hook! 'server-after-make-frame-hook
    (unless (string-match-p "\\*draft\\|\\*stdin\\|emacs-everywhere" (buffer-name))
      (switch-to-buffer +doom-dashboard-name))))


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
;;; !Pkg which-key
;;:------------------------

(setq which-key-idle-delay 0.5) ;; I need the help, I really do
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

;;:------------------------
;;; Multi-mode abbrev
;;:------------------------

;; This block defines `+abbrev-file-name'.

(add-hook 'doom-first-buffer-hook
          (defun +abbrev-file-name ()
            (setq-default abbrev-mode t)
            (setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))))


;;:------------------------
;;; !Pkg VLF
;;:------------------------

;; This block defines `+vlf-isearch-wrap', `+vlf-last-chunk-or-end',
;; `+vlf-next-chunk-or-start', and `+vlf-update-linum'.

(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write
  vlf-search vlf-occur vlf-follow vlf-ediff vlf
  :commands vlf vlf-mode
  :init
  (defadvice! +files--ask-about-large-file-vlf (size op-type filename offer-raw)
  "Like `files--ask-user-about-large-file', but with support for `vlf'."
  :override #'files--ask-user-about-large-file
  (if (eq vlf-application 'dont-ask)
      (progn (vlf filename) (error ""))
    (let ((prompt (format "File %s is large (%s), really %s?"
                          (file-name-nondirectory filename)
                          (funcall byte-count-to-string-function size) op-type)))
      (if (not offer-raw)
          (if (y-or-n-p prompt) nil 'abort)
        (let ((choice
               (car
                (read-multiple-choice
                 prompt '((?y "yes")
                          (?n "no")
                          (?l "literally")
                          (?v "vlf"))
                 (files--ask-user-about-large-file-help-text
                  op-type (funcall byte-count-to-string-function size))))))
          (cond ((eq choice ?y) nil)
                ((eq choice ?l) 'raw)
                ((eq choice ?v)
                 (vlf filename)
                 (error ""))
                (t 'abort)))))))
  :config
  (advice-remove 'abort-if-file-too-large #'ad-Advice-abort-if-file-too-large)
  (defvar-local +vlf-cumulative-linenum '((0 . 0))
  "An alist keeping track of the cumulative line number.")

(defun +vlf-update-linum ()
  "Update the line number offset."
  (let ((linenum-offset (alist-get vlf-start-pos +vlf-cumulative-linenum)))
    (setq display-line-numbers-offset (or linenum-offset 0))
    (when (and linenum-offset (not (assq vlf-end-pos +vlf-cumulative-linenum)))
      (push (cons vlf-end-pos (+ linenum-offset
                                 (count-lines (point-min) (point-max))))
            +vlf-cumulative-linenum))))

(add-hook 'vlf-after-chunk-update-hook #'+vlf-update-linum)

;; Since this only works with absolute line numbers, let's make sure we use them.
(add-hook! 'vlf-mode-hook (setq-local display-line-numbers t))
  (defun +vlf-next-chunk-or-start ()
  (if (= vlf-file-size vlf-end-pos)
      (vlf-jump-to-chunk 1)
    (vlf-next-batch 1))
  (goto-char (point-min)))

(defun +vlf-last-chunk-or-end ()
  (if (= 0 vlf-start-pos)
      (vlf-end-of-file)
    (vlf-prev-batch 1))
  (goto-char (point-max)))

(defun +vlf-isearch-wrap ()
  (if isearch-forward
      (+vlf-next-chunk-or-start)
    (+vlf-last-chunk-or-end)))

(add-hook! 'vlf-mode-hook (setq-local isearch-wrap-function #'+vlf-isearch-wrap)))

;;:------------------------
;;; !Pkg Eros
;;:------------------------

(setq eros-eval-result-prefix "⟹ ") ; default =>

;;:------------------------
;;; !Pkg evil
;;:------------------------

(after! evil
  (setq evil-ex-substitute-global t     ; I like my s/../.. to by global by default
        evil-move-cursor-back nil       ; Don't move the block cursor when toggling insert mode
        evil-kill-on-visual-paste nil)) ; Don't put overwritten text in the kill ring

;;:------------------------
;;; !Pkg Consult
;;:------------------------

(after! consult
  (set-face-attribute 'consult-file nil :inherit 'consult-buffer)
  (setf (plist-get (alist-get 'perl consult-async-split-styles-alist) :initial) ";"))

;;:------------------------
;;; !Pkg Magit
;;:------------------------

;; This block defines `+org-commit-message-template',
;; `+magit-fill-in-commit-template', `+magit-default-forge-remote',
;; and `+magit-project-commit-templates-alist'.

(defvar +magit-project-commit-templates-alist nil
  "Alist of toplevel dirs and template hf strings/functions.")
(after! magit
  (defvar +magit-default-forge-remote "gitea@git.tecosaur.net:tec/%s.git"
  "Format string that fills out to a remote from the repo name.
Set to nil to disable this functionality.")
(defadvice! +magit-remote-add--streamline-forge-a (args)
  :filter-args #'magit-remote-add
  (interactive
   (or (and +magit-default-forge-remote
            (not (magit-list-remotes))
            (eq (read-char-choice
                 (format "Setup %s remote? [y/n]: "
                         (replace-regexp-in-string
                          "\\`\\(?:[^@]+@\\|https://\\)\\([^:/]+\\)[:/].*\\'" "\\1"
                          +magit-default-forge-remote))
                 '(?y ?n))
                ?y)
            (let* ((default-name
                     (subst-char-in-string ?\s ?-
                                           (file-name-nondirectory
                                            (directory-file-name (doom-project-root)))))
                   (name (read-string "Name: " default-name)))
              (list "origin" (format +magit-default-forge-remote name)
                    (transient-args 'magit-remote))))
       (let ((origin (magit-get "remote.origin.url"))
             (remote (magit-read-string-ns "Remote name"))
             (gh-user (magit-get "github.user")))
         (when (and (equal remote gh-user)
                    (string-match "\\`https://github\\.com/\\([^/]+\\)/\\([^/]+\\)\\.git\\'"
                                  origin)
                    (not (string= (match-string 1 origin) gh-user)))
           (setq origin (replace-regexp-in-string
                         "\\`https://github\\.com/" "git@github.com:"
                         origin)))
         (list remote
               (magit-read-url
                "Remote url"
                (and origin
                     (string-match "\\([^:/]+\\)/[^/]+\\(\\.git\\)?\\'" origin)
                     (replace-match remote t t origin 1)))
               (transient-args 'magit-remote)))))
  args)
(defun +magit-fill-in-commit-template ()
  "Insert template from `+magit-fill-in-commit-template' if applicable."
  (when-let ((template (and (save-excursion (goto-char (point-min)) (string-match-p "\\`\\s-*$" (thing-at-point 'line)))
                            (cdr (assoc (file-name-base (directory-file-name (magit-toplevel)))
                                        +magit-project-commit-templates-alist)))))
    (goto-char (point-min))
    (insert (if (stringp template) template (funcall template)))
    (goto-char (point-min))
    (end-of-line)))
(add-hook 'git-commit-setup-hook #'+magit-fill-in-commit-template 90)
(defun +org-commit-message-template ()
  "Create a skeleton for an Org commit message based on the staged diff."
  (let (change-data last-file file-changes temp-point)
    (with-temp-buffer
      (apply #'call-process magit-git-executable
             nil t nil
             (append
              magit-git-global-arguments
              (list "diff" "--cached")))
      (goto-char (point-min))
      (while (re-search-forward "^@@\\|^\\+\\+\\+ b/" nil t)
        (if (looking-back "^\\+\\+\\+ b/" (line-beginning-position))
            (progn
              (push (list last-file file-changes) change-data)
              (setq last-file (buffer-substring-no-properties (point) (line-end-position))
                    file-changes nil))
          (setq temp-point (line-beginning-position))
          (re-search-forward "^\\+\\|^-" nil t)
          (end-of-line)
          (cond
           ((string-match-p "\\.el$" last-file)
            (when (re-search-backward "^\\(?:[+-]? *\\|@@[ +-\\d,]+@@ \\)(\\(?:cl-\\)?\\(?:defun\\|defvar\\|defmacro\\|defcustom\\)" temp-point t)
              (re-search-forward "\\(?:cl-\\)?\\(?:defun\\|defvar\\|defmacro\\|defcustom\\) " nil t)
              (add-to-list 'file-changes (buffer-substring-no-properties (point) (forward-symbol 1)))))
           ((string-match-p "\\.org$" last-file)
            (when (re-search-backward "^[+-]\\*+ \\|^@@[ +-\\d,]+@@ \\*+ " temp-point t)
              (re-search-forward "@@ \\*+ " nil t)
              (add-to-list 'file-changes (buffer-substring-no-properties (point) (line-end-position)))))))))
    (push (list last-file file-changes) change-data)
    (setq change-data (delete '(nil nil) change-data))
    (concat
     (if (= 1 (length change-data))
         (replace-regexp-in-string "^.*/\\|.[a-z]+$" "" (caar change-data))
       "?")
     ": \n\n"
     (mapconcat
      (lambda (file-changes)
        (if (cadr file-changes)
            (format "* %s (%s): "
                    (car file-changes)
                    (mapconcat #'identity (cadr file-changes) ", "))
          (format "* %s: " (car file-changes))))
      change-data
      "\n\n"))))

(add-to-list '+magit-project-commit-templates-alist (cons "org" #'+org-commit-message-template)))


;;:------------------------
;;; !Pkg Smerge
;;:------------------------

;; This block defines `smerge-repeatedly'.

(defun smerge-repeatedly ()
  "Perform smerge actions again and again"
  (interactive)
  (smerge-mode 1)
  (smerge-transient))
(after! transient
  (transient-define-prefix smerge-transient ()
    [["Move"
      ("n" "next" (lambda () (interactive) (ignore-errors (smerge-next)) (smerge-repeatedly)))
      ("p" "previous" (lambda () (interactive) (ignore-errors (smerge-prev)) (smerge-repeatedly)))]
     ["Keep"
      ("b" "base" (lambda () (interactive) (ignore-errors (smerge-keep-base)) (smerge-repeatedly)))
      ("u" "upper" (lambda () (interactive) (ignore-errors (smerge-keep-upper)) (smerge-repeatedly)))
      ("l" "lower" (lambda () (interactive) (ignore-errors (smerge-keep-lower)) (smerge-repeatedly)))
      ("a" "all" (lambda () (interactive) (ignore-errors (smerge-keep-all)) (smerge-repeatedly)))
      ("RET" "current" (lambda () (interactive) (ignore-errors (smerge-keep-current)) (smerge-repeatedly)))]
     ["Diff"
      ("<" "upper/base" (lambda () (interactive) (ignore-errors (smerge-diff-base-upper)) (smerge-repeatedly)))
      ("=" "upper/lower" (lambda () (interactive) (ignore-errors (smerge-diff-upper-lower)) (smerge-repeatedly)))
      (">" "base/lower" (lambda () (interactive) (ignore-errors (smerge-diff-base-lower)) (smerge-repeatedly)))
      ("R" "refine" (lambda () (interactive) (ignore-errors (smerge-refine)) (smerge-repeatedly)))
      ("E" "ediff" (lambda () (interactive) (ignore-errors (smerge-ediff)) (smerge-repeatedly)))]
     ["Other"
      ("c" "combine" (lambda () (interactive) (ignore-errors (smerge-combine-with-next)) (smerge-repeatedly)))
      ("r" "resolve" (lambda () (interactive) (ignore-errors (smerge-resolve)) (smerge-repeatedly)))
      ("k" "kill current" (lambda () (interactive) (ignore-errors (smerge-kill-current)) (smerge-repeatedly)))
      ("q" "quit" (lambda () (interactive) (smerge-auto-leave)))]]))


;;:------------------------
;;; !Pkg Company
;;:------------------------

;;:------------------------
;;; !Pkg Projectile
;;:------------------------

;; This block defines `projectile-ignored-project-function'.

(setq projectile-ignored-projects
      (list "~/" "/tmp" (expand-file-name "straight/repos" doom-local-dir)))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

;;:------------------------
;;; !Pkg Ispell
;;:------------------------

(setq ispell-dictionary "en-custom")
(setq ispell-personal-dictionary
      (expand-file-name "misc/ispell_personal" doom-private-dir))


;;:------------------------
;;; TRAMP
;;:------------------------

(after! tramp
  (setenv "SHELL" "/bin/bash")
  (setq tramp-shell-prompt-pattern "\\(?:^\\|\n\\|\x0d\\)[^]#$%>\n]*#?[]#$%>] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*")) ;; default + 
(after! tramp
  (appendq! tramp-remote-path
            '("~/.guix-profile/bin" "~/.guix-profile/sbin"
              "/run/current-system/profile/bin"
              "/run/current-system/profile/sbin")))


;;:------------------------
;;; !Pkg AAS
;;:------------------------

(use-package! aas
  :commands aas-mode)



;;:------------------------
;;; !Pkg Screenshot
;;:------------------------

(use-package! screenshot
  :defer t
  :config (setq screenshot-upload-fn "upload %s 2>/dev/null"))


;;:------------------------
;;; !Pkg etrace
;;:------------------------

(use-package! etrace
  :after elp)


;;:------------------------
;;; !Pkg YASnippet
;;:------------------------

(setq yas-triggers-in-field t)


;;:------------------------
;;; !Pkg String Inflection
;;:------------------------

(use-package! string-inflection
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-kebab-case
             string-inflection-underscore
             string-inflection-capital-underscore
             string-inflection-upcase)
  :init
  (map! :leader :prefix ("c~" . "naming convention")
        :desc "cycle" "~" #'string-inflection-all-cycle
        :desc "toggle" "t" #'string-inflection-toggle
        :desc "CamelCase" "c" #'string-inflection-camelcase
        :desc "downCase" "d" #'string-inflection-lower-camelcase
        :desc "kebab-case" "k" #'string-inflection-kebab-case
        :desc "under_score" "_" #'string-inflection-underscore
        :desc "Upper_Score" "u" #'string-inflection-capital-underscore
        :desc "UP_CASE" "U" #'string-inflection-upcase)
  (after! evil
    (evil-define-operator evil-operator-string-inflection (beg end _type)
      "Define a new evil operator that cycles symbol casing."
      :move-point nil
      (interactive "<R>")
      (string-inflection-all-cycle)
      (setq evil-repeat-info '([?g ?~])))
    (define-key evil-normal-state-map (kbd "g~") 'evil-operator-string-inflection)))

;;:------------------------
;;; !Pkg Info colors
;;:------------------------

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)


;;:------------------------
;;; !Pkg Theme magic
;;:------------------------

(use-package! theme-magic
  :commands theme-magic-from-emacs
  :config
  (defadvice! theme-magic--auto-extract-16-doom-colors ()
    :override #'theme-magic--auto-extract-16-colors
    (list
     (face-attribute 'default :background)
     (doom-color 'error)
     (doom-color 'success)
     (doom-color 'type)
     (doom-color 'keywords)
     (doom-color 'constants)
     (doom-color 'functions)
     (face-attribute 'default :foreground)
     (face-attribute 'shadow :foreground)
     (doom-blend 'base8 'error 0.1)
     (doom-blend 'base8 'success 0.1)
     (doom-blend 'base8 'type 0.1)
     (doom-blend 'base8 'keywords 0.1)
     (doom-blend 'base8 'constants 0.1)
     (doom-blend 'base8 'functions 0.1)
     (face-attribute 'default :foreground))))


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
;;; !Pkg Keycast
;;:------------------------

(use-package! keycast
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" mode-line-keycast " ") global-mode-string))))
  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
                      :height 0.9)
    '(keycast-key :inherit custom-modified
                  :height 1.1
                  :weight bold)))


;;:------------------------
;;; !Pkg Screencast
;;:------------------------

;; This block defines `gif-screencast-write-colormap'.

(use-package! gif-screencast
  :commands gif-screencast-mode
  :config
  (map! :map gif-screencast-mode-map
        :g "<f8>" #'gif-screencast-toggle-pause
        :g "<f9>" #'gif-screencast-stop)
  (setq gif-screencast-program "maim"
        gif-screencast-args `("--quality" "3" "-i" ,(string-trim-right
                                                     (shell-command-to-string
                                                      "xdotool getactivewindow")))
        gif-screencast-optimize-args '("--batch" "--optimize=3" "--usecolormap=/tmp/doom-color-theme"))
  (defun gif-screencast-write-colormap ()
    (f-write-text
     (replace-regexp-in-string
      "\n+" "\n"
      (mapconcat (lambda (c) (if (listp (cdr c))
                                 (cadr c))) doom-themes--colors "\n"))
     'utf-8
     "/tmp/doom-color-theme" ))
  (gif-screencast-write-colormap)
  (add-hook 'doom-load-theme-hook #'gif-screencast-write-colormap))


;;:------------------------
;;; !Pkg mixed pitch
;;:------------------------

;; This block defines `mixed-pitch-serif-mode', `init-mixed-pitch-h',
;; and `mixed-pitch-modes'.

(defvar mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
  "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")
(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
  (when (memq major-mode mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))
(add-hook 'doom-init-ui-hook #'init-mixed-pitch-h)
(autoload #'mixed-pitch-serif-mode "mixed-pitch"
  "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch." t)

(setq! variable-pitch-serif-font (font-spec :family "Alegreya" :size 27))

(after! mixed-pitch
  (setq mixed-pitch-set-height t)
  (set-face-attribute 'variable-pitch-serif nil :font variable-pitch-serif-font)
  (defun mixed-pitch-serif-mode (&optional arg)
    "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch."
    (interactive)
    (let ((mixed-pitch-face 'variable-pitch-serif))
      (mixed-pitch-mode (or arg 'toggle)))))
(set-char-table-range composition-function-table ?f '(["\\(?:ff?[fijlt]\\)" 0 font-shape-gstring]))
(set-char-table-range composition-function-table ?T '(["\\(?:Th\\)" 0 font-shape-gstring]))


;;:------------------------
;;; Variable pitch serif font
;;:------------------------

;; This block defines `variable-pitch-serif-font' and
;; `variable-pitch-serif'.

(defface variable-pitch-serif
    '((t (:family "serif")))
    "A variable-pitch face with serifs."
    :group 'basic-faces)
(defcustom variable-pitch-serif-font (font-spec :family "serif")
  "The font face used for `variable-pitch-serif'."
  :group 'basic-faces
  :set (lambda (symbol value)
         (set-face-attribute 'variable-pitch-serif nil :font value)
         (set-default-toplevel-value symbol value)))


;;:------------------------
;;; !Pkg Marginalia
;;:------------------------

;; This block defines `+marginalia-file-size-colorful' and
;; `+marginalia--time-colorful'.

(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))


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
;;; !Pkg Nerd Icons
;;:------------------------

;;(after! nerd-icons
;;  (setcdr (assoc "m" nerd-icons-extension-icon-alist)
;;          (cdr (assoc "matlab" nerd-icons-extension-icon-alist))))


;;:------------------------
;;; !Pkg page break lines
;;:------------------------

(use-package! page-break-lines
  :commands page-break-lines-mode
  :init
  (autoload 'turn-on-page-break-lines-mode "page-break-lines")
  :config
  (setq page-break-lines-max-width fill-column)
  (map! :prefix "g"
        :desc "Prev page break" :nv "[" #'backward-page
        :desc "Next page break" :nv "]" #'forward-page))


;;:------------------------
;;; Writeroom
;;:------------------------

;; This block defines `+zen-nonprose-org-h', `+zen-prose-org-h',
;; `+zen-enable-mixed-pitch-mode-h', `+zen-org-starhide', and
;; `+zen-serif-p'.

(setq +zen-text-scale 0.8)
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


;;:------------------------
;;; Mail
;;:------------------------

;; This block defines `+org-msg-goto-body-when-replying',
;; `+org-msg-goto-body', `+mu4e-org-ml-signature',
;; `+mu4e-goto-subject-not-to-once', `+mu4e-compose-org-ml-setup',
;; `+browse-url-orgmode-ml', `+mu4e-ml-message-link',
;; `+org-ml-transient-mu4e-action', `+org-ml-select-patch-thread',
;; `+org-ml-current-patches', `+org-ml-apply-patch', `+org-ml--cache',
;; `+org-ml--cache-timestamp', `+org-ml-max-age',
;; `+org-ml-target-dir', `+mu4e-insert-woof-header',
;; `+mu4e-get-woof-header', `+mu4e-evil-enter-insert-mode',
;; `+mu4e-account-sent-folder', `+mu4e-update-personal-addresses',
;; `mu4e-from-name', `mu4e-compose-from-mailto',
;; `+mu4e-header--folder-colors', `mu4e-reindex-maybe',
;; `mu4e-file-reindex-request', `mu4e-reindex-request--add-watcher',
;; `mu4e-reindex-request--last-time',
;; `mu4e-reindex-request--file-just-deleted',
;; `mu4e-reindex-request--file-watcher',
;; `mu4e-reindex-request-min-seperation', and
;; `mu4e-reindex-request-file'.

;; Begin pre
(setq +org-msg-accent-color "#1a5fb4")
;; End pre


;;------------------------MAIL------------------------------------------------

;; Load mu4e from the installation path.
(use-package mu4e
  :load-path  "/usr/local/Cellar/mu/1.10.7/share/emacs/site-lisp/mu/mu4e/"
  :defer 20 ; Wait until 20 seconds after startup
  :config
  ;; For sending mails
  (require 'smtpmail)
  ;;mu4e-general-settings
  ;; we installed this with homebrew
  (setq mu4e-mu-binary (executable-find "mu"))
  ;; this is the directory we created before:
  (setq mu4e-maildir "~/.maildir")
  ;; this command is called to sync imap servers:
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
  ;; how often to call it in seconds:
  (setq mu4e-update-interval 300)
  ;;for mu4e completions (maildir folders, etc)
  (setq mu4e-completing-read-function #'completing-read)
  
  ;; save attachment to desktop by default
  ;; or another choice of yours:
  (setq mu4e-attachment-dir "~/Desktop")
  ;; Make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)

  ;; list of your email adresses:
  (setq mu4e-user-mail-address-list '("wedpositive@gmail.com"
				      "dorokhov.andrii@gmail.com"
				      "andrii.dorokhov@icloud.com"))

;;mu4e-favorites
  ;; check your ~/.maildir to see how the subdirectories are called
  ;; for the generic imap account:
  ;; e.g `ls ~/.maildir/example'
  (setq   mu4e-maildir-shortcuts
          '(("/icloud/INBOX" . ?i)
            ("/icloud/Sent Messages" . ?I)
            ("/gmail/INBOX" . ?g)
            ("/gmail/[Gmail]/Sent Mail" . ?G)
	    ("/gmail/Trash" . ?t)
	    ("/official/INBOX" . ?e)
 	    ("/official/[Gmail]/Sent Mail" . ?O)
	    ("/official/Trash" . ?o)))
  
;;mu4e-context
  (setq mu4e-contexts
        `(,(make-mu4e-context
          :name "icloud"
          :enter-func (lambda () (mu4e-message "Enter andrii.dorokhov@icloud.com context"))
          :leave-func (lambda () (mu4e-message "Leave andrii.dorokhov@icloud.com context"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg
                                                  :to "andrii.dorokhov@icloud.com")))
          :vars '((user-mail-address . "andrii.dorokhov@icloud.com" )
                  (user-full-name . "Andrii Dorokhov")
                  (mu4e-drafts-folder . "/icloud/Drafts")
                  (mu4e-refile-folder . "/icloud/Archive")
                  (mu4e-sent-folder . "/icloud/Sent Messages")
                  (mu4e-trash-folder . "/icloud/Deleted Messages")))
	  
	  ,(make-mu4e-context
          :name "official"
          :enter-func (lambda () (mu4e-message "Enter dorokhov.andrii@gmail.com context"))
          :leave-func (lambda () (mu4e-message "Leave dorokhov.andrii@gmail.com context"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg
                                                  :to "dorokhov.andrii@gmail.com")))
          :vars '((user-mail-address . "dorokhov.andrii@gmail.com")
                  (user-full-name . "Andrii Dorokhov")
                  (mu4e-drafts-folder . "/official/Drafts")
                  (mu4e-refile-folder . "/official/Archive")
                  (mu4e-sent-folder . "/official/Sent")
                  (mu4e-trash-folder . "/official/Trash")))
	  
	  ,(make-mu4e-context
            :name "gmail"
            :enter-func (lambda () (mu4e-message "Enter wedpositive@gmail.com context"))
            :leave-func (lambda () (mu4e-message "Leave wedpositive@gmail.com context"))
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg
                                                                :to "wedpositive@gmail.com")))
            :vars '((user-mail-address . "wedpositive@gmail.com")
                    (user-full-name . "Andrii Dorokhov")
                    (mu4e-drafts-folder . "/gmail/Drafts")
                    (mu4e-refile-folder . "/gmail/Archive")
                    (mu4e-sent-folder . "/gmail/Sent")
                    (mu4e-trash-folder . "/gmail/Trash")
		    (mu4e-sent-messages-behavior . sent)))))

  (setq mu4e-context-policy 'pick-first) ;; start with the first (default) context;
  (setq mu4e-compose-context-policy 'ask) ;; ask for context if no context matches;

   ;; Display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)

  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)

;;mu4e-sending
;; GPG encryption & decryption:

  ;; This can be left alone
  (require 'epa-file)
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback)
  (auth-source-forget-all-cached)

  ;; Don't keep message compose buffers around after sending
  (setq message-kill-buffer-on-exit t)

  (setq dw/mu4e-inbox-query "maildir:/gmail/INBOX AND flag:unread")

  ;; Send function:
  ;;(setq send-mail-function 'sendmail-send-it
  ;;      message-send-mail-function 'sendmail-send-it)
  (setq send-mail-function 'message-send-mail-with-sendmail
        message-send-mail-function 'message-send-mail-with-sendmail)

  ;; Send program:
  ;; This is external. Remember we installed it before.
  (setq sendmail-program (executable-find "msmtp"))

  ;; Select the right sender email from the context.
  (setq message-sendmail-envelope-from 'header)

  ;; Choose from account before sending
  ;; This is a custom function that works for me.
  ;; Well, I stole it somewhere long ago.
  ;; I suggest using it to make matters easy.
  ;; Of course, adjust the email addresses and account descriptions.
  (defun timu/set-msmtp-account ()
    (if (message-mail-p)
        (save-excursion
          (let*
              ((from (save-restriction
                       (message-narrow-to-headers)
                       (message-fetch-field "from")))
               (account
                (cond
                 ((string-match "andrii.dorokhov@icloud.com" from) "icloud")
                 ((string-match "dorokhov.andrii@gmail.com" from) "official")
                 ((string-match "wedpositive@gmail.com" from) "gmail"))))
            (setq message-sendmail-extra-arguments (list '"-a" account))))))

  (add-hook 'message-send-mail-hook 'timu/set-msmtp-account)

  ;; mu4e CC & BCC
  ;; This is custom as well
  (add-hook 'mu4e-compose-mode-hook
            (defun timu/add-cc-and-bcc ()
              "My Function to automatically add Cc & Bcc: headers.
              This is in the mu4e compose mode."
              (save-excursion (message-add-header "Cc:\n"))
              (save-excursion (message-add-header "Bcc:\n"))))

  ;; mu4e address completion
  (add-hook 'mu4e-compose-mode-hook 'company-mode)

  ;;optional
  ;; Store link to message if in header view, not to header query:
  (setq org-mu4e-link-query-in-headers-mode nil)
  ;; Don't have to confirm when quitting:
  (setq mu4e-confirm-quit nil)
  ;; Number of visible headers in horizontal split view:
  (setq mu4e-headers-visible-lines 20)
  ;; Don't show threading by default:
  (setq mu4e-headers-show-threads nil)
  ;; Hide annoying "mu4e Retrieving mail..." message in minibuffer:
  (setq mu4e-hide-index-messages t)
  ;; Customize the reply-quote-string:
  (setq message-citation-line-format "%N @ %Y-%m-%d %H:%M :\n")
  ;; M-x find-function RET message-citation-line-format for docs:
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  ;; By default, do not show related emails:
  (setq mu4e-headers-include-related nil)

;;bookmarks
  (add-to-list 'mu4e-bookmarks
       '(:name "Unread"
             :query "flag:unread and not flag:trashed"
             :key ?f)
       t)
    (defun dw/go-to-inbox ()
      (interactive)
      (mu4e-headers-search dw/mu4e-inbox-query))

  ;; Start mu4e in the background so that it syncs mail periodically
  (mu4e t))


;;MU4E ALERT
(use-package mu4e-alert
  :after mu4e
  :config
  ;; Show unread emails from all inboxes
  (setq mu4e-alert-interesting-mail-query "flag:unread AND maildir:/INBOX")
  ;; Show notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil)
  (mu4e-alert-enable-notifications))


(use-package mu4e-views
  :after mu4e
  :defer nil
  :bind (:map mu4e-headers-mode-map
	    ("v" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
	    ("M-n" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
	    ("M-p" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
        ("f" . mu4e-views-toggle-auto-view-selected-message) ;; toggle opening messages automatically when moving in the headers view
        ("i" . mu4e-views-mu4e-view-as-nonblocked-html) ;; show currently selected email with all remote content
	    )
  :config
  (setq mu4e-views-completion-method 'ivy) ;; use ivy for completion
  (setq mu4e-views-default-view-method "html") ;; make xwidgets default
  (mu4e-views-mu4e-use-view-msg-method "html") ;; select the default
  (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window) ;; when pressing n and p stay in the current window
  (setq mu4e-views-auto-view-selected-message t)) ;; automatically open messages when moving in the headers view

(after! mu4e-views
  (setq email-whitelist '("wedpositive@gmail.com" "dorokhov.andrii@gmail.com" "andrii.dorokhov@icloud.com"))
  (setq mu4e-views-dispatcher-predicate-view-map
        '((,(lambda (msg)
             (-contains-p email-whitelist (mu4e-message-field msg :from)))
           . "html-nonblock")
          (,(lambda (msg) (mu4e-message-field msg :body-html)) . "html")
          (,(lambda (msg) (ignore msg) t) . "text"))))


;;------------------------MAIL------------------------------------------------

(defvar mu4e-reindex-request-file "/tmp/mu_reindex_now"
  "Location of the reindex request, signaled by existance")
(defvar mu4e-reindex-request-min-seperation 5.0
  "Don't refresh again until this many second have elapsed.
Prevents a series of redisplays from being called (when set to an appropriate value)")

(defvar mu4e-reindex-request--file-watcher nil)
(defvar mu4e-reindex-request--file-just-deleted nil)
(defvar mu4e-reindex-request--last-time 0)

(defun mu4e-reindex-request--add-watcher ()
  (setq mu4e-reindex-request--file-just-deleted nil)
  (setq mu4e-reindex-request--file-watcher
        (file-notify-add-watch mu4e-reindex-request-file
                               '(change)
                               #'mu4e-file-reindex-request)))

(defadvice! mu4e-stop-watching-for-reindex-request ()
  :after #'mu4e--server-kill
  (if mu4e-reindex-request--file-watcher
      (file-notify-rm-watch mu4e-reindex-request--file-watcher)))

(defadvice! mu4e-watch-for-reindex-request ()
  :after #'mu4e--server-start
  (mu4e-stop-watching-for-reindex-request)
  (when (file-exists-p mu4e-reindex-request-file)
    (delete-file mu4e-reindex-request-file))
  (mu4e-reindex-request--add-watcher))

(defun mu4e-file-reindex-request (event)
  "Act based on the existance of `mu4e-reindex-request-file'"
  (if mu4e-reindex-request--file-just-deleted
      (mu4e-reindex-request--add-watcher)
    (when (equal (nth 1 event) 'created)
      (delete-file mu4e-reindex-request-file)
      (setq mu4e-reindex-request--file-just-deleted t)
      (mu4e-reindex-maybe t))))

(defun mu4e-reindex-maybe (&optional new-request)
  "Run `mu4e--server-index' if it's been more than
`mu4e-reindex-request-min-seperation'seconds since the last request,"
  (let ((time-since-last-request (- (float-time)
                                    mu4e-reindex-request--last-time)))
    (when new-request
      (setq mu4e-reindex-request--last-time (float-time)))
    (if (> time-since-last-request mu4e-reindex-request-min-seperation)
        (mu4e--server-index nil t)
      (when new-request
        (run-at-time (* 1.1 mu4e-reindex-request-min-seperation) nil
                     #'mu4e-reindex-maybe)))))
(setq mu4e-headers-fields
      '((:flags . 6)
        (:account-stripe . 2)
        (:from-or-to . 25)
        (:folder . 10)
        (:recipnum . 2)
        (:subject . 80)
        (:human-date . 8))
      +mu4e-min-header-frame-width 142
      mu4e-headers-date-format "%d/%m/%y"
      mu4e-headers-time-format "⧖ %H:%M"
      mu4e-headers-results-limit 1000
      mu4e-index-cleanup t)


(defvar +mu4e-header--folder-colors nil)
(setq sendmail-program "/usr/bin/msmtp"
      send-mail-function #'smtpmail-send-it
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from"); , "--read-recipients")
      message-send-mail-function #'message-send-mail-with-sendmail)
(defun mu4e-compose-from-mailto (mailto-string &optional quit-frame-after)
  (require 'mu4e)
  (unless mu4e--server-props (mu4e t) (sleep-for 0.1))
  (let* ((mailto (message-parse-mailto-url mailto-string))
         (to (cadr (assoc "to" mailto)))
         (subject (or (cadr (assoc "subject" mailto)) ""))
         (body (cadr (assoc "body" mailto)))
         (headers (-filter (lambda (spec) (not (-contains-p '("to" "subject" "body") (car spec)))) mailto)))
    (when-let ((mu4e-main (get-buffer mu4e-main-buffer-name)))
      (switch-to-buffer mu4e-main))
    (mu4e~compose-mail to subject headers)
    (when body
      (goto-char (point-min))
      (if (eq major-mode 'org-msg-edit-mode)
          (org-msg-goto-body)
        (mu4e-compose-goto-bottom))
      (insert body))
    (goto-char (point-min))
    (cond ((null to) (search-forward "To: "))
          ((string= "" subject) (search-forward "Subject: "))
          (t (if (eq major-mode 'org-msg-edit-mode)
                 (org-msg-goto-body)
               (mu4e-compose-goto-bottom))))
    (font-lock-ensure)
    (when evil-normal-state-minor-mode
      (evil-append 1))
    (when quit-frame-after
      (add-hook 'kill-buffer-hook
                `(lambda ()
                   (when (eq (selected-frame) ,(selected-frame))
                     (delete-frame)))))))
(defvar mu4e-from-name "Andrii Dorokhov"
  "Name used in \"From:\" template.")
(defadvice! mu4e~draft-from-construct-renamed (orig-fn)
  "Wrap `mu4e~draft-from-construct-renamed' to change the name."
  :around #'mu4e~draft-from-construct
  (let ((user-full-name mu4e-from-name))
    (funcall orig-fn)))
(setq message-signature mu4e-from-name)
(defun +mu4e-update-personal-addresses ()
  (let ((primary-address
         (car (cl-remove-if-not
               (lambda (a) (eq (mod (apply #'* (cl-coerce a 'list)) 600) 0))
               (mu4e-personal-addresses)))))
    (setq +mu4e-personal-addresses
          (and primary-address
               (append (mu4e-personal-addresses)
                       (mapcar
                        (lambda (subalias)
                          (concat subalias "@"
                                  (subst-char-in-string ?@ ?. primary-address)))
                        '("orgmode"))
                       (mapcar
                        (lambda (alias)
                          (replace-regexp-in-string
                           "\\`\\(.*\\)@" alias primary-address t t 1))
                        '("contact" "timothy")))))))

(add-transient-hook! 'mu4e-compose-pre-hook
  (+mu4e-update-personal-addresses))
(setq mu4e-sent-folder #'+mu4e-account-sent-folder)
(defun +mu4e-account-sent-folder (&optional msg)
  (let ((from (if msg
                  (plist-get (car (plist-get msg :from)) :email)
                (save-restriction
                  (mail-narrow-to-head)
                  (mail-fetch-field "from")))))
    (if (and from (string-match "@gmail\\.com>?$" from))
        "/gmail-com/Sent"
      "/sent")))

(defun +mu4e-evil-enter-insert-mode ()
  (when (eq (bound-and-true-p evil-state) 'normal)
    (call-interactively #'evil-append)))

(add-hook 'mu4e-compose-mode-hook #'+mu4e-evil-enter-insert-mode 90)

(after! mu4e
  (defun +mu4e-ml-message-link (msg)
    "Copy the link to MSG on the mailing list archives."
    (let* ((list-addr (or (mu4e-message-field msg :list)
                          (thread-last (append (mu4e-message-field-raw msg :list-post)
                                               (mu4e-message-field msg :to)
                                               (mu4e-message-field msg :cc))
                                       (mapcar (lambda (e) (plist-get e :email)))
                                       (mapcar (lambda (addr)
                                                 (when (string-match-p "emacs.*@gnu\\.org$" addr)
                                                   (replace-regexp-in-string "@" "." addr))))
                                       (delq nil)
                                       (car))))
           (msg-url
            (pcase list-addr
              ("emacs-orgmode.gnu.org"
               (format "https://list.orgmode.org/%s" (mu4e-message-field msg :message-id)))
              (_ (user-error "Mailing list %s not supported" list-addr)))))
      (message "Link %s copied to clipboard"
               (propertize (gui-select-text msg-url) 'face '((:weight normal :underline nil) link)))
      msg-url))

  (add-to-list 'mu4e-view-actions (cons "link to message ML" #'+mu4e-ml-message-link) t))
(defun +browse-url-orgmode-ml (url &optional _)
  "Open an orgmode list url using notmuch."
  (let ((id (and (or (string-match "^https?://orgmode\\.org/list/\\([^/]+\\)" url)
                     (string-match "^https?://list\\.orgmode\\.org/\\([^/]+\\)" url))
                 (match-string 1 url))))
    (mu4e-view-message-with-message-id id)))

(defun +mu4e-compose-org-ml-setup ()
  (when (string-match-p "\\`orgmode@" user-mail-address)
    (goto-char (point-min))
    (save-restriction
      (mail-narrow-to-head)
      (when (string-empty-p (mail-fetch-field "to"))
        (re-search-forward "^To: .*$")
        (replace-match "To: emacs-orgmode@gnu.org")
        (advice-add 'message-goto-to :after #'+mu4e-goto-subject-not-to-once)))
    (when (and org-msg-mode
               (re-search-forward "^:alternatives: (\\(utf-8 html\\))" nil t))
      (replace-match "utf-8" t t nil 1))
    (if org-msg-mode
        (let ((final-elem (org-element-at-point (point-max))))
          (when (equal (org-element-property :type final-elem) "signature")
            (goto-char (org-element-property :contents-begin final-elem))
            (delete-region (org-element-property :contents-begin final-elem)
                           (org-element-property :contents-end final-elem))
            (setq-local org-msg-signature
                        (format "\n\n#+begin_signature\n%s\n#+end_signature"
                                (cdr +mu4e-org-ml-signature)))
            (insert (cdr +mu4e-org-ml-signature) "\n")))
      (goto-char (point-max))
      (insert (car +mu4e-org-ml-signature)))
    (setq default-directory
          (replace-regexp-in-string
           (regexp-quote straight-build-dir)
           "repos"
           (file-name-directory (locate-library "org"))))))

(defun +mu4e-goto-subject-not-to-once ()
  (message-goto-subject)
  (advice-remove 'message-goto-to #'+mu4e-goto-subject-not-to-once))

(defvar +mu4e-org-ml-signature
  (cons
   "All the best,
Andrii Dorokhov"
   "All the best,\\\\
@@html:<b>@@Andrii Dorokhov@@html:</b>@@

-\u200b- \\\\
Andrii Dorokhov")
  "Plain and Org version of the org-ml specific signature.")

(add-hook 'mu4e-compose-mode-hook #'+mu4e-compose-org-ml-setup 1)
(setq org-msg-signature "\n\n#+begin_signature\nAll the best,\\\\\n@@html:<b>@@Andrii Dorokhov@@html:</b>@@\n#+end_signature")
(defun +org-msg-goto-body (&optional end)
  "Go to either the beginning or the end of the body.
END can be the symbol top, bottom, or nil to toggle."
  (interactive)
  (let ((initial-pos (point)))
    (org-msg-goto-body)
    (when (or (eq end 'top)
              (and (or (eq initial-pos (point)) ; Already at bottom
                       (<= initial-pos ; Above message body
                           (save-excursion
                             (message-goto-body)
                             (point))))
                   (not (eq end 'bottom))))
      (message-goto-body)
      (search-forward (format org-msg-greeting-fmt
                              (concat " " (org-msg-get-to-name)))))))
(map! :map org-msg-edit-mode-map
      :after org-msg
      :n "G" #'+org-msg-goto-body)
(defun +org-msg-goto-body-when-replying (compose-type &rest _)
  "Call `+org-msg-goto-body' when the current message is a reply."
  (when (and org-msg-edit-mode (eq compose-type 'reply))
    (+org-msg-goto-body)))

(advice-add 'mu4e~compose-handler :after #'+org-msg-goto-body-when-replying)

;;; config-mail.el ends here



;;:------------------------
;;; File Templates
;;:------------------------

(set-file-template! "\\.tex$" :trigger "__" :mode 'latex-mode)
(set-file-template! "\\.org$" :trigger "__" :mode 'org-mode)
(set-file-template! "/LICEN[CS]E$" :trigger '+file-templates/insert-license)


;;:------------------------
;;; Plaintext
;;:------------------------

;; This block defines `+setup-text-mode-left-margin' and
;; `+text-mode-left-margin-width'.

(after! text-mode
  (add-hook! 'text-mode-hook
    (unless (derived-mode-p 'org-mode)
      ;; Apply ANSI color codes
      (with-silent-modifications
        (ansi-color-apply-on-region (point-min) (point-max) t)))))
(defvar +text-mode-left-margin-width 1
  "The `left-margin-width' to be used in `text-mode' buffers.")

(defun +setup-text-mode-left-margin ()
  (when (and (derived-mode-p 'text-mode)
             (not (and (bound-and-true-p visual-fill-column-mode)
                       visual-fill-column-center-text))
             (eq (current-buffer) ; Check current buffer is active.
                 (window-buffer (frame-selected-window))))
    (setq left-margin-width (if display-line-numbers
                                0 +text-mode-left-margin-width))
    (set-window-buffer (get-buffer-window (current-buffer))
                       (current-buffer))))

(add-hook 'window-configuration-change-hook #'+setup-text-mode-left-margin)
(add-hook 'display-line-numbers-mode-hook #'+setup-text-mode-left-margin)
(add-hook 'text-mode-hook #'+setup-text-mode-left-margin)
(defadvice! +doom/toggle-line-numbers--call-hook-a ()
  :after #'doom/toggle-line-numbers
  (run-hooks 'display-line-numbers-mode-hook))
(remove-hook 'text-mode-hook #'display-line-numbers-mode)

;;:------------------------
;;; !Pkg org-modern
;;:------------------------

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
  (setq python-shell-virtualenv-path "/Users/apogee/.local/share/virtualenvs/myenv/")
  (add-hook 'python-mode-hook
            (lambda ()
              (require 'lsp-pyright)
              (lsp-deferred)
              (setq indent-tabs-mode nil)
              (setq tab-width 4)))
  (pyvenv-activate "/Users/apogee/.local/share/virtualenvs/myenv")
  )

;;PYENV

(use-package! pipenv
  :defer t
  :hook (python-mode . pipenv-mode)
  :config
  (pyvenv-workon "/Users/apogee/.local/share/virtualenvs/myenv")  
  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

;;Add myvenv
(defun my-activate-virtualenv ()
  (interactive)
  (let ((venv-path "/Users/apogee/.local/share/virtualenvs/myenv/bin/activate"))
    (when (file-exists-p venv-path)
      (vterm-send-string (format "source %s" venv-path))
     (vterm-send-return))))


(use-package pyvenv
  :config
  (pyvenv-mode 1)
  (pyvenv-workon "myenv"))

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
  (dap-ui-controls-mode 1)  ; Відображення панелі з кнопками відлагодження
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
;;; Styled-components NPM
;;:------------------------


(use-package! polymode
   :ensure t
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
;;; !Pkg treemacs
;;:------------------------
(after! treemacs
  (defvar treemacs-file-ignore-extensions '()
    "File extension which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-globs '()
    "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-regexps '()
    "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
  (defun treemacs-ignore-filter (file full-path)
    "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
    (or (member (file-name-extension file) treemacs-file-ignore-extensions)
        (let ((ignore-file nil))
          (dolist (regexp treemacs-file-ignore-regexps ignore-file)
            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))
(setq treemacs-file-ignore-extensions
      '(;; LaTeX
        "aux"
        "ptc"
        "fdb_latexmk"
        "fls"
        "synctex.gz"
        "toc"
        ;; LaTeX - glossary
        "glg"
        "glo"
        "gls"
        "glsdefs"
        "ist"
        "acn"
        "acr"
        "alg"
        ;; LaTeX - pgfplots
        "mw"
        ;; LaTeX - pdfx
        "pdfa.xmpi"
        ))
(setq treemacs-file-ignore-globs
      '(;; LaTeX
        "*/_minted-*"
        ;; AucTeX
        "*/.auctex-auto"
        "*/_region_.log"
        "*/_region_.tex"))



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
;;EDIT
;;:------------------------

;;Set default tab char’s display width to 4 spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;; make tab key always call a indent command.
;;(setq-default tab-always-indent t)
;; make tab key call indent command or insert tab character, depending on cursor position
;;(setq-default tab-always-indent nil)
;; make tab key do indent first then completion.
;;(setq-default tab-always-indent 'complete)

;;Set fill-column
(setq-default fill-column 88)

;;Delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;Overwrite active region
(delete-selection-mode t)

;;Indent new line automatically on ENTER
(global-set-key (kbd "RET") 'newline-and-indent)

;;Multiple Cursors
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

;;Enable moving line or region, up or down
(use-package move-text
  :config
  (move-text-default-bindings))

;;Expand region
(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))


(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))


;;Navigation

(defun switch-to-first-matching-buffer (regex)
  (switch-to-buffer (car (remove-if-not (apply-partially #'string-match-p regex)
                                        (mapcar #'buffer-name (buffer-list))))))
;;Focus buffer by name

(defun +select-window-by-name (regexp)
  "Selects the window with buffer NAME"
  (select-window
   (car (seq-filter
     (lambda (window)
       (string-match-p regexp (buffer-name (window-buffer window))))
     (window-list-1 nil 0 t)))))

;;Simplify whitespace style
(setq-default whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))

;;Enable soft-wrap lines
(global-visual-line-mode t)

;;Highlight syntax
(global-font-lock-mode t)

;;Highlight current line
;;(global-hl-line-mode +1)


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

