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
;; - `doom-unicode-font' -- for unicode glyphs
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
;;(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;;(setq display-line-numbers-type t)

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

;;Repair mu4e icon in mode-line
(setq doom-unicode-font nil)

(setq warning-minimum-level :emergency)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;;1MB

;;Inhibit compacting font cache
(setq inhibit-compacting-font-caches t)

;;Turn off ad-handle-definition: `tramp-read-passwd’ got redefined
(setq ad-redefinition-action 'accept)

;;Character Encodings
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setq-default left-margin-width 1 right-margin-width 2) ; Define new widths.

(setq user-full-name "AndriiDorohov"
      user-mail-address "wedpositive@gmail.com")

;;EDIT
;;Set default tab char’s display width to 4 spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;; make tab key always call a indent command.
(setq-default tab-always-indent t)
;; make tab key call indent command or insert tab character, depending on cursor position
(setq-default tab-always-indent nil)
;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

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


(use-package all-the-icons
  :if (display-graphic-p))

(setq +m-color-main "#61AFEF"
      +m-color-secondary "red")

;;(use-package! modus-themes
;;  :defer t)
(setq doom-theme 'doom-dracula)


;;(setq doom-theme 'doom-moonlight)

(setq fancy-splash-image "/Users/apogee/.config/doom/icons/I-am-doom.png")


(set-frame-font "JetBrainsMono Nerd Font 15" nil t)
(custom-set-faces
 `(font-lock-comment-face ((t (:font "ChalkBoard SE" :italic t :height 1.0))))
 `(font-lock-string-face ((t (:italic t :height 1.0)))))

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 15)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 15))

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

(use-package! nyan-mode
  :after doom-modeline
  :init
  (nyan-mode))

;;(progn
;;  (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
;;  (add-to-list 'default-frame-alist '(alpha . (95 . 95))))


;;Highlight matching delimiters parens, brackets, and braces with different colors
(use-package rainbow-delimiters
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

;;Highlight hex color strings
(use-package! rainbow-mode
  :hook (((css-mode sass-mode scss-mode web-mode html-mode org-mode typescript-mode js-mode emacs-lisp-mode). rainbow-mode))
  :defer 5)


(use-package minions
  :hook (doom-modeline-mode . minions-mode))


;;----------------------MODELINE----------------------------------------------
;; If non-nil, cause imenu to see `doom-modeline' declarations.
;; This is done by adjusting `lisp-imenu-generic-expression' to
;; include support for finding `doom-modeline-def-*' forms.
;; Must be set before loading doom-modeline.
(setq doom-modeline-support-imenu t)

;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setq doom-modeline-height 25)

;; How wide the mode-line bar should be. It's only respected in GUI.
(setq doom-modeline-bar-width 4)

;; Whether to use hud instead of default bar. It's only respected in GUI.
(setq doom-modeline-hud nil)

;; The limit of the window width.
;; If `window-width' is smaller than the limit, some information won't be
;; displayed. It can be an integer or a float number. `nil' means no limit."
(setq doom-modeline-window-width-limit 85)

;; How to detect the project root.
;; nil means to use `default-directory'.
;; The project management packages have some issues on detecting project root.
;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
;; to hanle sub-projects.
;; You can specify one if you encounter the issue.
(setq doom-modeline-project-detection 'auto)

(setq doom-modeline-buffer-file-name-style 'auto)

;; Whether display icons in the mode-line.
;; While using the server mode in GUI, should set the value explicitly.
(setq doom-modeline-icon t)

;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Whether display the colorful icon for `major-mode'.
;; It respects `nerd-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)

;; Whether display the modification icon for the buffer.
;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)

;; Whether display the time icon. It respects variable `doom-modeline-icon'.
;;(setq doom-modeline-time-icon t)

;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
(setq doom-modeline-unicode-fallback nil)

;; Whether display the buffer name.
(setq doom-modeline-buffer-name t)

;; Whether highlight the modified buffer name.
(setq doom-modeline-highlight-modified-buffer-name t)

;; When non-nil, mode line displays column numbers zero-based.
;; See `column-number-indicator-zero-based'.
(setq doom-modeline-column-zero-based t)

;; Specification of \"percentage offset\" of window through buffer.
;; See `mode-line-percent-position'.
(setq doom-modeline-percent-position '(-3 "%p"))

;; Format used to display line numbers in the mode line.
;; See `mode-line-position-line-format'.
(setq doom-modeline-position-line-format '("L%l"))

;; Format used to display column numbers in the mode line.
;; See `mode-line-position-column-format'.
(setq doom-modeline-position-column-format '("C%c"))

;; Format used to display combined line/column numbers in the mode line. See `mode-line-position-column-line-format'.
(setq doom-modeline-position-column-line-format '("%l:%c"))

;; Whether display the minor modes in the mode-line.
;;(setq doom-modeline-minor-modes nil)

;; If non-nil, a word count will be added to the selection-info modeline segment.
;;(setq doom-modeline-enable-word-count nil)

;; Major modes in which to display word count continuously.
;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
;; remove the modes from `doom-modeline-continuous-word-count-modes'.
(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
;; Whether display the buffer encoding.
(setq doom-modeline-buffer-encoding t)
;; Whether display the indentation information.
(setq doom-modeline-indent-info nil)
;; Whether display the total line number。
(setq doom-modeline-total-line-number nil)
;; If non-nil, only display one number for checker information if applicable.
(setq doom-modeline-checker-simple-format t)
;; The maximum number displayed for notifications.
(setq doom-modeline-number-limit 99)
;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 12)
;; Whether display the workspace name. Non-nil to display in the mode-line.
(setq doom-modeline-workspace-name t)
;; Whether display the perspective name. Non-nil to display in the mode-line.
(setq doom-modeline-persp-name t)
;; If non nil the default perspective name is displayed in the mode-line.
(setq doom-modeline-display-default-persp-name nil)
;; If non nil the perspective name is displayed alongside a folder icon.
(setq doom-modeline-persp-icon t)
;; Whether display the `lsp' state. Non-nil to display in the mode-line.
(setq doom-modeline-lsp t)
;; Whether display the GitHub notifications. It requires `ghub' package.
(setq doom-modeline-github nil)
;; The interval of checking GitHub.
(setq doom-modeline-github-interval (* 30 60))
;; Whether display the modal state.
;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
(setq doom-modeline-modal t)

;; Whether display the modal state icon.
;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
(setq doom-modeline-modal-icon t)

;; Whether display the modern icons for modals.
(setq doom-modeline-modal-modern-icon t)

;; When non-nil, always show the register name when recording an evil macro.
(setq doom-modeline-always-show-macro-register nil)

;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
;;(setq doom-modeline-mu4e t)
;; also enable the start of mu4e-alert
;;(mu4e-alert-enable-mode-line-display)

;; Whether display the gnus notifications.
;;(setq doom-modeline-gnus t)

;; Whether gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
;;(setq doom-modeline-gnus-timer 2)

;; Wheter groups should be excludede when gnus automatically being updated.
;;(setq doom-modeline-gnus-excluded-groups '("dummy.group"))

;; Whether display the IRC notifications. It requires `circe' or `erc' package.
;;(setq doom-modeline-irc t)

;; Function to stylize the irc buffer names.
;;(setq doom-modeline-irc-stylize 'identity)

;; Whether display the battery status. It respects `display-battery-mode'.
;;(setq doom-modeline-battery t)

;; Whether display the time. It respects `display-time-mode'.
;;(setq doom-modeline-time t)

;; Whether display the misc segment on all mode lines.
;; If nil, display only if the mode line is active.
(setq doom-modeline-display-misc-in-all-mode-lines t)

;; Whether display the environment version.
(setq doom-modeline-env-version t)
;; Or for individual languages
;;(setq doom-modeline-env-enable-python t)
;;(setq doom-modeline-env-enable-ruby t)
;;(setq doom-modeline-env-enable-perl t)
;;(setq doom-modeline-env-enable-go t)
;;(setq doom-modeline-env-enable-elixir t)
;;(setq doom-modeline-env-enable-rust t)

;; Change the executables to use for the language version string
(setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
(setq doom-modeline-env-ruby-executable "ruby")
(setq doom-modeline-env-perl-executable "perl")
(setq doom-modeline-env-go-executable "go")
(setq doom-modeline-env-elixir-executable "iex")
(setq doom-modeline-env-rust-executable "rustc")

;; What to display as the version while a new one is being loaded
(setq doom-modeline-env-load-string "...")

;; By default, almost all segments are displayed only in the active window. To
;; display such segments in all windows, specify e.g.
(setq doom-modeline-always-visible-segments '(mu4e irc))

;; Hooks that run before/after the modeline version string is updated
(setq doom-modeline-before-update-env-hook nil)
(setq doom-modeline-after-update-env-hook nil)

(setq doom-modeline-height 1) ; optional

(custom-set-faces
  '(mode-line ((t (:family "Noto Sans" :height 0.98))))
  '(mode-line-active ((t (:family "Noto Sans" :height 0.98)))) ; For 29+
  '(mode-line-inactive ((t (:family "Noto Sans" :height 0.98)))))


(use-package! doom-modeline
  :hook (after-init . doom-modeline-mode)
  :defer t
  :config
  (setq doom-modeline-buffer-file-name-style 'file-name))
;;----------------------MODELINE----------------------------------------------

;;(require 'kv)
;;(require 'esxml)
;;(require 'package)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'load-path "/Users/apogee/.config/xwidgets-reuse")
;;(require 'xwidgets-reuse)
;;(require 'elfeed)
;;(require 'simple-httpd)
;;(setq httpd-root "/var/www")
;;(httpd-start)
;;(require 'dom)
;;(require 'esxml-query)

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


;;EMOJI
(use-package emojify
  :hook (erc-mode . emojify-mode))

(with-eval-after-load "emojify"
  (delete 'mu4e-headers-mode emojify-inhibit-major-modes))

;;CURSOR COLOR

(setq evil-normal-state-cursor '(box "#41a7fc")
      evil-insert-state-cursor '(bar "#00AEE8")
      evil-visual-state-cursor '(hollow "#c75ae8"))

;;Save window layout history.
(winner-mode 1)

;;Browser
(setq browse-url-generic-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")


(defun my-switch-to-xwidget-buffer (&optional a b)
  "Switch to xwidget buffer."
  (interactive)
  (switch-to-first-matching-buffer "xwidget webkit"))

(defun my-toggle-default-browser ()
  "Toggle default browser for preview"
  (interactive)
  (if (eq browse-url-browser-function #'browse-url-default-browser)
      (progn (setq browse-url-browser-function #'xwidget-webkit-browse-url)
             (advice-add 'browse-url :after #'my-switch-to-xwidget-buffer))
    (progn
      (setq browse-url-browser-function #'browse-url-default-browser)
      (advice-remove 'browse-url #'my-switch-to-xwidget-buffer))))


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
(global-hl-line-mode +1)



;;TERMINAL

(defun my-remove-cr (&optional begin end)
  "Remove line prefixes ending with carriage-return.

BEGIN END specifies region, otherwise works on entire buffer."
  (save-excursion
    (goto-char (or begin (point-min)))
    (while (re-search-forward "^.*\033\\[2K\033\\[1G" end t)
      (replace-match ""))))

;;Add myvenv
;;(defun my-activate-virtualenv ()
;;  (interactive)
;;  (let ((venv-path "/Users/apogee/.local/share/virtualenvs/myenv/bin/activate"))
;;    (when (file-exists-p venv-path)
;;      (vterm-send-string (format "source %s" venv-path))
;;     (vterm-send-return))))


;;WORKSPACES

(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))

;;COPY PATH CURRENT DIR
(defun my-copy-pwd ()
  "Copy PWD command to clipboard"
  (interactive)
  (when (buffer-file-name)
    (kill-new (replace-regexp-in-string " " "\\\\\  " (file-name-directory (buffer-file-name))))))

;;COPY CURRENT FILE NAME
(defun my-copy-file-name ()
  "Copy file name command to clipboard"
  (interactive)
  (when (buffer-file-name)
    (kill-new (file-name-nondirectory (buffer-file-name)))))

;;COPY FULL PATH
(defun my-copy-full-path ()
  "Copy full path till file to clipboard"
  (interactive)
  (when (buffer-file-name)
    (kill-new (replace-regexp-in-string " " "\\\\\  " (buffer-file-name)))))

;;OPEN VTERM CURRENT BUFFER

(defun my-vterm-change-current-directory-to-active-buffer-pwd ()
  "Just exec CD to pwd of active buffer."
  (interactive)
  (when-let* ((file-name (buffer-file-name))
              (file-dir (file-name-directory file-name))
              (file-dir (replace-regexp-in-string " " "\\\\\  " file-dir)))
    (message "FILE: %s" file-dir)
    (save-window-excursion
      (switch-to-first-matching-buffer "vterm")
      (vterm-send-C-c)
      (vterm-send-string (concat "cd " file-dir))
      (vterm-send-return)
      )
    (evil-window-down 1)))

;;FORGE OPEN REMOTE FILE

(defun my-forge-browse-buffer-file ()
  (interactive)
  (browse-url
   (let
       ((rev (cond ((and (boundp git-timemachine-mode) git-timemachine-mode) (git-timemachine-kill-revision))
                   ((and (boundp magit-gitflow-mode) magit-gitflow-mode) (magit-copy-buffer-revision))
                   (t "master")))
        (repo (forge-get-repository 'stub))
        (file (magit-file-relative-name buffer-file-name))
        (highlight
         (if
             (use-region-p)
             (let ((l1 (line-number-at-pos (region-beginning)))
                   (l2 (line-number-at-pos (- (region-end) 1))))
               (format "#L%d-L%d" l1 l2))
           ""
           )))
     (message "rev: %s" rev)
     (if (not file)
         (if-let ((path (forge--split-remote-url (forge--get-remote))))
                  (message "https://%s/%s/%s/commit/%s" (nth 0 path) (nth 1 path) (nth 2 path) rev)
           (user-error "Cannot browse non-forge remote %s" (forge--get-remote)))

       (forge--format repo "https://%h/%o/%n/blob/%r/%f%L"
                      `((?r . ,rev) (?f . ,file) (?L . ,highlight)))))))

;;TOGLE TRANCPERANCY

(setq my-transparency-disabled-p t)
(defun my-toggle-transparency ()
  "Toggle transparency"
  (interactive)
  (let* ((not-transparent-p (and (boundp 'my-transparency-disabled-p) my-transparency-disabled-p))
         (alpha (if not-transparent-p 90 100))) ; Змініть значення alpha за замовчуванням
    (setq my-transparency-disabled-p (not not-transparent-p))
    (message "%s" alpha)
    (progn
      (set-frame-parameter (selected-frame) 'alpha `(,alpha . ,alpha))
      (add-to-list 'default-frame-alist `(alpha . (,alpha . ,alpha))))))

(let ((bg-color (face-background 'default nil 'default)))
  (set-frame-parameter nil 'background-color bg-color))

(my-toggle-transparency)


;;DIRVISH

(use-package! dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
 
  (set-face-attribute 'ansi-color-blue nil :foreground "#FFFFFF")
  (dirvish-define-preview exa (file)
  "Use `exa' to generate directory preview."
  :require ("exa") ; tell Dirvish to check if we have the executable
  (when (file-directory-p file) ; we only interest in directories here
    `(shell . ("exa" "-al" "--color=always" "--icons"
               "--group-directories-first" ,file))))

(add-to-list 'dirvish-preview-dispatchers 'exa)
  ;; Go back home? Just press `bh'
  (dirvish-bookmark-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")))
  ;; (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format ; it's ok to place string inside
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  ;; Don't worry, Dirvish is still performant even you enable all these attributes
  (dirvish-attributes '(all-the-icons file-size collapse subtree-state vc-state git-msg))
  ;; Maybe the icons are too big to your eyes
  (dirvish-all-the-icons-height 0.8)
  ;; In case you want the details at startup like `dired'
  ;; (dirvish-hide-details nil)
  :config
  ;; (dirvish-peek-mode)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dirvish-reuse-session t)
  ;; Dired options are respected except a few exceptions, see *In relation to Dired* section above
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  (setq dirvish-default-layout '(1 0.3 0.7))
  ;; Enable mouse drag-and-drop files to other applications
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  (setq dirvish-attributes '(vc-state subtree-state collapse git-msg file-size))
  (advice-add #'+dired/quit-all :after (lambda () (interactive) (dirvish-kill (dirvish-prop :dv))))
  :bind
  ;; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dired-mode-map ; Dirvish respects all the keybindings in this map
   ("h" . dired-up-directory)
   ("j" . dired-next-line)
   ("k" . dired-previous-line)
   ("l" . dired-find-file)
   ("i" . wdired-change-to-wdired-mode)
   ("." . dired-omit-mode)
   ("b"   . dirvish-bookmark-jump)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("s"   . dirvish-quicksort) ; remapped `dired-sort-toggle-or-edit'
   ("?"   . dirvish-dispatch)  ; remapped `dired-summary'
   ("TAB" . dirvish-subtree-toggle)
   ("SPC" . dirvish-history-jump)
   ("M-n" . dirvish-history-go-forward)
   ("M-p" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-f" . dirvish-toggle-fullscreen)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))
;; Placement
;; (setq dirvish-use-header-line nil)     ; hide header line (show the classic dired header)
;; (setq dirvish-use-mode-line nil)       ; hide mode line
(setq dirvish-use-header-line 'global)    ; make header line span all panes

;; Height
;;; '(25 . 35) means
;;;   - height in single window sessions is 25
;;;   - height in full-frame sessions is 35
(setq dirvish-header-line-height '(25 . 35))
(setq dirvish-mode-line-height 25) ; shorthand for '(25 . 25)

;; Segments
;;; 1. the order of segments *matters* here
;;; 2. it's ok to place raw string inside
(setq dirvish-header-line-format
      '(:left (path) :right (free-space))
      dirvish-mode-line-format
      '(:left (sort file-time " " file-size symlink) :right (omit yank index)))

(use-package dired-x
  :config
  ;; Make dired-omit-mode hide all "dotfiles"
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

;; Addtional syntax highlighting for dired
(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

;; Use `all-the-icons' as Dirvish's icon backend
;;(use-package all-the-icons)

;; Or, use `vscode-icon' instead
;;(use-package vscode-icon
;;   :config
;;   (push '("jpg" . "image") vscode-icon-file-alist))

;;TREEMACS

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask" "/.mypy_cache")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

;;    (treemacs-git-mode 'deferred)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
;;    (treemacs-hide-gitignored-files-mode nil)
)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(after! (treemacs projectile)
  (treemacs-project-follow-mode 1))

(setq doom-themes-treemacs-enable-variable-pitch nil)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

;;TOOLS
;;Project Navigation
;;Bookmark for navigation inside file
;;(use-package! bm
;;  :defer t
;;  :custom-face
;;  (bm-face ((t (:foreground ,+m-color-secondary))))
;;  :bind (("C-M-n" . bm-next)
;;         ("C-M-p" . bm-previous)
;;         ("s-b" . bm-toggle)))


(use-package bm
  :demand t

  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)


  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)

  ;; where to store persistant files
;;  (setq bm-repository-file "~/.config/doom/bm-repository")

  ;; save bookmarks
  (setq-default bm-buffer-persistence t)

  ;; Loading the repository from file when on start up.
  (add-hook' after-init-hook 'bm-repository-load)

  ;; Restoring bookmarks when on file find.
  (add-hook 'find-file-hooks 'bm-buffer-restore)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

  ;; Use mouse + left fring to handle bookmarks
  (global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
  (global-set-key (kbd "C-<mouse-4>") 'bm-next-mouse)
  ;; (global-set-key (kbd "C-<mouse-3>") 'bm-previous-mouse)

  ;; fix Lisp nesting exceeds ‘max-lisp-eval-depth’ in bm-count
  (setq max-lisp-eval-depth 10000)

  :bind (("C-M-n" . bm-next)
         ("C-M-p" . bm-previous)
         ("s-b" . bm-toggle))
  )

;; HACK: To make bm work in emacs-29
;; https://github.com/joodland/bm/issues/45
(defun bm-lists (&optional direction predicate)
  "Return a pair of lists giving all the bookmarks of the current buffer.
The car has all the bookmarks before the overlay center;
the cdr has all the bookmarks after the overlay center.
A bookmark implementation of `overlay-lists'.

If optional argument DIRECTION is provided, only return bookmarks
in the specified direction.

If optional argument PREDICATE is provided, it is used as a
selection criteria for filtering the lists."
  (if (null predicate)
      (setq predicate 'bm-bookmarkp))

  (overlay-recenter (point))
  (cond ((equal 'forward direction)
         (cons nil (remq nil (mapcar predicate (overlays-in (point) (point-max))))))
        ((equal 'backward direction)
         (cons (remq nil (mapcar predicate (overlays-in (point-min) (point)))) nil))
        (t
         (cons
          (remq nil (mapcar predicate (overlays-in (point-min) (point))))
          (remq nil (mapcar predicate (overlays-in (point) (point-max))))))))

;;recentf
;;https://www.emacswiki.org/emacs/RecentFiles Recentf is a minor mode that builds a list of recently opened files. This list is is automatically saved across sessions on exiting Emacs - you can then access this list through a command or the menu.
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
;;(global-set-key "\C-x\ \C-r" 'recentf-open-files)


;;DOOM BOOKMARKS

(use-package! bookmark
  :defer t
  :config
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file "~/.doom.d/bookmarks"))

;;TERMINAL
;;VTERM

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


;;FOLDING
(use-package! origami
  :defer 2
  :bind (:map evil-normal-state-map
              ("SPC z a" . origami-toggle-node)
              ("SPC z r" . origami-open-all-nodes)
              ("SPC z m" . origami-close-all-node))
  :hook ((ng2-html-mode html-mode) . origami-mode))

;;OUTLINE

(use-package! outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    'outline-minor-faces-add-font-lock-keywords))

;;HOLD SIDELINE
(use-package! sideline
  :hook (lsp-mode-hook . sideline-mode)
  :init
  (setq sideline-backends-right '(sideline-lsp))
  :defer 2)

;;HYDRA

(use-package! hydra
  :defer t)

;;GLOBAL KEYS

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

;;MULTILANGUAGE

(use-package! reverse-im
  :defer 5
  :config
  (reverse-im-activate "russian-computer"))

;;EVIL

(use-package! evil-leader
  :after evil
  :bind (:map evil-normal-state-map
         ("f" . avy-goto-word-1)
         ("SPC n r f" . org-roam-node-find)
         ("SPC t a" . treemacs-add-project-to-workspace)
         ("SPC g t" . git-timemachine)
         ;; Compilation
         ("SPC c v" . (lambda ()
                        (interactive)
                        (compilation-display-error)
                        (+select-window-by-name "*compilation.*")))
         ;; CUSTOM
         ("SPC t i" . my-insert-todo-by-current-git-branch)
         ;; Org mode
         ("SPC d t" . org-time-stamp-inactive)
         ("SPC d T" . org-time-stamp)
         ("SPC r p" . +python/open-ipython-repl)
         ("SPC r n" . nodejs-repl)
         ("SPC t t" . ivy-magit-todos)
         ("SPC v t t" . my-toggle-transparency)
         ;; TODO: add check might be roam buffer already opened?
         ("SPC r u" . (lambda () (interactive)
                        (org-roam-ui-open)
                        (run-at-time "0.3 sec" nil (lambda () (org-roam-ui-sync-theme) (my-switch-to-xwidget-buffer)))))
         ("SPC j" . ace-window)
         ("SPC w f" . ace-window)
         ("s-Y" . xah-paste-from-register-1)
         ("s-p" . yank-from-kill-ring)
         ("s-r" . (lambda () (interactive) (set-mark-command nil) (evil-avy-goto-char)))
         ("SPC y k" . yank-from-kill-ring)
         ("s-." . ace-window)
         ;; Git
         ("SPC g o f" . my-forge-browse-buffer-file)
         :map evil-insert-state-map
         ("s-Y" . xah-copy-to-register-1)
         ("s-P" . xah-paste-from-register-1)
         ("s-p" . yank-from-kill-ring)
         ("s-." . ace-window)
         :map evil-visual-state-map
         ("s-Y" . xah-copy-to-register-1)
         ("s-P" . xah-paste-from-register-1)
         ("s-." . ace-window)
         ("SPC r r" . query-replace))
  :init
  (global-evil-leader-mode)
  :config
  (setq-default evil-kill-on-visual-paste nil)
  (evil-leader/set-key
    ;; "f" 'evil-find-char
    "f" 'avy-goto-char
    "b" 'my-switch-to-xwidget-buffer
    "x" 'my-ecmascript-formatter
    "k" 'save-buffer-without-dtw
    "w" 'avy-goto-word-0
    "]" 'flycheck-next-error
    "[" 'flycheck-previous-error

    "d" 'dap-debug
    "\\" 'ace-window

    "o" 'org-mode
    "q" 'kill-current-buffer
    "v" 'vterm
    "`" 'vterm-toggle-cd
    "i" 'git-messenger:popup-message
    "t" 'google-translate-smooth-translate
    "T" 'google-translate-query-translate

    "a" 'counsel-org-agenda-headlines
    "c" 'dired-create-empty-file
    "p" '+format/buffer
    "s" 'publish-org-blog
    "g" 'ace-window
    ;; Evil
    "=" 'evil-record-macro
    "-" 'evil-execute-macro
    "0" 'my-toggle-default-browser
    ;; "=" 'kmacro-start-macro-or-insert-counter
    ;; Lsp
    "h" 'lsp-ui-doc-show
    "e" 'lsp-treemacs-errors-list
    "l" 'lsp-execute-code-action

    "r" 'treemacs-select-window

    "m" 'toggle-maximize-buffer
    "y" 'yas-expand))

;;Evil quick jump to bracket

(use-package! evil-matchit
  :defer t)

(evilmi-load-plugin-rules '(ng2-html-mode) '(html))
(global-evil-matchit-mode 1)

;;Quick navigation

(use-package! avy
  :defer t
  :bind (:map evil-normal-state-map
              ("SPC k l" . avy-kill-whole-line)
              ("SPC k r" . avy-kill-region))
  :custom
  (avy-single-candidate-jump t)
  (avy-keys '(?w ?e ?r ?t ?y ?u ?i ?o ?p ?a ?s ?d ?f ?g ?h ?j ?k ?l ?z ?x ?c ?v ?b ?n ?m)))

;;SPELLING

(defun my-set-spellfu-faces ()
  "Set faces for correct spell-fu working"
  (interactive)
  (setq spell-fu-faces-include '(tree-sitter-hl-face:comment
                                 tree-sitter-hl-face:doc
                                 tree-sitter-hl-face:string
                                 tree-sitter-hl-face:function
                                 tree-sitter-hl-face:variable
                                 tree-sitter-hl-face:type
                                 tree-sitter-hl-face:method
                                 tree-sitter-hl-face:function.method
                                 tree-sitter-hl-face:function.special
                                 tree-sitter-hl-face:attribute
                                 font-lock-comment-face
                                 font-lock-doc-face
                                 font-lock-string-face
                                 lsp-face-highlight-textual
                                 default))
  (setq spell-fu-faces-exclude (append spell-fu-faces-exclude
                                       '(diredfl-file-name))))
(use-package! spell-fu
  :bind (:map evil-normal-state-map
              ("z g" . spell-fu-word-add))
  :defer 2
  :config
  (setq ispell-program-name "aspell")
  (setq spell-fu-directory "~/config/doom/dictionary")
  (setq ispell-program-name "aspell"
        ;;           ;; Notice the lack of "--run-together"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=56"))
  (setq spell-fu-ignore-modes '(dired-mode vterm-mode elfeed-search-mode))

  (add-hook 'spell-fu-mode-hook
            (lambda ()
              (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
              (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "ru"))
              (spell-fu-dictionary-add
               (spell-fu-get-personal-dictionary "en-personal" "/Users/apogee/.config/doom/dictionary/.pws"))
              (spell-fu-dictionary-add
               (spell-fu-get-personal-dictionary "ru-personal" "/Users/apogee/.config/doom/dictionary/ru.pws"))))

  ;; Camel case support
  (setq-default spell-fu-word-regexp
                (rx
                 (or
                  ;; lowercase
                  (seq
                   (one-or-more lower)
                   (opt
                    (any "'’")
                    (one-or-more lower)
                    word-end))

                  ;; capitalized
                  (seq
                   upper
                   (zero-or-more lower)
                   (opt
                    (any "'’")
                    (one-or-more lower)
                    word-end))

                  ;; uppercase
                  (seq
                   (one-or-more upper)
                   (opt
                    (any "'’")
                    (one-or-more upper)
                    word-end)))))

  (defun cs/spell-fu-check-range (pos-beg pos-end)
    (let (case-fold-search)
      (spell-fu-check-range-default pos-beg pos-end)))

  (setq-default spell-fu-check-range #'cs/spell-fu-check-range)
  (global-spell-fu-mode)
  (my-set-spellfu-faces))

;;GRAMMAR

(use-package! lsp-grammarly
  :defer t)
  ;; :hook (text-mode . (lambda ()
  ;;                      (require 'lsp-grammarly)
  ;;                      (lsp-deferred))))

;;GOOGLE TRANSLATE

(use-package! google-translate
  :defer 10
  :bind
  (:map google-translate-minibuffer-keymap
        ("C-k" . google-translate-next-translation-direction)
        ("C-n" . google-translate-next-translation-direction)
        ("C-l" . google-translate-next-translation-direction))
  :config
  (require 'google-translate-smooth-ui)
  (setq google-translate-backend-method 'curl)
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-translation-directions-alist
        '(("en" . "ru") ("ru" . "en") ))
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

;;JINJA

(use-package! jinja2-mode
  :defer t)

;;MARKDOWN

(use-package! grip-mode
  :after markdown-mode
  :custom
  (browse-url-browser-function 'browse-url-generic)
  ;; (grip-url-browser #'browse-url-firefox-program)
  :config
  (let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential))))

;;MAGIT

(use-package! magit
  :defer t
  :bind (:map magit-mode-map
         ("s-<return>" . magit-diff-visit-worktree-file)
         :map evil-normal-state-map
         ("SPC g i" . (lambda () (interactive) (wakatime-ui--clear-modeline) (magit-status))))
  :hook
  (magit-process-mode . compilation-minor-mode)
  :config
  (define-key transient-map        "q" 'transient-quit-one)
  (define-key transient-edit-map   "q" 'transient-quit-one)
  (define-key transient-sticky-map "q" 'transient-quit-seq)
  (add-hook 'magit-process-mode #'disable-magit-hooks)
  ;; (add-hook 'magit-process-mode-hook #'compilation-mode)
  (setcdr magit-process-mode-map (cdr (make-keymap)))
  (set-keymap-parent magit-process-mode-map special-mode-map)
  (advice-add
   'ansi-color-apply-on-region
   :before
   #'my-remove-cr)
  (setq magit-process-finish-apply-ansi-colors t))


;;GITHUB GIST
(use-package! gist                       ;
  :defer t
  :bind (:map gist-list-menu-mode-map
         ("j" . next-line)
         ("k" . previous-line)
         ("c" . gist-fork)
         ("x" . gist-kill-current)
         ("f" . avy-goto-word-1)
         ("v" . evil-visual-char)
         :map evil-normal-state-map
         ("SPC g l g" . gist-list)))

;;FORGE

(use-package! forge
  :after magit
  :config
  (setq auth-sources '("~/.authinfo"))
;;  (push `(,+m-work-gitlab-url ,(concat +m-work-gitlab-url "/api/v4")
;;          "gpalex" forge-gitlab-repository)
;;        forge-alist)
)


;;BLAMER

(use-package blamer
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 30)
)

(defun my-blamer-tooltip-func (commit-info)
  (let ((commit-date (plist-get commit-info :commit-date))
        (commit-time (plist-get commit-info :commit-time)))
    (message "%s" commit-info)
    (format "%s - %s" commit-date commit-time)))

(setq blamer-tooltip-function 'my-blamer-tooltip-func)


;;CODE REVIEW

(use-package! code-review
  :defer t
  :config
  (setq code-review-new-buffer-window-strategy #'switch-to-buffer))

;;GIT GUTTER

(after! git-gutter
  (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x n") 'git-gutter:next-hunk))

(after! git-gutter-fringe
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))


;;TABS

(use-package centaur-tabs
  :init
  (setq centaur-tabs-enable-key-bindings t)
  :config
  (setq centaur-tabs-style "slant"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-show-new-tab-button t
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-set-bar 'under
        centaur-tabs-show-count nil
        ;; centaur-tabs-label-fixed-length 15
        ;; centaur-tabs-gray-out-icons 'buffer
        ;; centaur-tabs-plain-icons t
        x-underline-at-descent-line t
        centaur-tabs-left-edge-margin nil)
  (centaur-tabs-change-fonts (face-attribute 'default :font) 100)
  (centaur-tabs-headline-match)
  ;; (centaur-tabs-enable-buffer-alphabetical-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-S-<prior>" . centaur-tabs-move-current-tab-to-left)
  ("C-S-<next>" . centaur-tabs-move-current-tab-to-right)
  (:map evil-normal-state-map
        ("g t" . centaur-tabs-forward)
        ("g T" . centaur-tabs-backward)))

;;VERTICO
;;Incremental narrowing, completion


;;marginalia
;;Marginalia are marks or annotations placed at the margin of the page of a book or in this case helpful colorful annotations placed at the margin of the minibuffer for your completion candidates. Marginalia can only add annotations to be displayed with the completion candidates. It cannot modify the appearance of the candidates themselves, which are shown as supplied by the original commands.

;;https://github.com/minad/marginalia
(use-package marginalia
  :general
  (:keymaps 'minibuffer-local-map
   "M-A" 'marginalia-cycle)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

;;(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)

(use-package vertico
  :demand t                             ; Otherwise won't get loaded immediately
  :general
  (:keymaps '(normal insert visual motion)
   "M-." #'vertico-repeat
   )
  (:keymaps 'vertico-map
   "<tab>" #'vertico-insert ; Set manually otherwise setting `vertico-quick-insert' overrides this
   "<escape>" #'minibuffer-keyboard-quit
   "?" #'minibuffer-completion-help
   "C-M-n" #'vertico-next-group
   "C-M-p" #'vertico-previous-group
   ;; Multiform toggles
   "<backspace>" #'vertico-directory-delete-char
   "C-w" #'vertico-directory-delete-word
   "C-<backspace>" #'vertico-directory-delete-word
   "RET" #'vertico-directory-enter
   "C-i" #'vertico-quick-insert
   "C-o" #'vertico-quick-exit
   "M-o" #'kb/vertico-quick-embark
   "M-G" #'vertico-multiform-grid
   "M-F" #'vertico-multiform-flat
   "M-R" #'vertico-multiform-reverse
   "M-U" #'vertico-multiform-unobtrusive
   "C-l" #'kb/vertico-multiform-flat-toggle
   )
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
         (minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved
         )
  :custom
  (vertico-count 13)
  (vertico-resize nil)
  (vertico-cycle nil)
  ;; Extensions
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 50)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  (vertico-multiform-categories
   '((file reverse)
     (consult-grep buffer)
     (consult-location)
     (imenu buffer)
     (library reverse indexed)
     (org-roam-node reverse indexed)
     (t reverse)
     ))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (consult-lsp-diagnostics)
     ))
  :init
  (defun kb/vertico-multiform-flat-toggle ()
    "Toggle between flat and reverse."
    (interactive)
    (vertico-multiform--display-toggle 'vertico-flat-mode)
    (if vertico-flat-mode
        (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))
  (defun kb/vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))

  ;; Workaround for problem with `tramp' hostname completions. This overrides
  ;; the completion style specifically for remote files! See
  ;; https://github.com/minad/vertico#tramp-hostname-completion
  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
                 kb/basic-remote-try-completion kb/basic-remote-all-completions nil))
  :config
  (vertico-mode)
  ;; Extensions
  (vertico-multiform-mode)

  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
                                          (lambda (orig cand prefix suffix index _start)
                                            (setq cand (funcall orig cand prefix suffix index _start))
                                            (concat
                                             (if (= vertico--index index)
                                                 (propertize "» " 'face 'vertico-current)
                                               "  ")
                                             cand)))
  )




;;orderless
;;https://github.com/oantolin/orderless This package provides an orderless completion style that divides the pattern into space-separated components, and matches candidates that match all of the components in any order. Each component can match in any one of several ways: literally, as a regexp, as an initialism, in the flex style, or as multiple word prefixes. By default, regexp and literal matches are enabled.
(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles basic-remote ; For `tramp' hostname completion with `vertico'
                   orderless
                   ))
     ))

  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-flex
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-style-dispatchers
   '(prot-orderless-literal-dispatcher
     prot-orderless-strict-initialism-dispatcher
     prot-orderless-flex-dispatcher
     ))
  :init
  (defun orderless--strict-*-initialism (component &optional anchored)
    "Match a COMPONENT as a strict initialism, optionally ANCHORED.
The characters in COMPONENT must occur in the candidate in that
order at the beginning of subsequent words comprised of letters.
Only non-letters can be in between the words that start with the
initials.

If ANCHORED is `start' require that the first initial appear in
the first word of the candidate.  If ANCHORED is `both' require
that the first and last initials appear in the first and last
words of the candidate, respectively."
    (orderless--separated-by
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
      (cl-loop for char across component collect `(seq word-start ,char))
      (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
      (when (eq anchored 'both)
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))

  (defun orderless-strict-initialism (component)
    "Match a COMPONENT as a strict initialism.
This means the characters in COMPONENT must occur in the
candidate in that order at the beginning of subsequent words
comprised of letters.  Only non-letters can be in between the
words that start with the initials."
    (orderless--strict-*-initialism component))

  (defun prot-orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun prot-orderless-strict-initialism-dispatcher (pattern _index _total)
    "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "," pattern)
      `(orderless-strict-initialism . ,(substring pattern 0 -1))))

  (defun prot-orderless-flex-dispatcher (pattern _index _total)
    "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "." pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  )


;;INDENT
(use-package highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

(defun my-highlighter (level responsive display)
  (if (> 1 level)
      nil
    (highlight-indent-guides--highlighter-default level responsive display)))

(setq highlight-indent-guides-highlighter-function 'my-highlighter)

;;KEYS

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


;;Flycheck

(use-package! flycheck
  :defer 2
  :bind (:map evil-normal-state-map
              ("SPC f ]" . flycheck-next-error)
              ("SPC f [" . flycheck-previous-error)
              ("SPC e l" . flycheck-list-errors)))

;;Formatters

;;(use-package! format-all
;;  :defer t
;;  ;; :hook ((js2-mode typescript-mode ng2-html-mode ng2-ts-mode go-mode) . format-all-mode)
;;  :hook ((json-mode go-mode dart-mode) . format-all-mode)
;;  :config
;;  (add-to-list '+format-on-save-enabled-modes 'typescript-mode t)
;;  (add-to-list '+format-on-save-enabled-modes 'ng2-mode t)
;;  (add-to-list '+format-on-save-enabled-modes 'js2-mode t))

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


;;PRETTIER

(use-package! prettier
  :defer 5
  :hook ((typescript-tsx-mode typescript-mode js2-mode json-mode ng2-mode ng2-html-mode html-mode) . prettier-mode))



;;EDITING

;;Quick log inserting
(use-package! turbo-log
  :defer t
  :bind (("C-s-l" . turbo-log-print)
         ("C-s-i" . turbo-log-print-immediately)
         ("C-s-h" . turbo-log-comment-all-logs)
         ("C-s-s" . turbo-log-uncomment-all-logs)
         ("C-s-x" . turbo-log-delete-all-logs)
         ("C-s-[" . turbo-log-paste-as-logger )
         ("C-s-]" . turbo-log-paste-as-logger-immediately))
  :custom
  (turbo-log-allow-insert-without-tree-sitter-p t)
  ;; (turbo-log-payload-format-template "")
  ;; (turbo-log-payload-format-template "\x1b[35m%s: ")
  (turbo-log-payload-format-template "%s: ")
  :config
  (turbo-log-configure
   :modes (typescript-mode js2-mode web-mode ng2-ts-mode js-mode)
   :strategy merge
   :post-insert-hooks (prettier-prettify lsp)
   :msg-format-template "'🦄: %s'"))

;;Autopair

;;(use-package! autopair
;;  :defer 5
;;  :config
;;  (autopair-global-mode))

;;Hold undo system

(use-package! vundo
  :defer 1
  :config
  ;; Take less on-screen space.
  (setq vundo-compact-display t)

  ;; Better contrasting highlight.
  (custom-set-faces
   '(vundo-node ((t (:foreground "#808080"))))
   '(vundo-stem ((t (:foreground "#808080"))))
   '(vundo-highlight ((t (:foreground "#FFFF00")))))

  ;; Use `HJKL` VIM-like motion, also Home/End to jump around.
  (define-key vundo-mode-map (kbd "l") #'vundo-forward)
  (define-key vundo-mode-map (kbd "<right>") #'vundo-forward)
  (define-key vundo-mode-map (kbd "h") #'vundo-backward)
  (define-key vundo-mode-map (kbd "<left>") #'vundo-backward)
  (define-key vundo-mode-map (kbd "j") #'vundo-next)
  (define-key vundo-mode-map (kbd "<down>") #'vundo-next)
  (define-key vundo-mode-map (kbd "k") #'vundo-previous)
  (define-key vundo-mode-map (kbd "<up>") #'vundo-previous)
  (define-key vundo-mode-map (kbd "<home>") #'vundo-stem-root)
  (define-key vundo-mode-map (kbd "<end>") #'vundo-stem-end)
  (define-key vundo-mode-map (kbd "q") #'vundo-quit)
  (define-key vundo-mode-map (kbd "C-g") #'vundo-quit)
  (define-key vundo-mode-map (kbd "RET") #'vundo-confirm))

(use-package! undo-fu-session
  :defer 1
  :config
  (global-undo-fu-session-mode))

(with-eval-after-load 'evil (evil-define-key 'normal 'global (kbd "C-M-u") 'vundo))

;;Automatic rename html/xml tags

(use-package! auto-rename-tag
  :defer t
  :hook ((html-mode ng2-html-mode-hook vue-mode web-mode) . auto-rename-tag-mode)
  :config
  (auto-rename-tag-mode 1))

;;Allow to transform PASCAL_CASE -> camelCase -> snake_case

(use-package! string-inflection
  :defer t
  :bind ("C-s-c" . string-inflection-all-cycle))

;;Type from json
(use-package! quicktype
  :defer t
  :bind (("C-x j v" . quicktype-json-to-type)
         ("C-x j p" . quicktype-paste-json-as-type)
         ("C-x j q" . quicktype)))

;;LSP

(use-package! lsp
  :hook ((clojure-mode
          scss-mode
          go-mode
          css-mode
          js-mode
          typescript-mode
          vue-mode
          web-mode
          ng2-html-mode
          ng2-ts-mode
          python-mode
          dart-mode
          typescript-tsx-mode) . lsp-deferred)
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
  (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))
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

;;Tree sitter (AST)

(use-package! eask)

(use-package! tree-sitter-langs)


(use-package! tree-sitter
  :after (tree-sitter-langs spell-fu)
  :hook ((go-mode typescript-mode css-mode typescript-tsx-mode html-mode scss-mode ng2-mode js-mode python-mode rust-mode ng2-ts-mode ng2-html-mode) . tree-sitter-hl-mode)
  :init
  (setq tsc-dyn-get-from nil)
  :config
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


;;PYTHON

(use-package! python-mode
  :defer t
  :hook (python-mode . format-all-mode)
  :config
  (setq python-indent-level 4)
  (add-hook 'python-mode-hook
            (lambda ()
              (require 'lsp-pyright)
              (lsp-deferred)
              (setq indent-tabs-mode nil)
              (setq tab-width 4)))
  ;;(pyvenv-activate "/Users/apogee/.local/share/virtualenvs/myenv")
  )

;;(use-package python-mode
;;    :hook (python-mode . lsp-deferred)
;;    :custom
;;    (python-shell-inpreter "python3")
;;    (dap-python-executable "python3")
;;    (dap-python-debugger 'debugpy)
;;    :config
;;    (require 'dap-python))

;;poetry
;;https://github.com/galaunay/poetry.el
;;(use-package poetry
;;   :hook
;;   (python-mode . poetry-tracking-mode)
;;  )

;;LSP

(setq lsp-pyright-multi-root nil)
(use-package! lsp-pyright
  :defer t
  :config
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-auto-search-paths t)
  (setq lsp-pyright-log-level "trace")
  (setq lsp-pyright-multi-root nil)
  (setq lsp-pyright-use-library-code-for-types t)
  ;;(setq lsp-pyright-venv-directory "/Users/apogee/.local/share/virtualenvs/myenv")
  (setq lsp-pyright-diagnostic-mode "workspace"))


;;PYENV

(use-package! pipenv
  :defer t
  :hook (python-mode . pipenv-mode)
  :config
  ;;(pyvenv-workon "/Users/apogee/.local/share/virtualenvs/myenv")
  ;;(add-hook 'pyvenv-post-activate-hooks #'lsp-restart-workspace)
  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

;;(use-package pyvenv
;;  :config
;;  (pyvenv-mode 1)
;;  (pyvenv-workon "myenv"))

;;AUTO-COMPLETE

;;(ac-config-default)


;;DEBUG

(require 'dap-python)
;; Якщо ви встановили debugpy, встановіть його як dap-python-debugger
(setq dap-python-debugger 'debugpy)

(dap-register-debug-template "Debug Current Python Buffer"
  (list :type "python"
        :name "Debug Current Python Buffer"
        :request "launch"
        :program nil)) ; Важливо встановити `:program` в `nil`

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

;;KEYS

(global-set-key (kbd "C-c C-x") 'run-python)

;;TYPESCRIPT

(setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")
(use-package! typescript-mode
  :defer 10
  :config
  (setq typescript-indent-level 2)
  (add-to-list 'auto-mode-alist '("\.ts\'" . typescript-mode)))

;;JAVSCRIPT
(use-package! js2-mode
  :defer t
  :hook (js2-mode . js2-highlight-unused-variables-mode))


;;VUE

(use-package! lsp-volar
  :after lsp-mode)

;;WEB DEVELOPMENT

(use-package! web-mode
  :defer t
  :mode (("\\.vue\\'" . web-mode)
         ("\\.tsx\\'" . typescript-tsx-mode)
         ("\\.jsx\\'" . web-mode))
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
  ;; Crutch for tsx mode
  ;; (setq font-lock-defaults '('(web-mode-fontify) t))
  ;; (setq tree-sitter-hl-use-font-lock-keywords nil)
  ;; ---------------------------END CRUTCH HERE -------------------------------------
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

;;PUG

(use-package! pug-mode
  :defer t)

;;HTML

;;EMMET

(use-package! emmet-mode
  :hook ((scss-mode . emmet-mode) (css-mode . emmet-mode) (ng2-html-mode . emmet-mode) (html-mode . emmet-mode))
  :defer 5)

;;CSS

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

;;JSON

(use-package! json-mode
  :defer t
  :hook (json-mode . format-all-mode))

;;SASS AUTOFIX

(defun my-run-sass-auto-fix ()
  "Run sass auto fix if cli tool exist"
  (interactive)
  (save-window-excursion
    (let ((default-directory (file-name-directory buffer-file-name)))
      (async-shell-command "sass-lint-auto-fix")
      ;; (revert-buffer-no-confirm)
      (message "SASS FORMATTED"))))


;;ORG

(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

(setq
    org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")
)

(defun my/pretty-symbols ()
  (setq prettify-symbols-alist
          '(("#+begin_src python" . "🐍")
            ("#+begin_src elisp" . "λ")
            ("#+begin_src jupyter-python" . "🐍")
            ("#+end_src" . "―")
            ("#+results:" . "🔨")
            ("#+RESULTS:" . "🔨"))))

(add-hook 'org-mode-hook 'my/pretty-symbols)

(global-prettify-symbols-mode +1)

(after! org
  ;; disable auto-complete in org-mode buffers
  (remove-hook 'org-mode-hook #'auto-fill-mode)
  ;; disable company too
  (setq company-global-modes '(not org-mode))
  ;; ...
  )

;;AI
;;My Helpers
;;Revert all buffers and ignore errors
(defun sm/revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))


;;TELEGRAM
(use-package! telega
  :defer t
  :bind (:map evil-normal-state-map
         ("SPC t v" . telega)
         :map telega-prefix-map
         ("g" . telega-filter-by-folder))
  :config
  (require 'telega-alert)
  (setq telega-server-libs-prefix "/usr/local/")
  (setq telega-chat-fill-column 190)
  (setq telega-use-docker nil)
  (setq telega-accounts (list                         
			  (list "andrii_dorokhov" 'telega-database-dir (expand-file-name "andrii_dorokhov" telega-database-dir))))
  (telega-alert-mode 1))
