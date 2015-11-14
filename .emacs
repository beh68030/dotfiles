;;;; my .emacs config file
;;;; for emacs24 (nox)
;;;; sudo echo "@reboot    emacs --daemon" >>/var/spool/cron/crontabs/user

;;; don't start multiple instances
(require 'server)
(if (server-running-p)
    (kill-emacs)	      	;    (save-buffers-kill-terminal)
  (server-start))

;;; package management stuff 
(require 'package)

(add-to-list 'package-archives
  '("gnu" . "https://elpa.gnu.org/packages/"))

(add-to-list 'package-archives
  '("melpa-unstable" . "https://melpa.org/packages/"))

(add-to-list 'package-archives
  '("elpa" . "https://tromey.com/elpa/"))

(add-to-list 'package-archives
  '("marmalade" . "https://marmalade-repo.org/packages/"))

(package-initialize)

(setq my-package-list '(async 
			auctex 
			browse-kill-ring
			column-marker 
			company
			company-auctex
			dash 
			evil
			eww-lnum 
			geiser
			git-commit 
			god-mode 
			goto-last-change
			helm 
			helm-core 
			hideshowvis
			magit 
			magit-popup 
			neotree 
			nlinum 
			paredit 
			rainbow-delimiters 
			undo-tree
			with-editor 
			yasnippet))

;(mapc #'package-install my-package-list)

;;; end of package stuff

;;; do/don't blink the cursor
(blink-cursor-mode 0)

;; point the default location
(setq default-driectory "~/")

;; suppress the silly welcome buffer
(setq inhibit-startup-message t)

;; preserve text files from last time
(desktop-save-mode 1)
(setq desktop-globals-to-save '(desktop-missing-file-warning
				tags-file-name tags-table-list 
				search-ring
				regexp-search-ring
				register-alist 
				file-name-history))

;; keep backup files no matter what
(setq vc-make-backup-files t)

;; set the directory for ~ backups
(setq backup-directory-alist
      '(("." . "/home/user/.emacs.d/saves")))

;; scroll one line at a time
(setq scroll-step 1)

;; change column width to 80
(setq-default fill-column 80)

;; disable any beeping
;(setq ring-bell nil)

;; hide the menu
(menu-bar-mode -1)

;; show column as well
(column-number-mode t)

;; change yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; disable annoying autoindent
(electric-indent-mode -1)

;; delete silly \ in wordwrap
;(set-display-table-slot standard-display-table 'wrap ?\ )

;; don't split up words
;(global-visual-line-mode nil)

;; turn on auto-fill by default 
;(setq-default auto-fill-function t) ; use M-q instead on a paragraph

;; show a scrollbar
;(set-scrollbar-mode 'right)

;; autocomplete popup
;(global-company-mode nil)

;; snippets
;(yas-global-mode nil)

;; change the auto-fill for IRC
;(add-hook 'erc-mode-hook (lambda () (auto-fill-mode 0)))

;; open in eww
(setq browse-url-browser-function 'eww-browse-url)

;; enable parens matching
(show-paren-mode)

;; store in main clipboard
(setq x-select-enable-primary nil)

;; Format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")

;; enable colored parens
;(global-rainbow-delimiters-mode) ; this has been deprecated...
(require 'rainbow-delimiters)
(setq rainbow-delimiters-max-face-count 7)
(set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#d75f00")
(set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "#ffff00")
(set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "#00ff00")
(set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#00cdcd")
(set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "#0000ee")
(set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "#cd00cd")
(set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground "#7f7f7f")

;;; turn on helm mode
;(helm-mode nil)

;;; save place in files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

;;; show useful data in modeline
(display-time-mode 0)
(display-battery-mode 0)

;;; set visual scrolling to up/down keys
(global-set-key (kbd "<down>") (quote scroll-up-line))
(global-set-key (kbd "<up>") (quote scroll-down-line))
;(global-set-key (kbd "<right>") (quote scroll-right))
;(global-set-key (kbd "<left>") (quote scroll-left))

;;; set this variable for SLY
;(setq inferior-lisp-program "sbcl")

;;; tweak the auto reconnect for irc
(setq erc-server-auto-reconnect t)

;;; make erc take up the whole screen width
; (add-hook 'window-configuration-change-hook 
;	      '(lambda ()
;		       (setq erc-fill-column (- (window-width) 2))))

;;; disable beep
(setq ring-bell-function 'ignore
      visible-bell t)

;;; show column numbers on the left
(global-nlinum-mode)

;; paste from X clipboard into Emacs (needs xsel package)
(defun x-paste ()
  "insert text on X11's clipboard to current buffer."
  (interactive)
  (insert-string (shell-command-to-string "xsel -b --display :0")))

(global-set-key (kbd "C-x y") 'x-paste)

;; copy from Emacs to X clipboard (needs xsel package)
(defun x-copy ()
  "copy text on local kill-ring to X11's clipboard."
  (interactive)
  (copy-region-as-kill (point) (mark t))
  (let ((process-connection-type nil))
      (let ((proc (start-process "xsel" "*Messages*" "xsel" "-i" "-b")))
        (process-send-string proc (car kill-ring))
        (process-send-eof proc))))

(global-set-key (kbd "C-x w") 'x-copy)

(global-set-key (kbd "C-c C-d") (lambda () (interactive) (insert (shell-command-to-string "date"))))

;; hilight current line
(global-hl-line-mode 1)
(set-face-background 'highlight "#000")
(set-face-foreground 'highlight nil)
(set-face-underline 'highlight t)

;; rmail stuff
;(setenv "MAILHOST" "pop.gmail.com")
;(setq rmail-primary-inbox-list '("po:xxx@gmail.com")
;      rmail-pop-password-required t)


;; gnus stuff
(setq gnus-select-method
      '(nnimap "gmail"
	              (nnimap-address "imap.gmail.com")  ; it could also be
					; imap.googlemail.com if that's your
					; server.
		             (nnimap-server-port "imaps")
			            (nnimap-stream ssl)))

(setq smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
(setq send-mail-function (quote smtpmail-send-it))
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)
(setq user-mail-address "xxx@gmail.com")
(setq user-full-name "xxx")
;(setq nnimap-authinfo-file "~/.emacs.d/.authinfo")
;(setq smtpmail-auth-credentials "~/.emacs.d/.authinfo")

;; for ansi-term
(setq explicit-shell-file-name "/bin/bash")

;;; disable a minor mode in every buffer open
(defun global-disable-mode (mode-fn)
  "Disable `MODE-FN' in ALL buffers."
  (interactive "a")
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (funcall mode-fn -1))))

;;; choose how Gnus loads html articles
; (setq mm-text-html-renderer 'html2text)

;;; turn off the disgusting colors that eww draws when reading a page's CSS
(defvar eww-disable-colorize t)
(defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
  (unless eww-disable-colorize
    (funcall orig start end fg)))
(advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
(advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
(defun eww-disable-color ()
  (interactive)
  (setq-local eww-disable-colorize t)
  (eww-reload))
(defun eww-enable-color ()
  (interactive)
  (setq-local eww-disable-colorize nil)
  (eww-reload))

;;; make line numbers look nicer
(setq nlinum-format "%d> ")

;;; enable modal "vi" editing everywhere
(evil-mode 1)
;(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

;;; always make sure connections are tls'd
(setq tls-checktrust 'ask)

;;; swap brackets with parentheses keys - no, prefer xmodmap
;(keyboard-translate ?\( ?\[)
;(keyboard-translate ?\[ ?\()
;(keyboard-translate ?\) ?\])
;(keyboard-translate ?\] ?\))

;;;; end of init.el
