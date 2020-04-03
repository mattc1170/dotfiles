(setq load-path (append (list nil "~/.emacs.d/lisp") load-path ))
(load "functions")
(load "custom")
(load "local")
;;(setq load-path (append (list nil "~/src/org-7.8.03/lisp") load-path ))

;; Load my color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'atom-one-dark t)
(add-to-list 'default-frame-alist '(cursor-color . "orange"))
;;(setq zenburn-override-colors-alist
;;      '(("zenburn-bg" . "#383838")))
;;(load-theme 'zenburn t)

;; MELPA packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

;; Windows variables
(if (string-equal system-type "windows-nt")
    (progn
      ;; set the executable path to use gnu-win32 stuff supplied by EmacsW32
      (setq exec-path '("C:/Program Files/Emacs/EmacsW32/gnuwin32/bin"))
      (setenv "PATH" "C:/Program Files/Emacs/EmacsW32/gnuwin32/bin")
      (add-to-list 'default-frame-alist '(font . "Lucida Console-8")))
)


;; Set up local Linux font
(if (or (string-equal system-type "gnu/linux")
	(string-equal system-type "linux"))
    (progn
      (add-to-list 'default-frame-alist (cons 'font local-linux-font))
;;      (setenv "ESHELL" (expand-file-name "~/bin/eshell"))
    )
)

;; If not using windowing system, supress the menu bar
(when (and (not window-system)
           (fboundp 'menu-bar-mode))
  (menu-bar-mode 0))

;; Always supress toolbar and scrollbar
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; ido completion
(require 'ido)
(ido-mode 'buffer)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;; backup file directory
(setq backup-by-copying t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backup")))

;; Always use 'y' or 'n'
(defalias 'yes-or-no-p 'y-or-n-p)

;; default to unified diffs
(setq diff-switches "-u")

;; no splash screen
(setq inhibit-splash-screen t)

;; Selecting / copying text
(if (fboundp 'pc-selection-mode)
    (pc-selection-mode t))

(setq x-select-enable-clipboard t) 
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Make dired suck a lot less
;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

;; Tramp customization
(setq tramp-default-method "ssh")
(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*") 

;; Ediff customization
(setq ediff-keep-variants "nil")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Org
(require 'org-install)
(setq org-directory "~/Notes")
(setq org-startup-indented 't)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (quote("~/Notes")))
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "" "Tasks")
	 "* TODO %?\n %i\n %a")
	("j" "Journal" entry (file+datetree ,(concat org-directory "journal.org"))
	 "* %?\nEntered on %U\n %i\n %a")
	("r" "Code Review" entry (file ,(concat org-directory "/code-review.org"))
	 "* [[~/code_review/%f]] %t :code_review:\n %^{Reviewer}p")))
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")

(require 'use-package)

(use-package ace-window
	     :ensure t
	     :bind ("M-o" . ace-window)
	     :delight
	     :config (ace-window-display-mode 1))

(use-package magit
	     :ensure t)

(use-package dired-single
	     :ensure t)

;; My personal keybindings

(global-set-key (kbd "C-c t") 'toggle-window-dedicated)

(global-set-key [f5] 'query-replace)
;(global-set-key [f6] ')
(global-set-key [f7] 'compile)
;(global-set-key [f8]) 
;(global-set-key [f9] )
;(global-set-key [f10] )
;(global-set-key [f11] )
(global-set-key [f12] '(lambda() (interactive) (find-file "~/.emacs")))

(if (string-equal system-type "windows-nt")
    (w32-send-sys-command #xf030))

(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rtags stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(require 'rtags)

;; (define-key c-mode-base-map (kbd "M-.")
;;   (function rtags-find-symbol-at-point))
;; (define-key c-mode-base-map (kbd "M-,")
;;   (function rtags-find-references-at-point))
;; (setq rtags-autostart-diagnostics t)
;; (rtags-diagnostics)

;; ensure that we use only rtags checking
;; https://github.com/Andersbakken/rtags#optional-1
;;(defun setup-flycheck-rtags ()
;;  (interactive)
;;  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
;;  (setq-local flycheck-highlighting-mode nil)
;;  (setq-local flycheck-check-syntax-automatically nil))

;; only run this if rtags is installed
;; (when (require 'rtags nil :noerror)
  ;; make sure you have company-mode installed
;;  (require 'company)

;;;(define-key c-mode-base-map (kbd "<f6>")
;;;  (function rtags-find-symbol-at-point))
;;;(define-key c-mode-base-map (kbd "<f8>")
;;;  (function rtags-find-references-at-point))
;;;(define-key c-mode-base-map (kbd "M-,")
;;;  (function rtags-location-stack-back))
;;;(define-key c-mode-base-map (kbd "M-.")
;;;  (function rtags-location-stack-forward))

;; disable prelude's use of C-c r, as this is the rtags keyboard prefix
  ; (define-key prelude-mode-map (kbd "C-c r") nil)
  ;; install standard rtags keybindings. Do M-. on the symbol below to
  ;; jump to definition and see the keybindings.
;;  (rtags-enable-standard-keybindings)
  ;; comment this out if you don't have or don't use helm
  ; (setq rtags-use-helm t)
  ;; company completion setup
;;  (setq rtags-autostart-diagnostics t)
;;  (rtags-diagnostics)
;;  (setq rtags-completions-enabled t)
;;  (push 'company-rtags company-backends)
;;  (global-company-mode)
  ; (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
  ;; use rtags flycheck mode -- clang warnings shown inline
;;  (require 'flycheck-rtags)
  ;; c-mode-common-hook is also called by c++-mode
;;  (add-hook 'c-mode-common-hook #'setup-flycheck-rtags)
;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization no longer used but that might be re-enabled at
;; some later date.

;; always end a file with a newline
;(setq require-final-newline 'query)

;;; uncomment for CJK utf-8 support for non-Asian users
;; (require 'un-define)

;; Default frame configuration for colors and font
;(add-to-list 'default-frame-alist '(background-color . "#282838"))
;(add-to-list 'default-frame-alist '(foreground-color . "wheat"))
;(add-to-list 'default-frame-alist '(cursor-color .  "orchid"))


;; (defun mrc-xwin-look (frame)
;;   "Setup to use if running in an X window"
;;   (color-theme-tty-dark))

;; (defun mrc-terminal-look (frame)
;;   "Setup to use if running in a terminal"
;;   (color-theme-charcoal-black))

;; (load "color-theme")
;; (defun mrc-setup-frame (frame)
;;   (set-variable 'color-theme-is-global nil)
;;   (select-frame frame)
;;   (cond
;;    ((window-system)
;;     (mrc-xwin-look frame))
;;    (t (mrc-terminal-look frame))))

;; (add-hook 'after-make-frame-functions 'mrc-setup-frame)

;; (add-hook 'after-init-hook
;;       (lambda ()
;;         (mrc-setup-frame (selected-frame))))

;; Frame geometry settings
;(add-to-list 'default-frame-alist '(top . 0))
;(add-to-list 'default-frame-alist '(left . 700))
;(add-to-list 'default-frame-alist '(width . 92))
;(add-to-list 'default-frame-alist '(height . 65))

;(setq initial-frame-alist '((top . 0) (left . 0)))

;(require 'simplenote2)
;(setq simplenote2-email "mattcrane@fastmail.com")
;(setq simplenote2-password "bigben87")
;(simplenote2-setup)

;(require 'deft)

;; SLIME Common Lisp development environment
;(setq inferior-lisp-program "sbcl")
;(load (expand-file-name "~/quicklisp/slime-helper.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages (quote (use-package ace-window magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
