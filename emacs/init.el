;;
;; Matt Crane's Emacs init.el
;; Time-stamp: <2020-05-11 16:01:39 mcrane>

;;
;; Lots of good stuff picked up from Patrick Thomson here:
;; https://github.com/patrickt/emacs/blob/master/readme.org
;;

;; A few required variables defined with reasonable defaults.
;; Override them in local.el
(setq local-linux-font "Monospace-10")

(add-to-list 'load-path "~/.emacs.d/lisp")
(load "functions")
(load "local" t)

;; Load my color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'atom-one-dark t)

(add-to-list 'default-frame-alist '(cursor-color . "orange"))

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
    )
)

;; Always supress toolbar, menubar, tooltips, and scrollbar
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tooltip-mode 0)
;;(fringe-mode 4)

;; Scrolling to top and bottom
(setq scroll-error-top-bottom t)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;; backup file directory
(setq backup-by-copying t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backup")))

;; Stop backup/autosave littering
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Always use 'y' or 'n'
(defalias 'yes-or-no-p 'y-or-n-p)

;; default to unified diffs
(setq diff-switches "-u")

;; no splash screen
(setq inhibit-splash-screen t)

;; no reminder of what scratch is used for
(setq initial-scratch-message nil)

;; Prompts belong in the minibuffer, not in the GUI
(setq use-dialog-box nil)

;; Typing replaces selected text
(delete-selection-mode t)

;; Line / column numbers
;;(global-display-line-numbers-mode t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(column-number-mode)

; Selecting / copying text
(if (fboundp 'pc-selection-mode)
    (pc-selection-mode t))

(setq x-select-enable-clipboard t)
;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Make dired suck a lot less
;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

;; Ediff customization
(setq ediff-keep-variants "nil")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Scroll the compilation buffer
(setq compilation-scroll-output t)

;; Kill the annoying gdb I/O buffer popping up in every frame
(setq gdb-display-io-nopopup t)

;; Don't split a window vertically
(setq split-height-threshold nil)

;; Highlight current line in buffer menu
(add-hook 'buffer-menu-mode-hook 'hl-line-mode)

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Add final newline
(setq require-final-newline t)

;; Automagic parens/braces pairs
(electric-pair-mode)
(show-paren-mode)

;; Fix long line issues
(setq bidi-paragraph-direction 'left-to-right)
(if (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org)
(setq org-directory "~/Dropbox/org")
(setq org-startup-indented 't)
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(setq org-agenda-files '("~/Dropbox/org/inbox.org"
			 "~/Dropbox/org/projects.org"
			 "~/Dropbox/org/hold.org"))
(setq org-refile-targets '(("~/Dropbox/org/gtd-personal.org" :maxlevel . 2)
			   ("~/Dropbox/org/gtd-work.org" :maxlevel . 2)
			   ("~/Dropbox/org/someday.org" :maxlevel . 2)
			   ("~/Dropbox/org/hold.org" :maxlevel . 2 )))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-use-speed-commands t)
(setq org-agenda-default-appointment-duration 30)

(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("i" "Inbox" entry (file "~/Dropbox/org/inbox.org")
         "* %?\n %a\n %i\n")
        ("p" "Project" entry (file "~/Dropbox/org/projects.org")
         "* %?\nOUTCOME: \n")
	("c" "Cookbook" entry (file "~/Dropbox/org/cookbook.org")
         "%(org-chef-get-recipe-from-url)"
         :empty-lines 1)
        ("m" "Manual Cookbook" entry (file "~/Dropbox/org/cookbook.org")
         "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")))

(define-key org-mode-map (kbd "C-c a") 'org-agenda)

;; System org-capture
;; taken from: https://www.reddit.com/r/emacs/comments/74gkeq/system_wide_org_capture/
;; Open an org-capture frame from system using:
;; emacsclient -c -F '(quote (name . "capture"))' -e '(activate-capture-frame)'
;;
(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defun activate-capture-frame ()
  "run org-capture in capture frame"
  (select-frame-by-name "capture")
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (org-capture))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (when (and (equal "capture" (frame-parameter nil 'name))
             (not (eq this-command 'org-capture-refile)))
    (delete-frame)))

(defadvice org-capture-refile
    (after delete-capture-frame activate)
  "Advise org-refile to close the frame"
  (when (equal "capture" (frame-parameter nil 'name))
  (delete-frame)))

(defun org-capture-frame ()
  "create capture frame and run org-capture in it"
  (make-frame '((name . "capture")))
  (activate-capture-frame))

(define-key global-map "\C-co" 'activate-capture-frame)

;; 	("j" "Journal" entry (file+datetree ,(concat org-directory "journal.org"))
;; 	 "* %?\nEntered on %U\n %i\n %a")
;; 	("r" "Code Review" entry (file ,(concat org-directory "/code-review.org"))
;; 	 "* [[~/code_review/%f]] %t :code_review:\n %^{Reviewer}p")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mu4e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'mu4e)
;; (setq
;;  mue4e-headers-skip-duplicates  t
;;  mu4e-view-show-images t
;;  mu4e-view-show-addresses t
;;  mu4e-compose-format-flowed nil
;;  mu4e-date-format "%y/%m/%d"
;;  mu4e-headers-date-format "%Y/%m/%d"
;;  mu4e-change-filenames-when-moving t
;;  mu4e-attachments-dir "~/Downloads"

;;  mu4e-maildir       "~/Maildir"   ;; top-level Maildir
;;  ;; note that these folders below must start with /
;;  ;; the paths are relative to maildir root
;;  mu4e-refile-folder "/Archive"
;;  mu4e-sent-folder   "/Sent Items"
;;  mu4e-drafts-folder "/Drafts"
;;  mu4e-trash-folder  "/Trash")

;; ;; this setting allows to re-sync and re-index mail
;; ;; by pressing U
;; (setq mu4e-get-mail-command  "mbsync -c ~/.config/mbsync/mbsyncrc -a")
;; (setq mu4e-html2text-command "w3m -T text/html -dump")

;; ;; org / mu4e integration
;; (require 'org-mu4e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MELPA packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package diminish
  :config
  (diminish 'eldoc-mode))

(use-package doom-modeline
  :config
  (setq doom-modeline-vcs-max-length 30)
  (setq doom-modeline-height 1)
  (setq doom-modeline-icon t)
  (doom-modeline-mode))

(use-package neotree
  :config
  (setq neo-theme 'icons)
  (setq neo-window-width 40))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :delight
  :config
  (ace-window-display-mode 1))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package dired-single)

(use-package ivy
  :diminish
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers 'nil)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function #'ignore)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t))


(use-package counsel
  :diminish
  :config
  (counsel-mode 1)
  (global-set-key (kbd "M-i") 'counsel-imenu))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))

(use-package projectile
  :diminish
  :init
  (setq projectile-project-root-files #'( ".projectile" ))
  (setq projectile-project-root-files-functions #'(projectile-root-top-down
						   projectile-root-top-down-recurring
						   projectile-root-bottom-up
						   projectile-root-local))
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ag)

(use-package dtrt-indent
  :diminish
  :config
  (add-hook 'c-mode-common-hook
	    (lambda()
	      (dtrt-indent-mode t))))

(use-package restclient)

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(use-package indent-tools
  :config
  (global-set-key (kbd "C-c >") 'indent-tools-hydra/body))

(use-package ledger-mode
  :config
  (setq ledger-post-amount-alignment-column 60)
  (add-hook 'ledger-mode-hook
	     (lambda ()
	       (setq-local tab-always-indent 'complete)
	       (setq-local completion-cycle-threshold t)
	       (setq-local ledger-complete-in-steps t))))

(use-package rustic
  :config
  (setq rustic-lsp-client 'lsp-mode))

(use-package vterm)

(use-package yasnippet
  :config
  (yas-reload-all))

(use-package lsp-mode
  :config
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp))

(use-package docker-tramp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My personal keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c t") 'toggle-window-dedicated)

(global-set-key [f5] 'query-replace)
;(global-set-key [f6] )
(global-set-key [f7] 'compile)
;(global-set-key [f8])
;(global-set-key [f9] )
;(global-set-key [f10] )
;(global-set-key [f11] )
(global-set-key [f12] '(lambda() (interactive) (find-file "~/.emacs.d/init.el")))

(setq scroll-preserve-screen-position 'always)
(global-set-key (kbd "M-p") 'scroll-down)
(global-set-key (kbd "M-n") 'scroll-up)

(if (string-equal system-type "windows-nt")
    (w32-send-sys-command #xf030))

(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rtags stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'rtags nil t)
  (progn
    (setq rtags-path "/usr/local/bin")
    (setq rtags-autostart-diagnostics t)
    (rtags-diagnostics)

    (define-key c-mode-base-map (kbd "C-,")
      (function rtags-find-symbol-at-point))
    (define-key c-mode-base-map (kbd "C-.")
      (function rtags-find-references-at-point))
    (define-key c-mode-base-map (kbd "M-,")
      (function rtags-location-stack-back))
    (define-key c-mode-base-map (kbd "M-.")
      (function rtags-location-stack-forward))
    (define-key rtags-mode-map (kbd "M-o")
      nil)
    (define-key rtags-dependency-tree-mode-map (kbd "M-o")
      nil)
    (define-key rtags-references-tree-mode-map (kbd "M-o")
      nil)
    (define-key rtags-location-stack-visualize-mode-map (kbd "M-o")
      nil)
    )
  )

;; Load a local post configuration file if it exists
(load "local-post" t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(org-agenda-files '("~/Dropbox/org/inbox.org" "~/Dropbox/org/projects.org"))
 '(package-selected-packages
   '(docker-tramp lsp-mode terraform-mode yasnippet use-package ledger-mode org-chef vterm god-mode restclient projectile magit ivy dired-single ag ace-window))
 '(send-mail-function 'smtpmail-send-it)
 '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp) (comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:height 1.0))))
 '(mode-line-inactive ((t (:height 1.0)))))
(put 'erase-buffer 'disabled nil)
