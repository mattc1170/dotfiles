;; A few required variables defined with reasonable defaults.
;; Override them in local.el
(setq local-linux-font "Monospace-9")

;;(setq load-path (append (list nil "~/.emacs.d/lisp") load-path ))
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

;; Always supress toolbar, menubar, and scrollbar
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

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

;; Always use 'y' or 'n'
(defalias 'yes-or-no-p 'y-or-n-p)

;; default to unified diffs
(setq diff-switches "-u")

;; no splash screen
(setq inhibit-splash-screen t)

; Selecting / copying text
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

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org)
(setq org-directory "~/Dropbox/org")
(setq org-startup-indented 't)
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(setq org-agenda-files (quote("~/Dropbox/org")))
(define-key global-map "\C-cc" 'org-capture)
;; (setq org-capture-templates
;;       '(("t" "Todo" entry (file+headline "" "Tasks")
;; 	 "* TODO %?\n %i\n %a")
;; 	("j" "Journal" entry (file+datetree ,(concat org-directory "journal.org"))
;; 	 "* %?\nEntered on %U\n %i\n %a")
;; 	("r" "Code Review" entry (file ,(concat org-directory "/code-review.org"))
;; 	 "* [[~/code_review/%f]] %t :code_review:\n %^{Reviewer}p")))

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

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'icons)
  (setq neo-window-width 40))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :delight
  :config
  (ace-window-display-mode 1))

(use-package magit
  :ensure t)

(use-package dired-single
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function #'ignore)
  (setq enable-recursive-minibuffers t))


(use-package counsel
  :ensure t
  :config
  (counsel-mode 1)
  (global-set-key (kbd "M-i") 'counsel-imenu))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode 1))

(use-package projectile
  :ensure t
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

(use-package ag
  :ensure t)

(use-package dtrt-indent
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
	    (lambda()
	      (dtrt-indent-mode t))))

(use-package restclient
  :ensure t)

;; (use-package god-mode
;;   :ensure t
;;   :config
;;   (god-mode)
;;   (global-set-key (kbd "<escape>") #'god-local-mode))

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(use-package indent-tools
  :ensure t
  :config
  (global-set-key (kbd "C-c >") 'indent-tools-hydra/body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My personal keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c t") 'toggle-window-dedicated)

(global-set-key [f5] 'query-replace)
;(global-set-key [f6] ) ; f6 defined later for rtags
(global-set-key [f7] 'compile)
;(global-set-key [f8])  ; f8 defined later for rtags
;(global-set-key [f9] )
;(global-set-key [f10] )
;(global-set-key [f11] )
(global-set-key [f12] '(lambda() (interactive) (find-file "~/.emacs.d/init.el")))

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
    )
  )

;; Load a local post configuration file if it exists
(load "local-post" t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (god-mode restclient use-package projectile magit ivy dired-single ag ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
