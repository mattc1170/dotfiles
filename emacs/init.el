;;
;; Matt Crane's Emacs init.el
;; Time-stamp: <2020-05-11 16:01:39 mcrane>

;;
;; Lots of good stuff picked up from Patrick Thomson here:
;; https://github.com/patrickt/emacs/blob/master/readme.org
;;

;; A few required variables defined with reasonable defaults.
;; Override them in local.el
(setq local-linux-font "Monospace-9")

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

;; Highlight current line in buffer menu
(add-hook 'buffer-menu-mode-hook 'hl-line-mode)

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Open all files except empty and some others in read-only mode
(add-hook 'find-file-hook
	  (lambda()
	    (if (and (not (string-equal (buffer-name) 'COMMIT_EDITMSG))
			  (file-exists-p (buffer-file-name)))
		(read-only-mode))))

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
(setq org-refile-targets '(("~/Dropbox/org/projects.org" :maxlevel . 2)
			   ("~/Dropbox/org/someday.org" :maxlevel . 2)
			   ("~/Dropbox/org/hold.org" :maxlevel . 2 )))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("i" "Inbox" entry (file "~/Dropbox/org/inbox.org")
         "* %?\n %i\n")
        ("p" "Project" entry (file "~/Dropbox/org/projects.org")
         "* %?\nOUTCOME: \n")
	("c" "Cookbook" entry (file "~/Dropbox/org/cookbook.org")
         "%(org-chef-get-recipe-from-url)"
         :empty-lines 1)
        ("m" "Manual Cookbook" entry (file "~/Dropbox/org/cookbook.org")
         "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")))

(define-key org-mode-map (kbd "C-c a") 'org-agenda)

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
  (setq ivy-use-virtual-buffers t)
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
   '(org-chef vterm god-mode restclient use-package projectile magit ivy dired-single ag ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:height 1.0))))
 '(mode-line-inactive ((t (:height 1.0)))))
(put 'erase-buffer 'disabled nil)
