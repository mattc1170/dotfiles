(setq load-path (append (list nil "~/.emacs.d/lisp") load-path ))
(load "custom")
(load "local")
;;(setq load-path (append (list nil "~/src/org-7.8.03/lisp") load-path ))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)
(add-to-list 'default-frame-alist '(cursor-color . "orange"))

;; MELPA packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(if (string-equal system-type "windows-nt")
    (progn
      ;; set the executable path to use gnu-win32 stuff supplied by EmacsW32
      (setq exec-path '("C:/Program Files/Emacs/EmacsW32/gnuwin32/bin"))
      (setenv "PATH" "C:/Program Files/Emacs/EmacsW32/gnuwin32/bin")
      (add-to-list 'default-frame-alist '(font . "Lucida Console-8")))
)


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

(tool-bar-mode 0)
(scroll-bar-mode 0)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; ido completion
(require 'ido)
(ido-mode 'buffer)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;; backup file directory
(setq backup-by-copying t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backup")))
;(add-to-list 'backup-directory-alist
;	     (cons tramp-file-name-regexp nil))

;; autosave files to visited file name
;(setq auto-save-visited-file-name t)

;; Always use 'y' or 'n'
(defalias 'yes-or-no-p 'y-or-n-p)

;; default to unified diffs
(setq diff-switches "-u")

;; no splash screen
(setq inhibit-splash-screen t)

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

(defun double-wide (filename)
  "Double the size of the current frame and split into two windows"
  (interactive (list (read-file-name "Open file new window: ")))
  (if window-system
      (progn
	(set-frame-width (selected-frame) 189)
	(split-window-horizontally)
	(find-file-other-window filename))
    ))

(defun single-wide ()
  "Cut current frame in half and make single window"
  (interactive)
  (if window-system
      (progn
	(set-frame-width (selected-frame) 92)
	(delete-other-windows))
    ))

;; Selecting / copying text
(if (fboundp 'pc-selection-mode)
    (pc-selection-mode t))

(setq x-select-enable-clipboard t) 
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Useful subversion stuff
;; (require 'psvn)

;; Make dired suck a lot less
(require 'dired-single)
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
         loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'joc-dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (joc-dired-single-buffer "..")))))
;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))


;; My best guess about Tyco NVR C style
(defun my-c-mode-common-hook ()
  (setq tab-width 2)
  (setq c-basic-offset tab-width)
  (c-set-offset 'substatement-open  0)
  (c-set-offset 'innamespace 0)
  (setq indent-tabs-mode nil))

;(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(setq tramp-default-method "ssh")
(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*") 

;; Ediff customization
(setq ediff-keep-variants "nil")
(setq ediff-split-window-function 'split-window-horizontally)

;(require 'simplenote2)
;(setq simplenote2-email "mattcrane@fastmail.com")
;(setq simplenote2-password "bigben87")
;(simplenote2-setup)

;(require 'deft)

;; SLIME Common Lisp development environment
;(setq inferior-lisp-program "sbcl")
;(load (expand-file-name "~/quicklisp/slime-helper.el"))

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

;; Window shifting. C-x-o lets us go forward a window (or several). This
;; one lets us go back one or more windows. From Glickstein.
(defun other-window-backward (&optional n)
  "Select previous Nth window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))


;; My personal keybindings

(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-tab>")  'other-window-backward)

(global-set-key (kbd "C-.") 'double-wide)
(global-set-key (kbd "C-,") 'single-wide)

(global-set-key (kbd "M-a") 'beginning-of-buffer)
(global-set-key (kbd "M-e") 'end-of-buffer)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-h") 'beginning-of-line)
(global-set-key (kbd "M-;") 'end-of-line)
(global-set-key (kbd "M-n") 'scroll-up-nomark)
(global-set-key (kbd "M-p") 'scroll-down-nomark)

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
