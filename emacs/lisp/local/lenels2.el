;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Basic style
;;
(setq-default c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode nil)
(defun my-c-mode-common-hook ()
  (progn
    (c-add-style "my-c-style" '("stroustrup"
				(c-basic-offset . 4)
				(c-offsets-alist
				 (substatement-open . 0))
				))
    (setq c-default-style "my-c-style")
    (c-set-style "my-c-style")
    ))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


;;
;; Use the closest Makefile when compiling C/C++ code.
;;
(require 'cl) ; If you don't have it already

(defun* get-closest-make-directory (&optional (file "Makefile"))
  "Determine the directory pathname containing the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (loop
     for d = default-directory then (expand-file-name ".." d)
     if (file-exists-p (expand-file-name file d))
     return d
     if (equal d root)
     return nil)))

(add-hook 'c-mode-hook (lambda () (set (make-local-variable 'compile-command) (format "make -C %s" (get-closest-make-directory)))))
(add-hook 'c++-mode-hook (lambda () (set (make-local-variable 'compile-command) (format "make -C %s" (get-closest-make-directory)))))


;; Font size
(setq local-linux-font "Monospace-10")

;; Create a new status template and append to the end of the org file
(defun append-new-status()
  "Create a new status appended to end of document"
  (interactive)
  (goto-char (point-max))
  (insert "* ")
  (org-time-stamp '(16) t)
  (re-search-backward " [0-9][0-9]:[0-9][0-9]]")
  (replace-match "]")
  (insert "\n** What I did\n- \n** What I'm going to do\n- \n** Blockers\n- None\n")
  (search-backward "- ")
  (search-backward "- ")
  (search-backward "- ")
  (end-of-line)
  )

;; Select the last status entry, filter content for Confluence mark-up, and copy to clipboard
(defun export-status-to-clipboard ()
  "Export the current status entry to the X clipboard, re-formatted for Confluence markup"
  (interactive)
  (goto-char (point-max))
  (search-backward "* [")
  (setq status-string (buffer-substring-no-properties (point) (point-max)))
  (setq status-string (replace-regexp-in-string "^\\* \\[.*" "Matt" status-string))
  (setq status-string (replace-regexp-in-string "\\*\\* " "* " status-string))
  (setq status-string (replace-regexp-in-string "- " "** " status-string))
  (gui-set-selection 'CLIPBOARD status-string)
  (goto-char (point-max))
  )

;; Org mode keybindings for status functions
(define-key org-mode-map (kbd "C-c s") 'append-new-status)
(define-key org-mode-map (kbd "C-c e") 'export-status-to-clipboard)
