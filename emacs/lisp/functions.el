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

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
         loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))

(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

(defun matt-prose-buffer ()
  "Configure the current buffer for writing prose"
  (interactive)
  (progn
    (variable-pitch-mode)
    (visual-line-mode)
    (setq cursor-mode 'bar)))

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; Create a frame with a big font -- useful for meetings
(defun new-frame-big-font()
  (interactive)
  "Create a new frame with a large font"
  (let ((new-frame-list (list (make-frame))))
    (set-frame-font local-linux-bigfont nil new-frame-list)))

;; Create a new frame and run ediff-current-file in it
(defun ediff-current-new-frame ()
  (interactive)
  "Open ediff-current-file in a new frame."
  (select-frame (make-frame))
  (ediff-current-file))

;;
;; Org dashboard prototype
;;
(defvar my/org-dashboard-frame-name "Org Dashboard")
(defvar my/org-personal-file "/home/matt/Dropbox/org/gtd/personal.org")
(defvar my/org-work-file "/home/matt/Dropbox/org/gtd/work.org")

(defun my/get-org-dashboard-frame ()
  "Return the Org Dashboard frame if it exists."
  (seq-find
   (lambda (frame)
     (string= (frame-parameter frame 'name)
              my/org-dashboard-frame-name))
   (frame-list)))

(defun my/org-dashboard-frame-p (&optional frame)
  (eq (frame-parameter (or frame (selected-frame))
                       'my-org-dashboard)
      t))

(defvar my/org-dashboard-map (make-sparse-keymap))

;; Add as a minor mode
(define-minor-mode my/org-dashboard-mode
  "Minor mode active in Org Dashboard frames."
  :lighter " OrgDash"
  :keymap my/org-dashboard-map)

(defun my/setup-org-dashboard-frame (frame)
  (with-selected-frame frame
    (set-frame-parameter frame 'my-org-dashboard t)
    (delete-other-windows)

    ;; Open the org file in the main window
    (find-file my/org-personal-file)

    ;; Generate the agenda in the background without letting it manage windows
    (let ((org-agenda-window-setup 'current-window))
      (org-agenda nil "g"))

    ;; Now we have *Org Agenda* as the current buffer.
    ;; Stash a reference to it, then switch back and split manually.
    (let ((agenda-buffer (current-buffer)))
      ;; Put the org file back in this window
      (find-file my/org-personal-file)

      ;; Set the org-dashboard minor mode
      (my/org-dashboard-mode 1)

      ;; Create the top side window for the agenda
      (let ((agenda-window
             (display-buffer-in-side-window
              agenda-buffer
              '((side . top)
                (window-height . 0.4)
                (dedicated . t)))))
        ;; Ensure focus stays on the org file
        (select-window (get-buffer-window (current-buffer)))))))

(defun my/org-dashboard-toggle-file ()
  "Toggle between personal.org and work.org in the dashboard's main window."
  (interactive)
  (unless (my/org-dashboard-frame-p)
    (user-error "Not in the Org Dashboard frame"))
  (let* ((current-file (expand-file-name (or (buffer-file-name) "")))
         (target-file (if (string= current-file (expand-file-name my/org-personal-file))
                          my/org-work-file
                        my/org-personal-file)))
    (find-file target-file)
    (my/org-dashboard-mode 1)))

(define-key my/org-dashboard-map (kbd "C-c d") #'my/org-dashboard-toggle-file)

(defun my/org-dashboard ()
  "Create or raise the Org Dashboard frame."
  (interactive)
  (let ((frame (my/get-org-dashboard-frame)))
    (if frame
        (progn
          (select-frame-set-input-focus frame)
          (raise-frame frame))
      (let ((new-frame
             (make-frame
              `((name . ,my/org-dashboard-frame-name)
                (width . 100)
                (height . 40)))))
        (my/setup-org-dashboard-frame new-frame)
        (select-frame-set-input-focus new-frame)))))
