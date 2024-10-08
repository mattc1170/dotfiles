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

;; Create a new frame and run ediff in it
;; (defun ediff-files-new-frame ()
;;   (interactive)
;;   "Open ediff files in a new frame."
;;   (setq ediff-frame (make-frame))
;;   (select-frame ediff-frame)
;;   (ediff-files))
