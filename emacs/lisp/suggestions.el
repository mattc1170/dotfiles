;;
;; suggestions.el
;;
;; This a buffer for recording useful customizations that we might
;; want to include in either local.el or local-post.el.  Those files
;; are not committed to git because they will likely be edited to suit
;; the needs of the local site installation.  Storing suggested
;; customizations here is done so that we can check something in to
;; git that can later be copied elsewhere.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
