(setq local-linux-font "DejaVu Sans Mono-10")
(setq local-linux-bigfont "DejaVu Sans Mono-12")

(setq-default c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode nil)

(defun gst-dev-off ()
  (interactive)
  "Unset GStreamer local dev environment"
  (setenv "CURRENT_GST")
  (setenv "GST_PLUGIN_PATH")
  (setenv "GST_PLUGIN_SCANNER")
  (setenv "GST_REGISTRY")
  (setenv "GST_PLUGIN_SYSTEM_PATH")
  (setenv "LD_LIBRARY_PATH")
  (setq pathlist (split-string (getenv "PATH") path-separator))
  (setq new-pathlist (delete "/home/matt/gstreamer-dev/bin" pathlist))
  (setq new-path (mapconcat 'identity new-pathlist ":"))
  (setenv "PATH" new-path)
  )

(defun gst-dev()
  (interactive)
  "Set up GStreamer local dev environment"
  (setenv "CURRENT_GST" "/home/matt/gstreamer-dev")
  (setenv "LD_LIBRARY_PATH" "/home/matt/gstreamer-dev/lib/x86_64-linux-gnu")
  (setenv "PATH" (concat "/home/matt/gstreamer-dev/bin" path-separator (getenv "PATH")))
  (setenv "PKG_CONFIG_PATH" "/home/matt/gstreamer-dev/lib/x86_64-linux-gnu/pkgconfig")
  (setenv "GST_PLUGIN_PATH" "/home/matt/gstreamer-dev/lib/x86_64-linux-gnu/gstreamer-1.0")
  (setenv "GST_PLUGIN_SCANNER" "/home/matt/gstreamer-dev/libexec/gstreamer-1.0/gst-plugin-scanner")
  (setenv "GST_REGISTRY" "/home/matt/gstreamer-dev/registry.dat")
  (setenv "GST_PLUGIN_SYSTEM_PATH" "")
  (setq exec-path (append '("/home/matt/gstreamer-dev/bin") exec-path))
  )
;(gst-dev)

(setq exec-path (append '("/home/matt/.cargo/bin" "/home/matt/.local/bin") exec-path))
(setenv "PATH" (concat "/home/matt/.local/bin" path-separator (getenv "PATH")))
;; Set up SMTP for sending mail from Emacs using matt@standard.ai
(setq user-mail-address "matt@standard.ai")
(setq mail-user-agent 'message-user-agent)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; Auto mode for berglas files
(add-to-list 'auto-mode-alist '("berglas-[0-9]*\\'" . conf-toml-mode))

;; Create a frame with a big font -- useful for meetings
(defun new-frame-big-font()
  (interactive)
  "Create a new frame with a large font"
  (setq my/frame-list (list (make-frame)))
  (set-frame-font "DejaVu Sans Mono-12" nil my/frame-list))
