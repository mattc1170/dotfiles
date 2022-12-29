;; Font that looks the best with my work laptop
(setq local-linux-font "DejaVu Sans Mono-10")

;; Environment variables to use with a custom GStreamer install
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
  (set exec-path ("/home/matt/gstreamer-dev/bin" "/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin" "/usr/games" "/usr/local/games" "/snap/bin" "/home/matt/.local/bin" "/home/matt/bin")
  )
