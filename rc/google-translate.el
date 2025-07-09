(use-package google-translate
  :config
  (global-set-key (kbd "<f12>") 'google-translate-at-point)
  (global-set-key (kbd "S-<f12>") 'google-translate-at-point-reverse)
  (global-set-key (kbd "M-<f12>") 'google-translate-smooth-translate)
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "uk")
  )

(use-package google-translate-smooth-ui
  :after google-translate)
