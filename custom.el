(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4343cbc036f09361b2912119c63573433df725f599bfbdc16fb97f1e4847a08b" default))
 '(ispell-dictionary nil)
 '(native-comp-async-report-warnings-errors nil)
 '(org-safe-remote-resources '("\\`https://fniessen\\.github\\.io\\(?:/\\|\\'\\)"))
 '(package-selected-packages
   '(project treemacs tree-sitter-langs treesit-auto magit tree-sitter-mode tree-sitter-lang tab-bar-mode vterm which-key-posframe vertico-posframe treemacs-magit treemacs-all-the-icons terraform-mode terraform-doc reverse-im rainbow-delimiters pyvenv-auto python-mode php-mode pdf-tools ox-hugo org-roam-ui orderless mood-line kubedoc kele go-mode fsharp-mode exec-path-from-shell eglot ef-themes dockerfile-mode docker-compose-mode crontab-mode consult-org-roam company-box circe-notifications auto-sudoedit all-the-icons-dired))
 '(safe-local-variable-values
   '((eval setq-local org-export-before-parsing-functions
           (append org-export-before-parsing-functions
                   '(my/org-export-before-parsing)))))
 '(vterm-buffer-name-string "%s vterm"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight semi-bold :height 115 :width normal))))
 '(ansi-color-black ((t (:background "#555753" :foreground "#2e3436"))))
 '(ansi-color-blue ((t (:background "#66aadd" :foreground "#2266cc"))))
 '(ansi-color-cyan ((t (:background "#00dddd" :foreground "#008888"))))
 '(ansi-color-green ((t (:background "#77cc00" :foreground "#448800"))))
 '(ansi-color-magenta ((t (:background "#cc88bb" :foreground "#886699"))))
 '(ansi-color-red ((t (:background "#ff3333" :foreground "#cc1100"))))
 '(ansi-color-white ((t (:background "#d3d7cf" :foreground "#b5b9b2"))))
 '(ansi-color-yellow ((t (:background "#eedd00" :foreground "#aa6600"))))
 '(multi-magit-repo-heading ((t (:inherit magit-section-heading :box nil)))))
