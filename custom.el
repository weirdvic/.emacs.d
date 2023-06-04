(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-dictionary nil)
 '(native-comp-async-report-warnings-errors nil)
 '(package-selected-packages
   '(vertico zygospore which-key vterm use-package terraform-mode terraform-doc su solo-jazz-theme solaire-mode reverse-im rainbow-delimiters pyvenv-auto projectile php-mode pdf-tools ox-pandoc ox-hugo org-roam-ui mood-line magit lsp-ui kubedoc kele go-mode eyebrowse exec-path-from-shell dockerfile-mode docker-compose-mode dirvish crontab-mode consult-org-roam company-posframe circe all-the-icons-dired ag))
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
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight semi-bold :height 108 :width normal))))
 '(ansi-color-black ((t (:background "#555753" :foreground "#2e3436"))))
 '(ansi-color-blue ((t (:background "#66aadd" :foreground "#2266cc"))))
 '(ansi-color-cyan ((t (:background "#00dddd" :foreground "#008888"))))
 '(ansi-color-green ((t (:background "#77cc00" :foreground "#448800"))))
 '(ansi-color-magenta ((t (:background "#cc88bb" :foreground "#886699"))))
 '(ansi-color-red ((t (:background "#ff3333" :foreground "#cc1100"))))
 '(ansi-color-white ((t (:background "#d3d7cf" :foreground "#b5b9b2"))))
 '(ansi-color-yellow ((t (:background "#eedd00" :foreground "#aa6600"))))
 '(multi-magit-repo-heading ((t (:inherit magit-section-heading :box nil)))))
