;; Подключение репозиториев пакетов
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Если пакет use-package не установлен, его нужно скачать и установить
(unless (package-installed-p 'use-package)
  (message "Emacs will install use-package.el")
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Перенести переменные, создаваемые Custom в отдельный файл
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; #############################################################################
;; #                                                                           #
;; #                     Настройки интерфейса и переменных                     #
;; #                                                                           #
;; #############################################################################

;; Отключение элементов интерфейса
(setq inhibit-splash-screen   t)
(setq inhibit-startup-message t)

;; Отдельные настройки для GUI версии
(when (window-system)
  ;;(menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq-default frame-title-format '("Emacs " emacs-version)))

;; Использование desktop.el для сохранения состояния фрейма и окон
(desktop-save-mode 1)
;; Использовать 4 пробела вместо табуляции
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; Перед сохранением файла удалять пробелы в конце строк
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Вводимый текст перезаписывает выделенный
(delete-selection-mode t)
 ;; Добавить новую пустую строку в конец файла при сохранении
(setq require-final-newline t)
 ;; Не добавлять новую строку в конец при смещении
(setq next-line-add-newlines nil)
;; Выделять цветом результаты поиска и замены
(setq search-highlight t)
(setq query-replace-highlight t)
;; Отключаем файлы бэкапов и автосохранения
(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil)
;; Пустые строки выделить глифами
(setq-default indicate-empty-lines t)
;; Переносить по словам
(setq word-wrap t)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(show-paren-mode t)
(electric-pair-mode t)
;; Настройки Dired
(put 'dired-find-alternate-file 'disabled nil)
(defvar my/dired-preview-temp-buffer nil)

;;;###autoload
(defun my/dired-preview ()
  "Preview a file/directory at point in Dired.
It's similar to `dired-display-file' but it checks if a buffer
visiting a file is live.  If not, it will be put into a temp file
to be removed."
  (when my/dired-preview-temp-buffer (kill-buffer my/dired-preview-temp-buffer))
  (setq my/dired-preview-temp-buffer nil)
  (let* ((file (dired-get-file-for-visit))
         (buf (find-buffer-visiting file)))
    (unless buf
      (setq buf (find-file-noselect file))
      (setq my/dired-preview-temp-buffer buf))
    ;; Check if a buffer visiting file is live; if not it's a temp buffer to be
    ;; deleted.
    (display-buffer buf t)))

;;;###autoload
(defun my/dired-next-line-display ()
  (interactive)(dired-next-line 1)(my/dired-preview))

;;;###autoload
(defun my/dired-previous-line-display ()
  (interactive)(dired-previous-line 1)(my/dired-preview))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)
            ;; I don't use dired-subtree at the moment
            ;;(define-key dired-mode-map (kbd "<tab>") #'dired-subtree-toggle)
            ;;(define-key dired-mode-map (kbd "<C-tab>") #'dired-subtree-cycle)
            (define-key dired-mode-map (kbd "<down>") #'my/dired-next-line-display)
            (define-key dired-mode-map (kbd "<up>") #'my/dired-previous-line-display)))
;; Дни недели и месяцы на русском языке
(setq calendar-week-start-day 1
      calendar-day-name-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
      calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май"
                                 "Июнь" "Июль" "Август" "Сентябрь"
                                 "Октябрь" "Ноябрь" "Декабрь"])
;; Отображение времени в 24 часовом формате вместо AM/PM
(setq display-time-24hr-format t)
;; Позволяет переключаться между окнами с зажатым Shift
(windmove-default-keybindings)
;; Дополнительные клавиши для управления размерами окон
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-z") 'undo)
;; Переключить комментарии выделенного фрагмента по C-c C-k
(define-key prog-mode-map (kbd "C-c C-k") 'comment-or-uncomment-region)
;; Прокрутка по одной линии за раз
(setq scroll-step 1)
;; Настройки org-mode
(setq org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (shell . t)
    (sql . t)))
(setq org-todo-keywords
  '((sequence "TODO" "WORK" "DONE")))
(setq org-edit-src-content-indentation 0
  org-adapt-indentation nil
  org-src-tab-acts-natively t
  yaml-indent-offset 4)

;; #############################################################################
;; #                                                                           #
;; #                Настройки пакетов с применением use-package                #
;; #                                                                           #
;; #############################################################################

;; Управление буферами и список буферов по C-x C-b

(use-package ibuffer
  :config
  (defalias 'list-buffers 'ibuffer))

;; Настройки клиента IRC
(use-package circe
  :after (epg)
  :config
  ;; Выравнивание никнеймов
  (setq circe-format-say "{nick:-16s} {body}")
  ;; Отображать собственный никнейм
  (setq circe-format-self-say "<{nick}> {body}")
;; Показывать название канала в prompt
(add-hook 'circe-chat-mode-hook 'my-circe-prompt)
(defun my-circe-prompt ()
  (lui-set-prompt
   (concat (propertize (concat (buffer-name) ">")
                       'face 'circe-prompt-face)
           " ")))
;; Показывать время сообщений справа
(setq
 lui-time-stamp-position 'right-margin
 lui-time-stamp-format "%H:%M")
(add-hook 'lui-mode-hook 'my-circe-set-margin)
(defun my-circe-set-margin ()
  (setq right-margin-width 5)))

;; Настройки company-mode
(use-package company
  :config
  (setq company-idle-delay 0.05)
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "C-<tab>") 'company-complete))

;; Улучшенная работа с crontab файлами
(use-package crontab-mode)

;; Настройки для работы с Docker
(use-package docker-compose-mode)
(use-package dockerfile-mode)

;; Цветовые схемы
(use-package solo-jazz-theme
  :config
  (load-theme 'solo-jazz t))
(use-package solaire-mode
  :after (solo-jazz-theme)
  :config (solaire-global-mode +1))

;; Включаем прозрачное шифрование файлов при помощи GPG
;; В файле secrets.el.gpg хранятся логины и пароли, которые нельзя хранить в
;; открытом виде в init.el
(use-package epg
  :config
  (epa-file-enable)
  (setq secrets-file (expand-file-name "secrets.el.gpg" user-emacs-directory))
  (when (file-exists-p secrets-file)
  (load secrets-file)))

;; Получать значение $PATH из шелла
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Eyebrowse для работы с раскладками окон
(use-package eyebrowse
  :config
  (eyebrowse-mode t))

;; Базовый пакет для поддержки Go
(use-package go-mode
  :after (lsp-mode)
  ;; Перед сохранением файла форматировать код и сортировать импорты
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports)))

;; IDO плагин
(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere t)
  (icomplete-mode t)
  (setq ido-virtual-buffers t)
  (setq ido-enable-flex-matching t))

;; Настройки интеграции с Kubernetes
(use-package kele
  :config
  (kele-mode 1)
  (bind-key (kbd "s-k") kele-command-map kele-mode-map))
(use-package kubedoc)

;; Префикс для lsp-command-keymap
(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :hook (
         (go-mode . lsp)
         (php-mode . lsp)
         (python-mode . lsp)
         ;; Интеграция с which-key
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-prefer-flymake nil
        lsp-eldoc-hook nil)
  :commands lsp)

;; Дополнительно
(use-package lsp-ui
  :requires lsp-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe nil
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable nil
        lsp-ui-peek-enable nil)
  :commands lsp-ui-mode)

;; Настройки Magit
(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;; Улучшенный модлайн
(use-package mood-line
  :config
  (mood-line-mode))

;; Настройки org-roam
(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org-roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template
      (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))))

(use-package consult-ag)

(use-package consult-org-roam
   :after org-roam
   :init
   (require
    'consult-org-roam
    'consult-ag)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ag' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ag)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key (kbd "M-."))
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n r" . consult-org-roam-search))

;; Пакет для экспорта из .org в другие форматы
(use-package ox-pandoc)

;; Настройки для php-mode
(use-package php-mode
 :mode
 ("\\.php\\'" . php-mode))

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; Управление проектами
(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; Поддержка виртуальных окружений Python
(use-package pyvenv)
(use-package pyvenv-auto)

;; Подсветка скобок
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Пакет для работы клавиш емакса в русской раскладке
(use-package reverse-im
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

;; Пакет для автоматического использования su/sudo для редактирования файлов
;; в случае если файл не может быть отредактирован текущим пользователем
(use-package su
  :config
  (su-mode +1))

;; Пакеты treemacs для отображения файлового дерева
(use-package treemacs
  :bind ("<f9>" . treemacs)
  :config (treemacs-follow-mode 1))
(use-package treemacs-all-the-icons
  :after (treemacs))
(use-package treemacs-magit
  :after (treemacs magit))
(use-package treemacs-projectile
  :after (treemacs projectile))

;; Пакет vterm для эмулятора терминала внутри Emacs
(use-package vterm
  :bind ("s-x" . vterm)
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-buffer-name-string "%s vterm"))

;; Подсказывать справку по доступным сочетаниям при нажатии
;; C-h во время ввода сочетания
(use-package which-key
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 3)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

;; Зум отдельного окна на весь фрейм, как C-b z в tmux
(use-package zygospore
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))
