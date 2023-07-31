;; -*- coding: utf-8; lexical-binding: t -*-
;; Увеличение порога срабатывания сборщика мусора
(setq gc-cons-threshold (* 50 1000 1000))
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
(setq inhibit-startup-screen t)

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
(setq create-lockfiles         nil)

;; Пустые строки выделить глифами
(setq-default indicate-empty-lines t)

;; Переносить текст по словам, подсвечивать скобки
(setq word-wrap t)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(show-paren-mode t)
(electric-pair-mode t)

;; Прокрутка по одной линии за раз
(setq scroll-step 1)

;; Предпочитать новые файлы
(setq load-prefer-newer t)

;; Юникодные многоточия…
(setq truncate-string-ellipsis "…")

;; Всегда предпочитать Юникод другим кодировкам
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

;; Отображение времени в 24 часовом формате вместо AM/PM
(setq display-time-24hr-format t)

;; При нажатии `a' на строке в dired-mode, открывать в том же буфере
(put 'dired-find-alternate-file 'disabled nil)

;; Дни недели и месяцы на русском языке
(setq calendar-week-start-day 1
      calendar-day-name-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
      calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май"
                                 "Июнь" "Июль" "Август" "Сентябрь"
                                 "Октябрь" "Ноябрь" "Декабрь"])

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

;; #############################################################################
;; #                                                                           #
;; #                Настройки пакетов с применением use-package                #
;; #                                                                           #
;; #############################################################################

;; Поиск при помощи ag
(use-package ag)

;; Управление буферами и список буферов по C-x C-b
(use-package ibuffer
  :config
  (defalias 'list-buffers 'ibuffer))

;; Настройки клиента IRC
(use-package circe
  :after epg
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
(use-package company-box
  :after company
  :config (company-box-mode 1))

;; Улучшенная работа с crontab файлами
(use-package crontab-mode)

;; Улучшенный dired-mode
(use-package all-the-icons
  :if (display-graphic-p))
(use-package all-the-icons-dired)

(use-package dirvish
  :after (all-the-icons pdf-tools)
  :init
  (dirvish-override-dired-mode)
  (require 'dired-x)
  (setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
  (setq dired-omit-mode t)
  (setq dirvish-fd-program "fdfind")
  (setq dirvish-default-layout '(0 0.4 0.6))
  :custom
  (dirvish-quick-access-entries ; Настройка быстрого доступа
   '(("h" "~/"               "Home")
     ("d" "~/Загрузки/" "Downloads")
     ("e" "~/.emacs.d/"     "Emacs")
     ("r" "~/org-roam/" "OrgRoam")
     ("s" "~/croesus/" "Fort Ludios")))
  :config
  ;; (dirvish-peek-mode) ; Предпросмотр файлов в минибуффере
  (dirvish-side-follow-mode)
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  ;; (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-la --human-readable --group-directories-first --no-group")
  (setq dirvish-open-with-programs
  (when-let ((vlc (executable-find "vlc")))
    `((,dirvish-audio-exts . (,vlc "%f"))
      (,dirvish-video-exts . (,vlc "%f")))))
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   ("<f9>"  . dirvish-side)
   :map dirvish-mode-map ; Dirvish наследует `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ;;("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; переназначено с `describe-mode'
   ("s"   . dirvish-quicksort)    ; переназначено с `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; переназначено с `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

;; Настройки для работы с Docker
(use-package docker-compose-mode)
(use-package dockerfile-mode)

;; Цветовые схемы
(use-package solo-jazz-theme
  :config
  (load-theme 'solo-jazz t))
(use-package solaire-mode
  :after solo-jazz-theme
  :config (solaire-global-mode +1))

;; Немного глобальных настроек
(use-package emacs
  :init
  ;; Добавление индикатора к `completing-read-multiple'.
  ;; Выглядит как [CRM<разделитель>], например, [CRM,] если разделитель запятая.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Убрать курсор из запроса минибуффера
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: скрыть команды в M-x которые недоступны в текущем режиме.
  ;; Команды Vertico скрыты в обычных буферах
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Разрешить вложенные минибуфферы
  (setq enable-recursive-minibuffers t)
  (setq split-width-threshold 0))

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
  ;; Перед сохранением файла форматировать код и сортировать импорты
  :hook ((go-mode . eglot-ensure)))

;; Настройки интеграции с Kubernetes
(use-package kele
  :config
  (kele-mode 1)
  (bind-key (kbd "s-k") kele-command-map kele-mode-map))
(use-package kubedoc)

;; Настройки Magit
(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;; Улучшенный модлайн
(use-package mood-line
  :config
  (mood-line-mode))

;; Общие настройки org-mode
(use-package org
  :config
  ;; Настройки org-mode
  (org-babel-do-load-languages
 'org-babel-load-languages
        '((emacs-lisp . t)
          (python . t)
          (shell . t)
          (sql . t)))
  (setq
   org-todo-keywords '((sequence "TODO" "WORK" "DONE"))
   org-edit-src-content-indentation 0
   org-adapt-indentation nil
   org-src-tab-acts-natively t
   yaml-indent-offset 2
   org-return-follows-link t))

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
         ("C-c n p" . (lambda () (interactive)
                        (my/org-roam-to-hugo "posts" (my/org-roam-select "post"))
                        (my/org-roam-to-hugo "mycelium" (my/org-roam-select "mycelium"))))
         ("C-c n e" . consult-org-roam-file-find)
         ("C-c n b" . consult-org-roam-backlinks)
         ("C-c n r" . consult-org-roam-search)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :functions
  (my/org-roam-select
   my/org-roam-to-hugo
   my/org-insert-date-keyword
   my/org-export-before-parsing)
  :config
  (defun my/org-roam-select (kind)
    "Выбирает ноды org-roam, содержащие определённое слово в параметре KIND"
    (cl-remove-if-not
     (lambda (node)
       (string= kind (cdr (assoc-string "KIND" (org-roam-node-properties node)))))
     (org-roam-node-list)))

  (defun my/org-roam-to-hugo (section nodes)
    "Главная функция для экспорта org файлов в Hugo"
    (org-roam-db-sync)
    (let ((before-buffers (buffer-list)))
      (let* ((file-list (mapcar (lambda (node) (org-roam-node-file node)) nodes))
             (unique-files (delete-dups file-list)))
        (dolist (file unique-files)
          (with-current-buffer (find-file-noselect file)
            (let ((org-hugo-section section))
              (org-hugo-export-wim-to-md))
            (unless (member (current-buffer) before-buffers)
              (kill-buffer (current-buffer))))))))

  (defun my/org-insert-date-keyword ()
    "Добавить в заметку ключевое слово date"
    (org-roam-set-keyword "date" (format-time-string "[%Y-%m-%d %a]" (current-time))))

  (defun my/org-export-before-parsing (backend)
    "Установить параметры перед экспортом в Markdown"
    (when (string= backend "hugo")
      (org-roam-set-keyword
       "hugo_lastmod"
       (format-time-string "%Y-%m-%d" (file-attribute-modification-time (file-attributes (buffer-file-name)))))))

  (defun my/org-link-advice (fn link desc &rest rest)
    (if (string= "id" (org-element-property :type link))
        (my/org-link-by-id fn link desc rest)
      (apply fn link desc rest)))

  (defun my/org-link-by-id (fn link desc rest)
    (let ((node (org-roam-node-from-id (org-element-property :path link)))
          (protocols '("http://" "https://" "ftp://")))
      (let ((kind (cdr (assoc-string "KIND" (org-roam-node-properties node)))))
        (if (not (or (string= kind "post")
                     (string= kind "mycelium")))
          ;; Если ссылка не на пост, вставляем ссылку из ROAM_REFS заметки, либо только текст
          (if-let ((url (seq-find (lambda (arg) (cl-some (lambda (p) (string-prefix-p p arg)) protocols))
                                  (split-string-and-unquote (or (cdr (assoc-string "ROAM_REFS" (org-roam-node-properties node))) "")))))
              (format "[%s](%s)" desc url)
            desc)
        ;; Если ссылка на другой пост, ставим ссылку на него
        (apply fn link desc rest)))))

  (advice-add #'org-hugo-link :around #'my/org-link-advice)

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :immediate-finish t
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("p" "post" plain "%?"
           :if-new (file+head "${slug}.org"
":PROPERTIES:
:KIND: post
:END:
#+title: ${title}
#+hugo_tags:
#+hugo_categories:
#+hugo_lastmod: Time-stamp: <>\n\n\n")
           :immediate-finish t
           :unnarrowed t)))
  (setq org-roam-node-display-template
        (concat "${title} " (propertize "${tags}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (add-hook 'before-save-hook 'time-stamp)
  (add-hook 'org-roam-capture-new-node-hook #'my/org-insert-date-keyword))

;; Графический интерфейс для org-roam
(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; Поиск по заметкам в org-roam
(use-package consult-org-roam
  :after org-roam
  :demand t
  :init
  (require 'consult-org-roam)
  (consult-org-roam-mode 1)
  :custom
  ;; Использовать `ripgrep' для поиска с `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Кнопка для сужения диапазона поиска в `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Не показывать превью для ссылок org-roam
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-.")))

;; Включение `orderless' режима автодополнения.
;; Нужно для гибкого поиска через vertico: например при поиске ноды в org-roam
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t))

;; Пакет для экспорта из .org в другие форматы
(use-package ox-pandoc)

;; Экспорт из .org в Hugo SSG
(use-package ox-hugo
  :pin melpa
  :after ox
  :config
  (setq org-export-with-author nil))

;; pdf-tools для чтения PDF файлов
(use-package pdf-tools)

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

;; Сохранение истории автодополнения команд между перезапусками Emacs
(use-package savehist
  :init
  (savehist-mode))

;; Пакет для автоматического использования su/sudo для редактирования файлов
;; в случае если файл не может быть отредактирован текущим пользователем
(use-package su
  :config
  (su-mode +1))

;; Пакеты для работы с Terraform
(use-package terraform-doc)
(use-package terraform-mode
  :config
  (setq terraform-format-on-save t))

;; Пакет vertico для вертикального автодополнения
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-count 20)
  (setq vertico-cycle t))

;; Вертикальное автодополнение для имён файлов
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

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
