;; -*- coding: utf-8; lexical-binding: t -*-

;; Немного глобальных настроек самого Emacs, затем
;; настройки пакетов в алфавитном порядке
(use-package emacs
  :functions
  (crm-indicator
   delete-current-file
   split-window-prefer-vertically)
  :config
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

  (defun delete-current-file ()
    "Удалить текущий файл и закрыть буфер."
    (interactive)
    (delete-file (buffer-file-name))
    (kill-current-buffer))

  (defun split-window-prefer-vertically (window)
    "Если открыто лишь одно окно, исключая минибуффер,
    в таком случае разделить окно горизонтально."
    (if (and (one-window-p t)
             (not (active-minibuffer-window)))
        (let ((split-width-threshold 0))
          (split-window-sensibly window))
      (split-window-sensibly window)))

  ;; Увеличение порога срабатывания сборщика мусора
  (setq gc-cons-threshold (* 50 1000 1000))
  ;; Подключение репозиториев пакетов
  (require 'package)
  (setq package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)

  (require 'use-package)
  (setq use-package-always-ensure t)

  ;; Перенести переменные, создаваемые Custom в отдельный файл
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))

  ;; Отключение элементов интерфейса
  (setq inhibit-splash-screen   t)
  (setq inhibit-startup-screen t)

  ;; Отдельные настройки для GUI версии
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq-default frame-title-format '("Emacs " emacs-version))

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
  (setq dired-kill-when-opening-new-dired-buffer t)

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
  ;; Разделять окно только по вертикали
  (setq split-window-preferred-function 'split-window-prefer-vertically)

  ;; Фиксы для работы в Windows
  (when (eq system-type 'windows-nt)
    (require 'tramp)
    (push '("-tt")
          (cadr (assoc 'tramp-login-args
                       (assoc "ssh" tramp-methods))))
    (setq shell-file-name "C:/Windows/system32/bash.exe")
    (setenv "ESHELL" "bash"))


  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode)
          (go-mode . go-ts-mode)
          (c-mode . c-ts-mode)))

)

;; Управление буферами и список буферов по C-x C-b
(use-package ibuffer
  :config
  (defalias 'list-buffers 'ibuffer))

;; Настройки company-mode
(use-package company
  :config
  (setq tab-always-indent 'complete)
  (setq company-idle-delay 0.05)
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "C-<tab>") 'company-complete))
(use-package company-box
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (require 'all-the-icons)
  (setf (alist-get 'min-height company-box-frame-parameters) 6)
  (setq company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-backends-colors nil
        company-box-icons-all-the-icons
        `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
          (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
          (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
          (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
          (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
          (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
          (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
          (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
          (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
          (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
          (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
          (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
          (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
          (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
          (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
          (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
          (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
          (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
          (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
          (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
          (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
          (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
          (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green)))))

;; Улучшенная работа с crontab файлами
(use-package crontab-mode)

;; Улучшенный dired-mode
(use-package all-the-icons)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Пакет для автоматического использования su/sudo для редактирования файлов
;; в случае если файл не может быть отредактирован текущим пользователем
(use-package auto-sudoedit
  :config
  (auto-sudoedit-mode 1))

;; Настройки для работы с Docker
(use-package docker-compose-mode)
(use-package dockerfile-mode)

;; Базовый пакет для поддержки Go
(use-package go-ts-mode
  :config
  (setq go-ts-mode-indent-offset tab-width))

;; Базовый пакет для поддержки PHP
(use-package php-mode)
;; Базовый пакет для поддержки Python
(use-package python-mode)
;; Поддержка виртуальных окружений Python
(use-package pyvenv
  :config
  (pyvenv-mode t)
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))
(use-package pyvenv-auto
  :hook ((python-ts-mode . pyvenv-auto-run)))

(use-package eglot
  :hook
  ((python-ts-mode . eglot-ensure))
  ((go-ts-mode . eglot-ensure))
  ((c-ts-mode . eglot-ensure))
  :config
  (add-hook 'before-save-hook 'eglot-format))

;; Цветовые схемы
(use-package ef-themes
  :init
  (defun set-seasonal-theme ()
    "Установить светлую тему в соответствии с текущим временем года"
    (let ((current-month (string-to-number (format-time-string "%m"))))
      (cond
       ;; `ef-winter' тёмная тема, заменил её на `ef-frost'
       ((member current-month '(12 1 2)) (load-theme 'ef-frost :no-confirm))
       ((member current-month '(3 4 5)) (load-theme 'ef-spring :no-confirm))
       ((member current-month '(6 7 8)) (load-theme 'ef-summer :no-confirm))
       ;; `ef-autumn' тёмная тема, заменил её на `ef-arbutus'
       ((member current-month '(9 10 11)) (load-theme 'ef-arbutus :no-confirm)))))
  :config
  (setq ef-themes-to-toggle '(ef-summer ef-winter))
  (mapc #'disable-theme custom-enabled-themes)
  (set-seasonal-theme))

;; Включаем прозрачное шифрование файлов при помощи GPG
;; В файле secrets.el.gpg хранятся логины и пароли, которые нельзя хранить в
;; открытом виде в init.el
(use-package epg
  :config
  (epa-file-enable)
  (setq secrets-file (expand-file-name "secrets.el.gpg" user-emacs-directory))
  (when (file-exists-p secrets-file)
    (load secrets-file)))

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
  :demand t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org-roam")
  (org-roam-completion-everywhere t)
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
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
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :functions
  (my/org-roam-select
   my/org-roam-to-hugo
   my/org-roam-update-graph
   my/org-insert-date-keyword
   my/org-export-before-parsing)
  :config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Этот код необходимо поместить в ~/org-roam/.dir-locals.el ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ((org-mode . ((org-hugo-base-dir . "~/weirdvic\.github\.io")
  ;;             (eval . (setq-local
  ;;                      org-export-before-parsing-functions
  ;;                      (append org-export-before-parsing-functions '(my/org-export-before-parsing)))))))

  (require 'org-roam-dailies)
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
              (kill-buffer (current-buffer)))))))
    (my/org-roam-update-graph))

  (defun my/org-roam-update-graph ()
    "Функция для обновления графа связей заметок org-roam"
    (interactive)
    (let* ((venv-bin (expand-file-name "~/.emacs.d/venv/bin/python3"))
           (roam2graph-script (expand-file-name "~/.emacs.d/roam2graph.py"))
           (output-file (expand-file-name "~/weirdvic.github.io/static/graph.json")))
      (shell-command (format "%s %s > %s 2>/dev/null" venv-bin roam2graph-script output-file))
      (message "Graph data generated and saved to '%s'" output-file)))

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
          ("m" "mycelium" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              ":PROPERTIES:
:KIND: mycelium
:END:
#+title: ${title}
#+hugo_tags:
#+hugo_categories:
#+hugo_lastmod: Time-stamp: <>\n\n\n")
           :immediate-finish t
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
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t))

;; Экспорт из .org в Hugo SSG
(use-package ox-hugo
  :pin melpa
  :after ox
  :config
  (setq org-export-with-author nil))

;; Экспорт из .org посредством pandoc
(use-package ox-pandoc)

;; pdf-tools для чтения PDF файлов
(use-package pdf-tools)

;; Пакет для работы клавиш емакса в русской раскладке
(use-package reverse-im
  :demand t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

;; Сохранение истории автодополнения команд между перезапусками Emacs
(use-package savehist
  :init
  (savehist-mode))

;; Настройки tab-bar для работы со вкладками
(use-package tab-bar
  :demand t
  :config
  (when (eq system-type 'windows-nt)
    (setq w32-lwindow-modifier 'super)
    (w32-register-hot-key [s-{])
    (w32-register-hot-key [s-}])
    (w32-register-hot-key [s-t])
    (w32-register-hot-key [s-w]))
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-tab-choice "*scratch*")
  (setq tab-bar-tab-hints t)
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (setq tab-bar-select-tab-modifiers "super")
  (global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab)
  (global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab)
  (global-set-key (kbd "s-t") 'tab-bar-new-tab)
  (global-set-key (kbd "s-w") 'tab-bar-close-tab))

;; Настройки tree-sitter для подсветки синтаксиса
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Для обновления грамматик использовать команду
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
(use-package tree-sitter-langs
  :config
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (add-to-list 'tree-sitter-major-mode-language-alist '(go-ts-mode . go))
  ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
  :after tree-sitter)

;; Настройки TRAMP
(use-package tramp
  :config
  (add-to-list 'tramp-remote-path "/data/data/com.termux/files/usr/bin" t)
  :custom
  (vc-handled-backends '(Git))
  (tramp-verbose 2))

;; Пакеты treemacs для отображения файлового дерева
(use-package treemacs
  :bind ("<f9>" . treemacs)
  :custom
  (treemacs-width 30)
  :config
  (add-hook 'treemacs-mode-hook (lambda () (text-scale-decrease 1)))
  (treemacs-follow-mode 1)
  (treemacs-project-follow-mode 1))
(use-package treemacs-all-the-icons
  :after (treemacs))
(use-package treemacs-magit
  :after (treemacs magit))

;; Пакет vertico для вертикального автодополнения
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-count 15
        vertico-cycle t
        vertico-scroll-margin 0
        vertico-resize t))

;; Вертикальное автодополнение для имён файлов
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
(use-package vertico-posframe
  :after vertico
  :config
  (vertico-posframe-mode 1))

;; Подсказывать справку по доступным сочетаниям при нажатии
;; C-h во время ввода сочетания
(use-package which-key
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 3)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))
(use-package which-key-posframe
  :after which-key
  :config
  (which-key-posframe-mode 1))
