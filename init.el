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
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq-default frame-title-format '("Emacs " emacs-version))
  ;; PDF-tools для чтения pdf в Emacs
  (use-package pdf-tools
    :ensure
    :config
    (pdf-tools-install)))

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
;; Отображать номера строк в буферах с исходниками
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(show-paren-mode t)
(electric-pair-mode t)
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

;; #############################################################################
;; #                                                                           #
;; #                Настройки пакетов с применением use-package                #
;; #                                                                           #
;; #############################################################################

;; Настройки для работы с Ansible
(use-package ansible
  :ensure)
(use-package ansible-doc
  :ensure)
(use-package ansible-vault
  :ensure)

;; Blockdiag для рисования диаграмм
(use-package blockdiag-mode
  :ensure)

(use-package ob-blockdiag
  :ensure
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((blockdiag . t))))

;; Управление буферами и список буферов по C-x C-b
(use-package bs
  :ensure)
(use-package ibuffer
  :ensure
  :config
  (defalias 'list-buffers 'ibuffer))

;; Настройки клиента IRC
(use-package circe
  :ensure
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

;; Настройки Company
(use-package company
  :ensure
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (add-to-list 'company-backends 'company-ansible)
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "C-<tab>") 'company-complete))
(use-package company-ansible
  :ensure)

;; Улучшенная работа с crontab файлами
(use-package crontab-mode
  :ensure)

;; Настройки для работы с Docker
(use-package docker-compose-mode
  :ensure)
(use-package dockerfile-mode
  :ensure)

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
  :ensure
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Eyebrowse для работы с раскладками окон
(use-package eyebrowse
  :ensure
  :config
  (eyebrowse-mode t))

;; Базовый пакет для поддержки Go
(use-package go-mode
  :after (lsp-mode)
  :ensure
  ;; Перед сохранением файла форматировать код и сортировать импорты
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports)))

;; IDO плагин
(use-package ido
  :ensure
  :config
  (ido-mode t)
  (ido-everywhere t)
  (icomplete-mode t)
  (setq ido-virtual-buffers t)
  (setq ido-enable-flex-matching t))

;; Настройки lsp-mode для python и go
;; https://emacs-lsp.github.io/lsp-mode/page/installation/

;; Префикс для lsp-command-keymap
(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :ensure
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
  :ensure
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
  :ensure
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;; Цветовые схемы
(use-package nord-theme
  :ensure
  :init
  (load-theme 'nord t))

;; Улучшенный модлайн
(use-package mood-line
  :ensure
  :config
  (mood-line-mode))

;; Пакет для экспорта из .org в другие форматы
(use-package ox-pandoc
  :ensure)

;; Настройки для php-mode
(use-package php-mode
 :ensure t
 :mode
 ("\\.php\\'" . php-mode))

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

(use-package phpunit
  :ensure t)
(provide 'lang-php)

;; Настройки Projectile
(use-package projectile
  :ensure
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

;; Racket mode для работы с Scheme
(use-package racket-mode
  :ensure
  :mode "\\.scm\\'"
  :config
  ;; Запуск кода по нажатию F5
  (add-hook 'racket-mode-hook
	    (lambda ()
	      (define-key racket-mode-map (kbd "<f5>") 'racket-run))))

;; Подсветка скобок
(use-package rainbow-delimiters
  :ensure
  :hook (prog-mode . rainbow-delimiters-mode))

;; Пакет для работы клавиш емакса в русской раскладке
(use-package reverse-im
  :ensure
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

;; Пакет для автоматического использования su/sudo для редактирования файлов
;; в случае если файл не может быть отредактирован текущим пользователем
(use-package su
  :ensure
  :config
  (su-mode +1))

;; Пакет vterm для эмулятора терминала внутри Emacs
(use-package vterm
  :ensure
  :bind
  ("s-x" . vterm)
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
  :ensure
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))
