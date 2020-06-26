;; Подключение репозиториев пакетов
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;;; Если пакет use-package не установлен, его нужно скачать и
;;; установить
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

;; Цветовая схема
(load-theme 'tango t)
;; Отключение элементов интерфейса
(setq inhibit-splash-screen   t)
(setq inhibit-startup-message t)

;; Отдельные настройки для GUI версии
(when (window-system)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq-default frame-title-format '("%f [%m]"))
  ;; PDF-tools для чтения pdf в Emacs
  (use-package pdf-tools
    :ensure t
    :config
    (pdf-tools-install)))

;; Использовать 4 пробела вместо табуляции
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
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
;; При вводе парного элемента (скобки, кавычки), автоматически добавлять
;; закрывающий элемент и ставить курсор между элементами
(electric-pair-mode t)
(show-paren-mode 1)
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
(global-set-key (kbd "C-q") 'undo)
(global-set-key (kbd "C-z") 'quoted-insert)
;; Переключить комментарии выделенного фрагмента по C-c C-k
(define-key prog-mode-map (kbd "C-c C-k") 'comment-or-uncomment-region)
;; Прокрутка по одной линии за раз
(setq scroll-step 1)

;; #############################################################################
;; #                                                                           #
;; #                Настройки пакетов с применением use-package                #
;; #                                                                           #
;; #############################################################################

;; The Silver Searcher -- ag, утилита для рекурсивного поиска в директориях
(use-package ag
  :ensure t)

;; Настройки Ansible
(use-package ansible
  :ensure t)
(use-package ansible-doc
  :ensure t)
(use-package ansible-vault
  :ensure t)

;; Управление буферами и список буферов по C-x C-b
(use-package bs
  :ensure t)
(use-package ibuffer
  :ensure t
  :config
  (defalias 'list-buffers 'ibuffer))

;; Настройки Company
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (add-to-list 'company-backends 'company-ansible)
  (add-hook 'after-init-hook 'global-company-mode))
(use-package company-ansible
  :ensure t)

;; Настройки Docker
(use-package docker-compose-mode
  :ensure t)
(use-package dockerfile-mode
  :ensure t)

;; Автодополнение для Go и Python
(use-package eglot
  :ensure t
  :config
  (setq eglot-put-doc-in-help-buffer t)
  (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-,") 'pop-tag-mark)
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyls")))
  :hook
  (go-mode . eglot-ensure)
  (python-mode . eglot-ensure))

;; Включаем прозрачное шифрование файлов при помощи GPG
;; В файле secrets.el.gpg хранятся логины и пароли, которые нельзя хранить в
;; открытом виде в init.el
(use-package epg
  :config
  (epa-file-enable)
  (setq secrets-file (expand-file-name "secrets.el.gpg" user-emacs-directory))
  (when (file-exists-p secrets-file)
  (load secrets-file)))

;; Настройки eshell
(use-package eshell-prompt-extras
  :ensure t
  :after (eshell esh-opt)
  :config
  (with-eval-after-load "esh-opt"
  (autoload 'epe-theme-dakrone "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-dakrone)))
(use-package esh-autosuggest
  :ensure t
  :hook (eshell-mode . esh-autosuggest-mode))
(use-package eshell-toggle
  :ensure t
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  :bind
  ("C-`" . eshell-toggle))

;; Получать значение $PATH из шелла
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Базовый пакет для поддержки Go
(use-package go-mode
  :ensure t)

;; IDO плагин
(use-package ido
  :ensure t
  :config
  (ido-mode t)
  (ido-everywhere t)
  (icomplete-mode t)
  (setq ido-virtual-buffers t)
  (setq ido-enable-flex-matching t))

;; Настройки Magit
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;; Улучшенный модлайн
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

;; Настройки Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

;; Racket mode для работы с Scheme
(use-package racket-mode
  :ensure t
  :mode "\\.scm\\'"
  :config
  ;; Запуск кода по нажатию F5
  (add-hook 'racket-mode-hook
	    (lambda ()
	      (define-key racket-mode-map (kbd "<f5>") 'racket-run))))

;; Подсветка скобок
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Настройки для IRC
(use-package rcirc
  :after (epg)
  :init
  ;; Аутентификация на сервере перед подключением к каналам
  (setq rcirc-authenticate-before-join t)
  ;; Подсветка никнеймов
  (setq rcirc-bright-nicks (quote ("wvc")))
  ;; Стандартный никнейм
  (setq rcirc-default-nick "wvc")
  (setq rcirc-prompt "%n@%t: ")
  ;; Список серверов и каналов
  (setq rcirc-server-alist
	(quote
	 (("irc.freenode.net" :channels
	   ("#em.slashem.me" "#hardfought" "#nethack")))))
  ;; Уведомления в модлайне
  (add-hook 'rcirc-mode-hook
	    (lambda ()
	      (rcirc-track-minor-mode 1)))
  ;; Хук для того чтобы буфер с IRC автоматически скроллился
  ;; и строка отправки сообщения оставалась внизу экрана
  (add-hook 'rcirc-mode-hook
	    (lambda ()
	      (set (make-local-variable 'scroll-conservatively)
		   8192))))

;; Пакет для работы клавиш емакса в русской раскладке
(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

;; Интерфейс для управления Transmission на домашнем сервере
(use-package transmission
  :after (epg)
  :ensure t)

;; Подсказывать справку по доступным сочетаниям при нажатии
;; C-h во время ввода сочетания.
(use-package which-key
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

;; Отмечать строки длиннее 80 символов
(use-package whitespace
  :ensure t
  :config
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (global-whitespace-mode t))
