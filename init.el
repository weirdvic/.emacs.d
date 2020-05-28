;; Подключение репозиториев пакетов
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(eval-when-compile
  (require 'use-package))

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
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
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
;; Отсутствие строки выделить глифами
(setq-default indicate-empty-lines t)
;; Переносить по словам
(setq word-wrap t)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
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
;; Прокрутка по одной линии за раз
(setq scroll-step 1)

;; #############################################################################
;; #                                                                           #
;; #                Настройки пакетов с применением use-package                #
;; #                                                                           #
;; #############################################################################

;; Включаем прозрачное шифрование файлов при помощи GPG
;; В файле secrets.el.gpg хранятся логины и пароли, которые нельзя хранить в
;; открытом виде в init.el
(use-package epa-file
  :config
  (epa-file-enable)
  (setq secrets-file (expand-file-name "secrets.el.gpg" user-emacs-directory))
  (when (file-exists-p secrets-file)
  (load secrets-file)))

(use-package ansible
  :ensure t)
(use-package ansible-doc
  :ensure t)
(use-package ansible-vault
  :ensure t)
(use-package docker-compose-mode
  :ensure t)
(use-package dockerfile-mode
  :ensure t)

;; Улучшенный модлайн
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

;; Подсветка скобок
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; The Silver Searcher -- ag, утилита для рекурсивного поиска в директориях
(use-package ag
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

;; Управление буферами и список буферов по C-x C-b
(use-package bs
  :ensure t)
(use-package ibuffer
  :ensure t
  :config
  (defalias 'list-buffers 'ibuffer))

;; Пакет для работы клавиш емакса в русской раскладке
(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

;; Настройки для IRC
(use-package rcirc
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

;; Автодополнение
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (add-to-list 'company-backends 'company-ansible)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-ansible
  :ensure t)

;; Автодополнение для Go при помощи gopls
;; Базовый пакет для поддержки Go
(use-package go-mode
  :ensure t)
;; Это нужно чтобы получить значение $PATH из шелла
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(setq lsp-register-custom-settings
      '(("gopls.completeUnimported" t t)
	("gopls.staticcheck" t t)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Racket mode для работы с Scheme
(use-package racket-mode
  :ensure t
  :mode "\\.scm\\'"
  :config
  ;; Запуск кода по нажатию F5
  (add-hook 'racket-mode-hook
	    (lambda ()
	      (define-key racket-mode-map (kbd "<f5>") 'racket-run))))

