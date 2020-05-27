(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(eval-when-compile
  (require 'use-package))

;; #############################################################################
;; #                                                                           #
;; #                     Настройки интерфейса и переменных                     #
;; #                                                                           #
;; #############################################################################

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
(add-hook 'text-mode-hook 'linum-mode)
;; Подсветка парных скобок
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
(global-set-key (kbd "C-q") 'undo)
(global-set-key (kbd "C-z") 'quoted-insert)
;; Прокрутка по одной линии за раз
(setq scroll-step 1)
;; Уведомления в модлайне для IRC
(add-hook 'rcirc-mode-hook
  (lambda ()
    (rcirc-track-minor-mode 1)))

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
  (load-library "~/.emacs.d/secrets.el.gpg"))

;; Улучшенный модлайн
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

;; Цветовая схема
;; (use-package organic-green-theme
;;   :ensure t
;;   :config (load-theme 'organic-green t))
(load-theme 'tango t)

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
  (setq rcirc-authenticate-before-join t)
  (setq rcirc-bright-nicks (quote ("wvc")))
  (setq rcirc-default-nick "wvc")
  (setq rcirc-prompt "%n@%t: ")
  (setq rcirc-server-alist
   (quote
    (("irc.freenode.net" :channels
      ("#em.slashem.me" "#hardfought" "#nethack"))))))

;; Автодополнение
(use-package company
  :ensure t
  :config
  (add-to-list 'company-backends 'company-ansible)
  (add-hook 'after-init-hook 'global-company-mode))
(use-package company-ansible
  :ensure t)

;; Дополнительные пакеты
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
(use-package go-mode
  :ensure t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Go Mono" :foundry "    " :slant normal :weight normal :height 120 :width normal)))))
