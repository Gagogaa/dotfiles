(package-initialize)

;;; Add some package servers
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;;; Get rid of the bell bacause omg is it bad
(setq ring-bell-function 'ignore)

;;; Load a non-ugly theme
(load-theme 'zenburn t)
;;; To help guess keybindings
(which-key-mode)
;;; I hate word wraping
(set-default 'truncate-lines t)

;;; Show me them sweet parens
(setq show-paren-delay 0)
(show-paren-mode t)

;;; Fix tabs
(setq-default indent-tabs-mode nil)

;;; IDO-MODE (Interactive do)
(require 'ido)
(ido-mode t)
(ido-everywhere t)

;;; For common lisp mode
;;; Don't forget to run the slime command when working on a lisp file
(setq inferior-lisp-program "clisp")

;;; Use an extra sharp font
(add-to-list 'default-frame-alist '(font . "xos4 Terminus-14" ))
(set-face-attribute 'default t :font "xos4 Terminus-14" )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (slime zenburn-theme which-key avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
