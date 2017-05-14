(package-initialize)

;;; Add some package servers
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;;; Load a non-ugly theme
(load-theme 'zenburn t)
;;; To help guess keybindings
(which-key-mode)
;;; I hate word wraping
(set-default 'truncate-lines t)

;;; Use an extra sharp font
(add-to-list 'default-frame-alist '(font . "xos4 Terminus-14" ))
(set-face-attribute 'default t :font "xos4 Terminus-14" )
