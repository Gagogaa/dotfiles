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
;;; Only insert spaces
(setq indent-tabs-mode nil)
(setq tab-width 2)

;;; Use an extra sharp font
(add-to-list 'default-frame-alist '(font . "xos4 Terminus-14" ))
(set-face-attribute 'default t :font "xos4 Terminus-14" )

;;; ---Auto Generated code---

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (which-key zenburn-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
