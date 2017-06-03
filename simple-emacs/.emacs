;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Built-in Customizations ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Disable word wraping
(set-default 'truncate-lines t)

;;; Only insert spaces
(setq indent-tabs-mode nil)
(setq tab-width 2)

;; Disable the bell
(setq ring-bell-function 'ignore)

;;; Use an extra sharp font
;;;(add-to-list 'default-frame-alist '(font . "xos4 Terminus-14" ))
;;;(set-face-attribute 'default t :font "xos4 Terminus-14" )
;;; Set the lisp variable

(setq inferior-lisp-program "clisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Keybindings ;;;;
;;;;;;;;;;;;;;;;;;;;;

;;; Better bindings for recording macros
(global-set-key '[(f1)]          'call-last-kbd-macro)
(global-set-key '[(shift f1)]    'toggle-kbd-macro-recording-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom Functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toggle-kbd-macro-recording-on ()
  "One-key keyboard macros: turn recording on."
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

(defun toggle-kbd-macro-recording-off ()
  "One-key keyboard macros: turn recording off."
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-on)
  (end-kbd-macro))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Downloaded packages ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package-initialize)

;;; Add package archives
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
;;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;;; Ensure that use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package expand-region
  :ensure t
  :bind
  ("M-n" . er/expand-region)
  ("M-p" . er/contract-region))

(use-package ace-jump-mode
  :ensure t
  :bind
  ("C-x SPC" . ace-jump-word-mode))

;;; Don't discard this just yet
;; (autoload
;;   'ace-jump-mode-pop-mark
;;   "ace-jump-mode"
;;   "Ace jump back:-)"
;;   t)
;; (eval-after-load "ace-jump-mode"
;;   '(ace-jump-mode-enable-mark-sync))
;; (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
;; (define-key global-map (kbd "C-x m") 'ace-jump-word-mode)

;;; ---Auto Generated code---

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (expand-region ace-jump-mode which-key zenburn-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
