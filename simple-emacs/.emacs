;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Built-In Customizations ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Only insert spaces
(setq indent-tabs-mode nil)
;;; TODO(Gregory): set this by major-mode
(setq tab-width 2)

;;; Get rid of the bell bacause omg is it bad
(setq ring-bell-function 'ignore)

;;; Disable word wraping
(set-default 'truncate-lines t)

(setq show-paren-delay 0)
(show-paren-mode t)

(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-use-filename-at-point 'guess)

(setq ring-bell-function 'ignore)

(tool-bar-mode -1)
;;; (menu-bar-mode nil)
(scroll-bar-mode -1)

(save-place-mode)

(electric-pair-mode)

(setq delete-by-moving-to-trash t)

;;; Replace the annoying yes-or-no prompt with the shorter y-or-n version
(fset 'yes-or-no-p 'y-or-n-p)

;;; I'm going to try using list-buffers for a bit
;;; (defalias 'list-buffers 'ibuffer)

(setq debug-on-error t)

;;; Make a backups directory in ~/.saves
(setq backup-directory-alist `(("." . "~/.saves")))

;;; Auto follow sym-links
(setq vc-follow-symlinks t)

;;; Use an extra sharp font
;;;(add-to-list 'default-frame-alist '(font . "xos4 Terminus-14" ))
;;;(set-face-attribute 'default t :font "xos4 Terminus-14" )

;;; For common lisp mode
;;; Don't forget to M-x slime when working with clisp
;;; TODO(Gregory): Install and use slime!!!
(setq inferior-lisp-program "clisp")

;;; (start-server)

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

;; TODO(Gregory): rename this to something else 
(defun global-key-set (pair)
  (global-set-key (kbd (car pair)) (car (cddr pair))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Keybindings ;;;;
;;;;;;;;;;;;;;;;;;;;;

(setq global-keybindings '(
                           ;;; Better bindings for recording macros
                           ("<f1>" . 'call-last-kbd-macro)
                           ("S-<f1>" . 'toggle-kbd-macro-recording-on)
                           ("<f2>" . 'shell)
                           ("M-o" . 'other-window)
                           ("C-x C-c" . 'delete-frame)
                           ))

(mapcar #'global-key-set global-keybindings)

;; (global-set-key (kbd "<f2>") 'eshell)

;; (global-set-key (kbd "M-o") 'other-window)

;;; These keybindings are used by expand region right now
;;; (global-set-key (kbd "M-p") 'backward-paragraph)
;;; (global-set-key (kbd "M-n") 'forward-paragraph)

;; (global-set-key (kbd "C-x C-c") 'delete-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Downloaded Packages ;;;;
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

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

;;; TODO(Gregory): Think about binding these functions
;;; hl-todo-previous
;;; hl-todo-next
;;; hl-todo-occur

(use-package hl-todo
  ;; hl-todo-mode keywords
  ;; HOLD
  ;; TODO
  ;; NEXT
  ;; THEM
  ;; PROG
  ;; OKAY
  ;; DONT
  ;; FAIL
  ;; DONE
  ;; NOTE
  ;; KLUDGE
  ;; HACK
  ;; FIXME
  ;; XXX
  ;; XXXX
  ;; ???

  :ensure t
  :config
  (global-hl-todo-mode))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auto Generated Code ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit hl-todo fic-ext-mode powerline zenburn-theme which-key use-package expand-region ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
