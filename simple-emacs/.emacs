;;;;    ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;;;;    ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;;;    █████╗  ██╔████╔██║███████║██║     ███████╗
;;;;    ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;;; ██╗███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;;; ╚═╝╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Built-In Customizations ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Only insert spaces
;;; TODO(Gregory): set this by major-mode
(setq indent-tabs-mode nil)
(setq tab-width 2)

(setq ring-bell-function 'ignore)	; Get rid of the bell bacause omg is it bad
(setq delete-by-moving-to-trash t)
(setq inferior-lisp-program "clisp")
(setq backup-directory-alist `(("." . "~/.saves"))) ; Make a backups directory in ~/.saves
(setq vc-follow-symlinks t)		; Auto follow sym-links
(set-default 'truncate-lines t)		; Disable word wraping
;;; (setq debug-on-error t)

(setq show-paren-delay 0)
(show-paren-mode t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(save-place-mode)
(electric-pair-mode)
(auto-save-mode)

;;; Replace the annoying yes-or-no prompt with the shorter y-or-n version
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'split-window-below 'split-window-right)
;;; (defalias 'list-buffers 'ibuffer)	; I'm going to try using list-buffers for a bit

;;(add-to-list 'default-frame-alist '(font . "xos4 Terminus-12" ))
;;(set-face-attribute 'default t :font "xos4 Terminus-12" )

(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-use-filename-at-point 'guess)

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

(defun set-key (keymap pair)
  "Binds a key:function pair too a keymap;
An example key:function pair that binds shell to F1 is (\"<f1>\" . shell)"
  (define-key keymap (kbd (car pair)) (cdr pair)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Keybindings ;;;;
;;;;;;;;;;;;;;;;;;;;;

;;; Set global keybindings
(mapcar #'(lambda (key-function-pair) (set-key global-map key-function-pair))
        '(
          ;; One key bindings for recording macros
          ("<f1>" . call-last-kbd-macro)
          ("S-<f1>" . toggle-kbd-macro-recording-on)
          ("<f2>" . eshell)
          ("M-o" . other-window)
	  ("M-<f4>" . save-buffers-kill-emacs)
          ("M-<f1>" . multi-occur-in-matching-buffers)
          ("C-x C-c" . delete-frame)    ; TODO(Gregory): Move this to the C-x map
          ))
;;; TODO(Greogry): make a C-z map!!!

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

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t))

(use-package color-theme-sanityinc-solarized
  :ensure t
  :config
  (load-theme 'sanityinc-solarized-dark t))

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
  ;; TODO(Gregory): Think about using my set-key function with the C-x map to bind these
  ("M-[" . ace-jump-word-mode)
  ("C-x M-DEL" . ace-jump-mode-pop-mark))

;;;; My new favorite plugin!!!!
(use-package god-mode
  :ensure t
  :bind
  ("C-." . god-mode-all)
  ("M-." . god-mode-all)
  :config
  ;; Copied from the git page
  (define-key god-local-mode-map (kbd "z") 'repeat)
  ;; (define-key god-local-mode-map (kbd ".") 'repeat)
  (define-key god-local-mode-map (kbd "i") 'god-local-mode)

  (defun my-update-cursor ()
    ;; "Change the look of the cursor depending on the state of god-mode"
    (setq cursor-type (if (or god-local-mode buffer-read-only)
			  (progn (set-cursor-color "red") 'box)
			(progn (set-cursor-color "white") 'bar))))
  
  (add-hook 'god-mode-enable-hook 'my-update-cursor)
  (add-hook 'god-mode-disable-hook 'my-update-cursor)
  (add-hook 'eshell-mode-hook '(lambda () (god-mode -1))) ; drop out of god mode when switching to eshell
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auto Generated Code ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
