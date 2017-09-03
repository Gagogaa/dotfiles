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
;;; TODO(Gregory): Create a funciton to change the indentation by major mode
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
(tooltip-mode -1)
(scroll-bar-mode -1)
(save-place-mode)
(electric-pair-mode)
(auto-save-mode)
(global-hl-line-mode)

;;; Replace the annoying yes-or-no prompt with the shorter y-or-n version
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'split-window-below 'split-window-right)
;;; (defalias 'list-buffers 'ibuffer)	; I'm going to try using list-buffers for a bit

(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-use-filename-at-point 'guess)

;;; (start-server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom Functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun install-use-package ()
  "Install the use-package package manager if its not installed"
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

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

;;; TODO(Greogry): make a C-z map!!!

;;; Set global keybindings
(mapcar #'(lambda (key-function-pair)
	    (set-key global-map key-function-pair))
        '(("<f1>" . call-last-kbd-macro)
          ("S-<f1>" . toggle-kbd-macro-recording-on)
          ("<f2>" . eshell)
          ("M-o" . other-window)
          ("M-<f1>" . multi-occur-in-matching-buffers)
          ("C-x C-c" . delete-frame)
	  ("C-M-{" . insert-pair)
	  ("C-M-(" . insert-pair)
	  ("C-M-[" . insert-pair)
	  ("C-M-'" . insert-pair)
	  ("C-M-\"" . insert-pair)
	  ("M-<f4>" . delete-frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Downloaded Packages ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package-initialize)

;;; Add package archives
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)

;; TODO -oGregory look into yasnippents to speed things up! Also
;; think about installing company for c and java maybe I'll have to
;; see what I'm using for classes

(install-use-package)

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
  ("M-[" . ace-jump-word-mode)
  ("C-x M-DEL" . ace-jump-mode-pop-mark))

;;;; NOTE: My new favorite plugin!!!!
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

  (defun update-cursor ()
    "Change the look of the cursor depending on the state of god-mode"
    (setq cursor-type (if (or god-local-mode buffer-read-only)
			  ;;   (progn (set-cursor-color "red") 'box)
			  ;; (progn (set-cursor-color "white") 'bar))))
			  'box
			'bar)))
  
  (add-hook 'god-mode-enable-hook 'update-cursor)
  (add-hook 'god-mode-disable-hook 'update-cursor)
  (add-hook 'eshell-mode-hook '(lambda () (god-mode -1))))

(use-package engine-mode
  :ensure t
  :config
  (engine-mode t)
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
    :keybinding "a")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine project-gutenberg
    "http://www.gutenberg.org/ebooks/search/?query=%s")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine twitter
    "https://twitter.com/search?q=%s")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")

  (defengine wiktionary
    "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s"
    :keybinding "e")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")

  ;; NOTE this is for work
  (defengine delphi-doc
    "http://docwiki.embarcadero.com/RADStudio/Berlin/en/%s"
    :keybinding "o"))

(use-package company
  :ensure t
  :config
  ;; TODO(Gregory): check out the config settings for this
  (company-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auto Generated Code ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
