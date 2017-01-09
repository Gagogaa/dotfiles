;;; TODO: Embed a tweaked version of control lock for capital key bindings
;;; TODO: Consider putting this in ~/.emacs.d/init.el and backing up the whole .emacs.d folder
;;; TODO: Make comments more readable
;;; TODO: Add clipboard-yank keybinding
;;; TODO: Lookup the mark ring maybe rebind mark pop
;;; TODO: check out magit and flycheck
;;; TODO: move all this TODO garbage to an org file
;;; TODO: Look into yasnippets
;;; TODO: Check out Abbrevs package
;;; TODO: consiter rebinding capslock to CRTL for emacs
;;; DejaVu Sans Mono
;;; multi-occur-in-this-mode searches in all buffers with the same major mode

;;; Emacs standard settings
;;; TODO: consider setting a custom-file for the customize groups
(tool-bar-mode -1) ; Kill toolbar
(menu-bar-mode -1) ; Remove menubar
(setq-default tab-width 2) ; Tab width 2
(setq show-paren-delay 0)	; 0 delay for paren matching
(show-paren-mode 1)	; Show matching paren
(scroll-bar-mode -1) ; No scroll bars
(toggle-word-wrap 1) ; Better word wrapping 1
(electric-pair-mode) ; Turn on pair matching for brackets
(desktop-save-mode 1) ; save emacs sessions
(global-hl-line-mode 1) ; Highlight the current line
(setq desktop-auto-save-timeout nil) ; dont autosave files
(setq delete-by-moving-to-trash t) ; Move things to the trash instead of destorying them
(tooltip-mode -1) ; Disable tool tips 
(setq tooltip-use-echo-area t) ; Show tool tips in the mini buffer
(fset 'yes-or-no-p 'y-or-n-p) ; Replace the annoying yes-or-no prompt with y-or-n
;; (global-linum-mode 1) ; Show line numbers
;; (winner-mode 1) ; Lets you undo and redo window layout commands with C-x left/right
;; (windmove-default-keybindings) ; Lets you move around windows with SHIFT+up/right/down/left
;; (setq debug-on-error t) ; Tell emacs to debug on error

;;; Currently using swiper... rip ido
;;; TODO: check out occur it may replace swiper
;;; READ this article https://www.masteringemacs.org/article/searching-buffers-occur-mode
;;; // start at the top most Dir "/"
;;; M-d Searches sub-Dirs too
;;; M-m Creates a new sub directory
;;; C-t Toggles RegExp matching 
;;; C-p toggles Prefix matching (if on matches by beginning rather that containing)
;;; NOTE you can customize a variable with M-x customize-variable RET variable-name-here
;; (require 'ido)
;; (ido-mode t)
;; (setq ido-mode-enable-flex-matching t) ; Flexable matching
;; (setq ido-enable-regexp t)
;; (setq ido-everywhere t)
;; (setq ido-use-filename-at-point 'guess) ; Uses point to ge tcontext for file search
;; (setq ido-create-new-buffer 'always) ; Lets ido create new buffers without propmting
;; (setq ido-file-extensions-order '(".org" ".java" ".emacs")) ; Tells ido to show these file types first
;; (setq ido-ignore-extensions t) ; Tells ido to use completion-ignored-extensions variable for a list of file extensions to ignore

(defalias 'list-buffers 'ibuffer) ; switch list-buffers to ibuffer

;;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq backup-directory-alist `(("." . "~/.saves"))) ; Make a backups directory in ~/.saves

;;; Emacs exec-path
(add-to-list 'exec-path "~/.bin")
;; (add-to-list 'load-path "~/.emacs.d/") ; Where emacs looks for files to load

;;; Consiter installing keyfreq for key logging in emacs 
;;; Package
(require 'package)

;;; Add some package servers
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(package-initialize)
(setq package-enable-at-startup nil)

;;; Ensure that use-package is installed
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

;;; Try lets you use packages withoutn actually installing them
(use-package try
	:ensure t)
 
;;; init loads things before the package loads
;;; config loads things after the package is loaded
;;; You can use
;;;   :init
;;;   (progn (more) (lisp) (forms))
;;; to execute different things on package load

;;; Creates a help window for keymaps (such as C-x)
(use-package which-key
	:ensure t
	:config
	(which-key-mode))

(use-package lorem-ipsum
	:ensure t
	:config
	(lorem-ipsum-use-default-bindings))

(use-package org-bullets
	:ensure t
	:config
	(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;; required for swiper
(use-package counsel
	:ensure t)

;;; link to swiper/ivy docs http://oremacs.com/swiper/
(use-package swiper
	:ensure t
	:config
	(progn (ivy-mode 1)
				 (setq ivy-use-virtual-buffers t)
				 ;; (setq ivy-display-style 'fancy)
				 (global-set-key "\C-s" 'swiper)
				 (global-set-key (kbd "C-c C-r") 'ivy-resume)
				 (global-set-key (kbd "<f6>") 'ivy-resume)
				 (global-set-key (kbd "M-x") 'counsel-M-x)
				 (global-set-key (kbd "C-x C-f") 'counsel-find-file)
				 (global-set-key (kbd "<f1> f") 'counsel-describe-function)
				 (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
				 (global-set-key (kbd "<f1> l") 'counsel-find-library)
				 (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
				 (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
				 (global-set-key (kbd "C-c g") 'counsel-git)
				 (global-set-key (kbd "C-c j") 'counsel-git-grep)
				 (global-set-key (kbd "C-c k") 'counsel-ag)
				 (global-set-key (kbd "C-x l") 'counsel-locate)
				 ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
				 (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

;;; TODO: see how to cancel control-lock when active
(use-package avy
	:ensure t
	:bind ("M-s" . avy-goto-word-1))

(use-package color-theme-sanityinc-solarized
	:ensure t)

(use-package emmet-mode
	:ensure t
	:config
	(progn
		(add-hook 'sgml-mode-hook 'emmet-mode) ; markup langs
		(add-hook 'html-mode-hook 'emmet-mode)
		(add-hook 'css-mode-hook 'emmet-mode)))

;;; Depricated do not use! 
;; (ensure-packages-installed
 ;; 'helm
 ;; 'clojure-mode
 ;; 'cider
 ;; 'smex
 ;; 'company
 ;; 'auto-complete
 ;; 'markdown-mode
 ;; 'flycheck
 ;; )

;;; Fix a bug with cider
;; (add-hook 'clojure-mode-hook #'cider-mode)

;; ;;; Smex keybindings
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;; My keybindings
;;; see this article for more info https://www.masteringemacs.org/article/mastering-key-bindings-emacs
;;; rebind C-S-<up/down/left/right> to resize windows
;;; renind <F1> to eshell
;;; note C-<return> is unbound in most modes
;;; Consiter swaping C-h, C-p
;;; maybe bind revert-buffer to something
;;; Super can be refered to in kbd as "s-?" ? being any key (NOTE the s is lowercase)
;;; Hyper can be refered to in kbd as "H-?" ^^
;;; TODO: bind swiper-all

;;; Super and hyper key bindings for windows
;; (setq w32-apps-modifier 'hyper)
;; (setq w32-lwindow-modifier 'super)
;; (setq w32-rwindow-modifier 'hyper)

;; (global-set-key (kbd "<escape>") 'control-lock-toggle)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-o") 'other-window)
;; (global-set-key (kbd "C-p") 'help-command)
;; (global-set-key (kbd "C-h") 'previous-line)
(global-set-key (kbd "<f1>") 'eshell)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "C-,") 'control-lock-toggle)
(define-key global-map (kbd "RET") 'newline-and-indent)

;;; TODO: de-ugly below this line
; ------------------------------------------------------------------------------

;;; Color Scheme stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
	 [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
	 ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(cursor-type (quote hollow))
 '(custom-enabled-themes (quote (sanityinc-solarized-light)))
 '(custom-safe-themes
	 (quote
		("80a2554792f7c6eadbc6abe9e7fe3db4a9710b5e052a5c309e1ca428adb38cd9" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(eww-restore-desktop t)
 '(fci-rule-color "#37474f")
 '(hl-sexp-background-color "#1c1f26")
 '(ivy-mode t)
 '(ivy-use-virtual-buffers t)
 '(package-selected-packages
	 (quote
		(ahungry-theme avy counsel ace-window tabbar which-key 2048-game try use-package lorem-ipsum swiper ivy material-theme flycheck auto-complete company peacock-theme cl-lib markdown-mode helm-smex smex control-lock hc-zenburn-theme gruvbox-theme grandshell-theme gotham-theme flatland-theme smooth-scrolling persistent-soft org-bullets mic-paren color-theme-sanityinc-solarized ## helm)))
 '(ring-bell-function (quote ignore))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
	 (quote
		((20 . "#f36c60")
		 (40 . "#ff9800")
		 (60 . "#fff59d")
		 (80 . "#8bc34a")
		 (100 . "#81d4fa")
		 (120 . "#4dd0e1")
		 (140 . "#b39ddb")
		 (160 . "#f36c60")
		 (180 . "#ff9800")
		 (200 . "#fff59d")
		 (220 . "#8bc34a")
		 (240 . "#81d4fa")
		 (260 . "#4dd0e1")
		 (280 . "#b39ddb")
		 (300 . "#f36c60")
		 (320 . "#ff9800")
		 (340 . "#fff59d")
		 (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))

;;; I think these next two bits we're for a terminal color scheme fix 
(defun on-after-init ()
  (set-face-background 'default "unspecified-bg" (selected-frame)))

(if (not (window-system)) (add-hook 'window-setup-hook 'on-after-init))

;;; A snippet to deal with emacs tab nonsense
;;; TODO: rewrite this for a better understanding of emacs and its tab nonsense
;;; http://blog.binchen.org/posts/easy-indentation-setup-in-emacs-for-web-development.html
(defun my-setup-indent (n)
	;; java/c/c++
	(setq-local c-basic-offset n)
	;; web development
	(setq-local coffee-tab-width n) ; coffeescript
	(setq-local javascript-indent-level n) ; javascript-mode
	(setq-local js-indent-level n) ; js-mode
	(setq-local js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
	(setq-local web-mode-markup-indent-offset n) ; web-mode, html tag in html file
	(setq-local web-mode-css-indent-offset n) ; web-mode, css in html file
	(setq-local web-mode-code-indent-offset n) ; web-mode, js code in html file
	(setq-local css-indent-offset n) ; css-mode
	)

(defun my-office-code-style ()
	(interactive)
	(message "Office code style!")
	;; use tab instead of space
	(setq-local indent-tabs-mode t)
	;; indent 2 spaces width
	(my-setup-indent 2))

(defun my-personal-code-style ()
	(interactive)
	(message "My personal code style!")
	;; use space instead of tab
	(setq indent-tabs-mode t)
	;; indent 2 spaces width
	(my-setup-indent 2))

;;; prog-mode-hook requires emacs24+
(add-hook 'prog-mode-hook 'my-personal-code-style)
;;; a few major-modes does NOT inherited from prog-mode
(add-hook 'lua-mode-hook 'my-personal-code-style)
(add-hook 'web-mode-hook 'my-personal-code-style)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:inherit lazy-highlight))))
 '(avy-lead-face-0 ((t (:inherit lazy-highlight))))
 '(avy-lead-face-2 ((t (:inherit lazy-highlight))))
 '(ivy-current-match ((t (:inherit isearch))))
 '(ivy-minibuffer-match-face-1 ((t (:inherit secondary-selection))))
 '(ivy-minibuffer-match-face-2 ((t (:inherit highlight))))
 '(ivy-minibuffer-match-face-3 ((t (:inherit isearch))))
 '(ivy-minibuffer-match-face-4 ((t (:inherit secondary-selection)))))

;;; Must be used after loading of color scheme
(use-package control-lock
	:ensure t
	:config
	(progn; (control-lock-keys)
				 (control-lock-toggle)))

