;;; TODO: embed a tweaked version of control lock
;;; Settings
(tool-bar-mode -1)	; Kill toolbar
(setq-default tab-width 2)	; Tab width 2
(setq show-paren-delay 0)	; 0 delay for paren matching 
(show-paren-mode 1)	; Show matching parens 
(scroll-bar-mode -1)	; No scrollbars 
;(setq debug-on-error t) ; Tell emacs to debug on error

;;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq backup-directory-alist `(("." . "~/.saves"))) ; Make a backups directory in ~/.saves

;;; Emacs exec-path
(add-to-list 'exec-path "~/.bin")

;;; Pakcage
(require 'package)

;;; Add some package servers
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;;; install packages
(defun ensure-packages-installed (&rest package-list)
	;; TODO write a better docstring
	"Install missing packages"
	(mapcar
	 (lambda (package)
		 (if (package-installed-p package) nil
			 (package-install package)))
	 package-list))

;;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;;; Activate installed packages
(package-initialize)
(ensure-packages-installed
 'helm
 'color-theme-sanityinc-solarized
 ;; 'emmet-mode
 'org-bullets
 ;; 'clojure-mode
 ;; 'cider
 'smooth-scrolling
 'control-lock
 'smex
 'ido
 )

(require 'smex)
(smex-initialize)

;;; // start at the top most Dir "/"
;;; M-d Searches sub-Dirs too
;;; M-m Creates a new sub directory
;;; C-t Toggles RegExp matching 
;;; C-p toggles Prefix matching (if on matches by beginning rather that containing)
;;; NOTE you can customize a variable with M-x customize-variable RET variable-name-here
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-enable-regexp t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess) ; Uses point to ge tcontext for file search
(setq ido-create-new-buffer 'always) ; Lets ido create new buffers without propmting
(setq ido-file-extensions-order '(".org" ".java" ".emacs")) ; Tells ido to show these file types first
(setq ido-ignore-extensions t) ; Tells ido to use completion-ignored-extensions variable for a list of file extensions to ignore

;;; M-p enables and disables control-lock 
(require 'control-lock)
(control-lock-keys)

(require 'helm-config)

;;; Fix a bug with cider
;; (add-hook 'clojure-mode-hook #'cider-mode)

;;; emmet
;; (require 'emmet-mode)
;; (add-hook 'sgml-mode-hook 'emmet-mode) ; markup langs
;; (add-hook 'html-mode-hook 'emmet-mode)
;; (add-hook 'css-mode-hook 'emmet-mode)

;;; Org mode
(add-hook 'org-mode-hook 'org-bullets-mode)

;;; Custom mode for keeping my keybindings
;;; http://stackoverflow.com/questions/12905017/rebinding-keys-in-orgmode#12905328
(defvar custom-keys-mode-map (make-keymap) "custom-keys-mode keymap.")
(define-minor-mode custom-keys-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'custom-keys-mode-map)
(custom-keys-mode 1)

(defun my-minibuffer-setup-hook ()
  (custom-keys-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'custom-keys-mode))
      (let ((mykeys (assq 'custom-keys-mode minor-mode-map-alist)))
        (assq-delete-all 'custom-keys-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;;; Macros

;;; I think its about time for the great rebinding!
;;; TODO: make the key maps a list of key function pairs
;;;       the write functions to map the keys
;;; Something like this maybe

;;; A possible list for the key bindings
(setq key-list '(
	("<escape>" control-lock-toggle) ; Toggle control-lock
	("C-n" help-map)
	("C-p" recenter-top-bottom)
	("C-h" backward-word)
	("C-j" next-line)
	("C-k" previous-line)
	("C-l" forward-word)
	("C-S-h" backward-char)
	("C-S-j" forward-sentence)
	("C-S-k" backward-sentence)
	("C-S-l" forward-char)
	("M-h" back-to-indentation) ;'move-beginning-of-line)
	("M-j" end-of-defun) ;(lambda () (interactive) (electric-newline-and-maybe-indent)))
	("M-k" beginning-of-defun)
	("M-l" move-end-of-line)
	("C-f" kill-word)
	("M-f" kill-line)
	("C-S-f" delete-char)
	("C-a" backward-kill-word)
	("M-a" (lambda () (interactive) (kill-line 0) (indent-according-to-mode)))
	("C-S-a" backward-delete-char-untabify)
	))

;;; Globaly map key bindings
;; (mapcar
;;  (lambda (key)
;; 		(global-set-key (kbd (car key)) (cadr key)))
;;  key-list)

;; ;;; Map org mode key bindings
;; (eval-after-load "org"
;; 	'(mapcar
;; 		(lambda (key)
;; 			(define-key org-mode-map (kbd (car key)) (cadr key)))
;; 		key-list))

(mapcar
	(lambda (key)
		(define-key custom-keys-mode-map (kbd (car key)) (cadr key)))
	key-list)


;;; Smex keybindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

; ------------------------------------------------------------------------------

;;; Color Scheme stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
	 ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(cursor-type (quote box))
 '(custom-enabled-themes (quote (sanityinc-solarized-light)))
 '(custom-safe-themes
	 (quote
		("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(package-selected-packages
	 (quote
		(helm-smex smex control-lock hc-zenburn-theme gruvbox-theme grandshell-theme gotham-theme flatland-theme smooth-scrolling persistent-soft org-bullets mic-paren color-theme-sanityinc-solarized ## helm)))
 '(ring-bell-function (quote ignore)))

(defun on-after-init ()
  (set-face-background 'default "unspecified-bg" (selected-frame)))

(if (not (window-system)) (add-hook 'window-setup-hook 'on-after-init))

;;; A snippet to deal with emacs tab nonsense
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

