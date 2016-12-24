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

;;; Check to see if packages are installed
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
  Return a list of installed packages or nil for every skipped package."
  (mapcar 
    (lambda (package)
      (if (package-installed-p package)
	nil
	(if (y-or-n-p (format "Package %s is missing. Install it? " package))
	  (package-install package)
	  package)))
    packages))

;;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;;; Activate installed packages
(package-initialize)
(ensure-package-installed
	'helm
	'color-theme-sanityinc-solarized
	;'emmet-mode
	'org-bullets
	;'clojure-mode
	;'cider
	'smooth-scrolling
	'control-lock
  ;'control-mode
	)

;(require 'control-mode)
;(control-mode-default-setup)

;M-p enables and disables control-lock 
(require 'control-lock)
(control-lock-keys)

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

(require 'helm-config)

;;; Fix a bug with cider
;(add-hook 'clojure-mode-hook #'cider-mode)

;;; emmet
;(require 'emmet-mode)
;(add-hook 'sgml-mode-hook 'emmet-mode) ; markup langs
;(add-hook 'html-mode-hook 'emmet-mode)
;(add-hook 'css-mode-hook 'emmet-mode)

;;; Org mode
(add-hook 'org-mode-hook 'org-bullets-mode)

;;; I think its about time for the great rebinding!
(global-set-key (kbd "<escape>") 'control-lock-toggle)

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
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
	 (quote
		("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(package-selected-packages
	 (quote
		(control-lock hc-zenburn-theme gruvbox-theme grandshell-theme gotham-theme flatland-theme smooth-scrolling persistent-soft org-bullets mic-paren color-theme-sanityinc-solarized ## helm)))
 '(ring-bell-function (quote ignore)))

(defun on-after-init ()
  (set-face-background 'default "unspecified-bg" (selected-frame)))

(if (not (window-system)) (add-hook 'window-setup-hook 'on-after-init))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-current-line-face ((t (:background "#073642")))))

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

