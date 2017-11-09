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
;;; TODO: Create a funciton to change the indentation by major mode
(setq indent-tabs-mode nil
      tab-width 2
      ring-bell-function 'ignore 	; Get rid of the bell bacause omg is it bad
      delete-by-moving-to-trash t
      inferior-lisp-program "clisp"
      vc-follow-symlinks t		; Auto follow sym-links
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      truncate-lines t
      ;; debug-on-error t			; Just in case I need to enable debugging

      c-default-style "linux"
      c-basic-offset 2)

(setq-default truncate-lines t
              c-default-style "linux"
              c-basic-offset 2)


(set-default-font "Ubuntu Mono 12")
;; (set-face-attribute 'default t :font "Ubuntu Mono" :height 120)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq show-paren-delay 0)
(show-paren-mode t)

(tool-bar-mode -1)
;; (menu-bar-mode -1) 			; Not I keep this on to check out the snippets in ya-snippets
(tooltip-mode -1)
(scroll-bar-mode -1)
(save-place-mode)
(electric-pair-mode)
(windmove-default-keybindings) ; Move around with shift arrow-keys
(global-prettify-symbols-mode +1)
;; (auto-save-mode) 			; It doesn't respect saving in another directory other than the source directory

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'split-window-below 'split-window-right)
;;; (defalias 'list-buffers 'ibuffer)	; I'm going to try using list-buffers for a bit

(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-use-filename-at-point 'guess)

(desktop-save-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom Functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun install-use-package ()
  "Install the use-package package manager if its not installed"
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;;; TODO: bundle these two up into one function.
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

(defun set-keys (keymap pairs)
  "Binds a list of keys to a keymap;
Example usage:

(set-keys global-map
	  '((\"<f1>\" . eshell)
	    (\"M-o\" . other-window)
	    (\"M-<f1>\" . multi-occur-in-matching-buffers)
	    (\"M-<f4>\" . delete-frame)))"

  (mapcar #'(lambda (key-function-pair)
              (define-key keymap
                (kbd (car key-function-pair))
                (cdr key-function-pair)))
          pairs))

(defun not-today ()
  (interactive)
  (message-box "Not Today!"))

;;; TODO: Make a stop nagging function for when I'm in a rush.
;;; Also take into account the last buffer I used save file in.
;;; and make this pop up if the files hasnt changed.
;;; Maybe if I give it an argument it should turn itself off
(setq last-time (current-time))
(defun stop-saving-so-much ()
  "Messages me when I'm saving the file way too often."
  (interactive)
  (if (< (- (time-to-seconds (current-time))
            (time-to-seconds last-time))
         60)
      (message-box "Stop saving so much!"))
  (setq last-time (current-time))
  (save-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Keybindings ;;;;
;;;;;;;;;;;;;;;;;;;;;
(set-keys global-map
	  '(("<f1>" . call-last-kbd-macro)
	    ("S-<f1>" . toggle-kbd-macro-recording-on)
	    ("<f2>" . eshell)
	    ("M-o" . other-window)
	    ("C-<f1>" . multi-occur-in-matching-buffers)
	    ("C-x C-c" . not-today)
	    ("C-x C-s" . stop-saving-so-much)
	    ("C-M-{" . insert-pair)
	    ("C-M-(" . insert-pair)
	    ("C-M-[" . insert-pair)
	    ("C-M-'" . insert-pair)
	    ("C-M-\"" . insert-pair)
	    ("M-<f4>" . delete-frame)))

(setq ctl-z-map (make-sparse-keymap))

(global-set-key (kbd "C-z") ctl-z-map)

(set-keys ctl-z-map
          '(("x" . kill-emacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Downloaded Packages ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)

(install-use-package)

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; (use-package color-theme-sanityinc-solarized
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-solarized-dark t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package expand-region
  :ensure t
  :bind
  ("M-N" . er/expand-region)
  ("M-P" . er/contract-region))

(use-package ace-jump-mode
  :ensure t
  :bind
  ("M-n" . ace-jump-word-mode)
  ("M-p" . ace-jump-mode-pop-mark))

(use-package god-mode
  :ensure t
  :bind
  ;; ("C-." . god-mode-all)
  ("M-." . god-mode-all)
  :init
  (god-mode-all)
  :config
  (define-key god-local-mode-map (kbd ".") 'repeat)

  (add-to-list 'god-exempt-major-modes 'eshell-mode)

  (defun update-cursor ()
    "Change the look of the cursor depending on the state of god-mode"
    (setq cursor-type
          (if (or god-local-mode buffer-read-only)
              'box
            'bar)))

  (add-hook 'god-mode-enabled-hook 'update-cursor)
  (add-hook 'god-mode-disabled-hook 'update-cursor))

(use-package engine-mode
  :ensure t
  :config
  (engine-mode t)

  (defun define-engines (engine-list)
    (mapcar #'(lambda (engine)
		(eval `(defengine ,(car engine)
			 ,(cadr engine)
			 :keybinding ,(cddr engine))))
  	    engine-list))

  (define-engines
    '((amazon "https://www.amazon.com/s/ref=nb_sb_noss_2/133-6164387-7931258?url=search-alias%3Daps&field-keywords=%s" . "a")
      (duckduckgo "https://duckduckgo.com/?q=%s" . "d")
      (twitter "https://twitter.com/search?q=%s" . nil)
      (github "https://github.com/search?ref=simplesearch&q=%s" . "g")
      (project-gutenberg "http://www.gutenberg.org/ebooks/search/?query=%s" . nil)
      (stack-overflow "https://stackoverflow.com/search?q=%s" . "s")
      (wikipedia "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s" . "w")
      (wiktionary "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s" . "i")
      (emacswiki "https://www.emacswiki.org/emacs/Search?action=index&match=%s" . "e")
      (youtube "http://www.youtube.com/results?aq=f&oq=&search_query=%s" . "y")
      (python-doc "https://docs.python.org/3/search.html?q=%s" . "p")
      ;; NOTE this is for work
      (delphi-doc "http://docwiki.embarcadero.com/RADStudio/Berlin/en/%s" . "o"))))

(use-package company
  :ensure t
  :config
  ;; TODO: Check out the config settings for this.
  (global-company-mode))

(use-package yasnippet
  :ensure t
  :config
  ;; TODO: Check out the config settings for this.
  ;; To add snippets due so under .emacs.d/snippets/my-mode/
  (yas-global-mode 1))

(use-package yasnippet-snippets
  ;; More snippets for yasnippet I should check them out!!!!
  :ensure t)

(use-package beacon
  :ensure t
  :config
  (beacon-mode t)
  (add-hook 'ace-jump-mode-end-hook 'beacon-blink))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auto Generated Code ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'narrow-to-region 'disabled nil)
