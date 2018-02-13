;;;;    ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;;;;    ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;;;    █████╗  ██╔████╔██║███████║██║     ███████╗
;;;;    ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;;; ██╗███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;;; ╚═╝╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Built-In Customizations ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Change a buch of the defualt settings
(setq-default
 ;; Don't insert spaces not tabs
 indent-tabs-mode nil
 ;; Keep the tab size at 2 spaces
 tab-width 2
 ;; Turn off the aweful ring bell function
 ring-bell-function 'ignore
 delete-by-moving-to-trash t
 ;; Auto follow sym-links
 vc-follow-symlinks t
 ;; Move emacs backup files to a different directory instead of the "current directory"
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 ;; Just in case I need to enable debugging
 ;; debug-on-error t
 ;; This may speed up emacs by makeing it's garbage collector run less often
 gc-cons-threshold 50000000
 ;; I know what scratch is for
 initial-scratch-message ""
 ;; Turn off line wraping
 truncate-lines t
 ;; Customize c mode for the indentation style that I like
 c-default-style "linux"
 c-basic-offset 2
 ;; Scroll one line at a time
 ;; scroll-conservatively 10000
 ;; Don't move the cursor when scrolling
 scroll-preserve-screen-position t
 ;; Highlight tabs in whitespace mode
 whitespace-style '(trailing tabs tab-mark))

;;; Setup fonts
(set-default-font "Fira Code 9")
;;; Set a better korean font
(set-fontset-font t 'unicode "Baekmuk Dotum" nil 'prepend)

;;; Delete whitespace at the end of lines when saveing files
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Save files when tabbing out of emacs
(add-hook 'focus-out-hook '(lambda () (save-some-buffers t)))

;;; Start emacs in fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

;;; Show matching parentheses
(setq show-paren-delay 0)
(show-paren-mode t)

;;; Get rid of ui clutter
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; Show the various whitespace symbols (for tabs)
(global-whitespace-mode)

;;; Save the location in files between sessions
(save-place-mode)

;;; Insert matching symbols (insterts a closing "]" when entering "[")
(electric-pair-mode)

;;; Highlight the current line
(global-hl-line-mode)

;;; Show line numbers
;; (global-linum-mode)
;; (setq linum-format "%d ")
;; (setq linum-eager t)

;;; Move around with shift arrow-keys
;; (windmove-default-keybindings)

;;; I don't think this does anything
;; (auto-save-mode)

;;; Swap the annoying "yes or no" prompts with just "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)
;;; Switch out list-buffers with the newer ibuffer
(defalias 'list-buffers 'ibuffer)

;;; Use "interactive do" it makes menuing MUCH easier
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-use-filename-at-point 'guess)

;; Save open files and layout for the next time emacs opens
(desktop-save-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom Functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun install-use-package ()
  "Install use-package package manager if its not installed."
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(defun swap-buffers ()
  "Swaps the current buffer with the next buffer."
  (interactive)
  (let ((buffer1 (buffer-name))
        (buffer2 nil))

    (other-window 1)
    (setq buffer2 (buffer-name))
    (switch-to-buffer buffer1)
    (other-window -1)
    (switch-to-buffer buffer2)
    (other-window 1)))

(defun toggle-kbd-macro-recording ()
  "Toggles macro recording on and off with one command!"
  (interactive)
  (if defining-kbd-macro
      (end-kbd-macro)
    (start-kbd-macro nil)))

(defun scratch ()
  "Switches to the *scratch* buffer creating one if it doesn't exist."
  (interactive)
  (switch-to-buffer "*scratch*"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Keybindings ;;;;
;;;;;;;;;;;;;;;;;;;;;
(set-keys global-map
          '(("<f1>" . call-last-kbd-macro)
            ("S-<f1>" . toggle-kbd-macro-recording)
            ("<f2>" . rgrep)
            ;; ("<f2>" . multi-occur-in-matching-buffers)
            ("<f3>" . ibuffer)
            ("M-<f4>" . delete-frame)
            ;; ("<f4>" . (lambda () (interactive) (dired ".")))
            ("M-o" . other-window)
            ("M-O" . (lambda () (interactive) (other-window -1)))
            ("C-|" . split-window-right)
            ("C--" . split-window-below)
            ("C-x C-o" . swap-buffers)
            ("C-x C-k" . kill-this-buffer)
            ("M-<down>" . (lambda () (interactive) (scroll-up-line 1)))
            ("M-<up>" . (lambda () (interactive) (scroll-down-line 1)))
            ("C-x C-s". (lambda () (interactive) (message-box "No saving!")))
            ("M-n" . forward-paragraph)
            ("M-p" . backward-paragraph)
            ))

;;; Make my own keymap
(setq ctl-z-map (make-sparse-keymap))

(global-set-key (kbd "C-z") ctl-z-map)

(set-keys ctl-z-map
          '(("k" . kill-emacs)
            ("s" . scratch)
            ("w" . save-buffer)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Downloaded Packages ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package-initialize)

;;; Add in additional package archives
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)

(install-use-package)

;;; TODO: wrap the color schemes into a neater little bundle

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-tomorrow-bright t))

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t))

;; (use-package color-theme-sanityinc-solarized
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-solarized-dark t))

;;; Shows avalable commands
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; A nicer, easier to read status bar
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

;;; Incrementally select text
(use-package expand-region
  :ensure t
  :bind
  ("C-h" . er/expand-region)
  ("C-S-H" . er/contract-region))

;;; Jump to text
(use-package ace-jump-mode
  :ensure t
  :bind
  ("M-i" . ace-jump-word-mode))

;;; Model editing (kinda like vim)
(use-package god-mode
  :ensure t
  :bind
  ("M-." . god-mode-all)
  ("C-." . god-mode-all)
  :init
  (god-mode-all)
  :config

  (defun update-cursor ()
    "Change the look of the cursor depending on the state of god-mode"
    (setq cursor-type
          (if (or god-local-mode buffer-read-only)
              'box
            'bar)))

  (add-hook 'god-mode-enabled-hook 'update-cursor)
  (add-hook 'god-mode-disabled-hook 'update-cursor))

;;; Search within emacs!
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

;;; An auto completion framework
(use-package company
  :ensure t
  :config
  ;; TODO: Check out the config settings for this.
  (global-company-mode))

;;; Snippets... no more hand writing boilerplate code
(use-package yasnippet
  :ensure t
  :config
  ;; TODO: Check out the config settings for this.
  ;; TODO: Make some of my own snippets.
  ;; To add snippets due so under .emacs.d/snippets/my-mode/
  (yas-global-mode 1))

;;; A bunch of premade snippets
(use-package yasnippet-snippets
  ;; More snippets for yasnippet I should check them out!!!!
  :ensure t)

;;; Delete functions now kill more white space
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

;;; Easily wrap selected regions
(use-package wrap-region
  :ensure t
  :config
  (wrap-region-global-mode t)
  (wrap-region-add-wrappers
   '(("(" ")")
     ("[" "]")
     ("{" "}")
     ("<" ">")
     ("'" "'")
     ("\"" "\"")
     ("‘" "’"   "q")
     ("“" "”"   "Q")
     ("*" "*"   "b"   org-mode)                 ; bolden
     ("*" "*"   "*"   org-mode)                 ; bolden
     ("/" "/"   "i"   org-mode)                 ; italics
     ("/" "/"   "/"   org-mode)                 ; italics
     ("~" "~"   "c"   org-mode)                 ; code
     ("~" "~"   "~"   org-mode)                 ; code
     ("=" "="   "v"   org-mode)                 ; verbatim
     ("=" "="   "="   org-mode)                 ; verbatim
     ("_" "_"   "u" '(org-mode markdown-mode))  ; underline
     ("**" "**" "b"   markdown-mode)            ; bolden
     ("*" "*"   "i"   markdown-mode)            ; italics
     ("`" "`"   "c"   markdown-mode)            ; code
     ("`" "'"   "c"   lisp-mode)                ; code
     ))
  :diminish wrap-region-mode)

;;; Highlight keywords in comments
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

;;; For python development
;; (use-package elpy
;;   :ensure t
;;   :config
;;   (elpy-enable))

;;; For clojure development
;; (use-package cider
;;   :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auto Generated Code ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
