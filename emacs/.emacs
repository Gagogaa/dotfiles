;;;;     8888888888 888b     d888        d8888  .d8888b.   .d8888b.
;;;;     888        8888b   d8888       d88888 d88P  Y88b d88P  Y88b
;;;;     888        88888b.d88888      d88P888 888    888 Y88b.
;;;;     8888888    888Y88888P888     d88P 888 888         "Y888b.
;;;;     888        888 Y888P 888    d88P  888 888            "Y88b.
;;;;     888        888  Y8P  888   d88P   888 888    888       "888
;;;; d8b 888        888   "   888  d8888888888 Y88b  d88P Y88b  d88P
;;;; Y8P 8888888888 888       888 d88P     888  "Y8888P"   "Y8888P"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Awesome Emacs Blogs and Resources ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq Awesome-Emacs-Sources
      '("http://sachachua.com/blog/"
        "https://www.masteringemacs.org/"
        "https://www.emacswiki.org/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Built-In Customizations ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Change a buch of the default settings
(setq-default
 indent-tabs-mode nil                    ; Insert spaces not tabs
 c-default-style "bsd"                   ; Customize c mode for the indentation style that I like
 c-basic-offset 4                        ; Set c indentation width
 tab-width 4                             ; Set tab size to 4 spaces
 ring-bell-function 'ignore              ; Turn off the awful bell
 delete-by-moving-to-trash t             ; Move files to trash instead of deleting them
 vc-follow-symlinks t                    ; Auto follow sym-links
 gc-cons-threshold 50000000              ; Speed up Emacs by making it's garbage collector run less often
 initial-scratch-message ""              ; Remove the default message from the scratch buffer
 truncate-lines t                        ; Turn off line wrapping
 ;; TODO do a platform check and change this accordingly
 terminal-command "gnome-terminal"       ; Default terminal emulator
 echo-keystrokes 0                       ; Don't show keystrokes in the minibuffer
 python-shell-interpreter "python3"      ; Set the python interpreter to python3
 inferior-lisp-program "chezscheme"      ; Set the lisp interpreter to sbcl
 inhibit-startup-echo-area-message t     ; Don't display a startup message
 extended-command-suggest-shorter nil    ; Don't suggest shorter commands
 next-line-add-newlines t                ; Add newlines when moveing to the end of file
 browse-url-generic-program "xdg-open"   ; Use xdg-open to determine what program to open files with
 font "Agave 11"                     ; Set the font family and size
 frame-title-format "Emacs 📝"           ; Set the title of the emacs frame
 inhibit-startup-screen t                ; Stop the default emacs startup screen
 diff-font-lock-prettify t               ; Better looking diffs
 warning-minimum-level :emergency        ; Set the warning level to something less pedantic

 ;; Move emacs backup and autosave files to the system temporary directory instead of the current working directory
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))

 ;; Switch to enable debugging in case of configuration issues
 ;; debug-on-error t
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; UI-Settings ;;;;
;;;;;;;;;;;;;;;;;;;;;

;;; Get rid of UI clutter
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; Stop the blinking cursor
(blink-cursor-mode -1)

;;; Setup fonts
;(set-default-font font)     ; Set the default font for the first frame
(set-face-attribute 'default nil :font font) ; Set the default front for future frames

;;; Show matching parentheses
(setq show-paren-delay 0)
(show-paren-mode t)

;;; Highlight the current line
;; (global-hl-line-mode)

;;; Move cursor by camelCase
(global-subword-mode 1)

;;; Turn on ido mode for better completions
;; (ido-mode)

;;; Display line numbers
(global-display-line-numbers-mode)

;;; Enable pixel perfect mouse scrolling
(pixel-scroll-precision-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Convenance ;;;;
;;;;;;;;;;;;;;;;;;;;

;; TODO get something to deal with mode specific configurations
;;; Change to object pascal mode when working with pascal files
(add-hook 'pascal-mode-hook 'opascal-mode)
(add-hook 'opascal-mode-hook '(lambda () (setq opascal-indent-level 2))) ; Set indentation level to two spaces

;;; Remove whitespace from line ends when saving files
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

;;; Save files when tabbing out of emacs
(add-hook 'focus-out-hook '(lambda () (save-some-buffers t)))

;;; Refresh buffer if the corresponding file changes on disk
(auto-revert-mode)

;;; Save the location in files between sessions
(save-place-mode)

;;; Insert matching symbols so inserting "[" will automatically instert a closing "]"
(electric-pair-mode)

;;; Replace the annoying "yes or no" prompts with just "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)
;;; Switch out list-buffers with the newer ibuffer
(defalias 'list-buffers 'ibuffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom Functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun install-use-package ()
  "Install use-package package manager if its not installed."
  (unless (package-installed-p 'use-package)
    (package-install 'use-package)))

(defun transpose-windows ()
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
  "Toggles macro recording on and off with one command!;
Example:

;;; F1 plays the keyboard macro.
(global-set-key (kbd \"<f1>\") 'call-last-kbd-macro)

;;; Shift F1 toggles macro recording.
(global-set-key (kbd \"S-<f1>\") 'toggle-kbd-macro-recording)"
  (interactive)

  (if defining-kbd-macro
      (end-kbd-macro)
    (start-kbd-macro nil)))

(defun set-keys (keymap pairs)
  "Binds a list of keys to a keymap
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

;;; Open multiple marked files dired
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

(defun eshell-clear-buffer ()
  "Clear eshell terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          '(lambda()
             (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

;;; TODO take a look into these functions to see if I want to keep them or not
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

;; (define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Keybindings ;;;;
;;;;;;;;;;;;;;;;;;;;;

;;; Change keys in the global map
;;; TODO In the lambda functions only the first function is called not the other functions...
(set-keys global-map
          '(("<f1>" . call-last-kbd-macro)
            ("S-<f1>" . toggle-kbd-macro-recording)
            ("<f2>" . projectile-ripgrep)
            ("<f3>" . calc)
            ;; TODO Look into dired+
            ("<f4>" . (lambda () (interactive) (dired ".")))
            ("M-<f4>" . delete-frame)
            ;; TODO stop the popup window thats happening on this
            ("<f5>" . (lambda () (interactive) (shell-command terminal-command)))
            ("<f7>" . run-python)
            ("M-o" . other-window)
            ("M-O" . (lambda () (interactive) (other-window -1)))
            ("C-|" . (lambda () (interactive) (split-window-right) (balance-windows)))
            ("C--" . (lambda () (interactive) (split-window-below) (balance-windows)))
            ("C-M-<backspace>" . (lambda () (interactive) (delete-window) (balance-windows)))
            ("C-x C-o" . transpose-windows)
            ("C-x C-k" . kill-this-buffer)
            ("C-M-o" . ff-find-other-file)
            ("C-`" . push-mark-no-activate)
            ))

;;; Make my own keymap
(setq ctl-z-map (make-sparse-keymap))
(global-set-key (kbd "C-z") ctl-z-map)
(set-keys ctl-z-map
          '(("k" . kill-emacs)
            ("s" . (lambda () (interactive) (switch-to-buffer "*scratch*")))
            ("e" . (lambda () (interactive) (find-file user-init-file)))
            ("p" . package-list-packages)
            ("a" . align-regexp)
            ("h" . eshell)
            ("l" . view-lossage)
            ("b" . list-abbrevs)
            ("f" . find-file-at-point)
            ("o" . other-frame)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Downloaded Packages ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Add in additional package archives
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)

;; (package-refresh-contents)
;; (install-use-package)

(use-package moe-theme
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(use-package color-theme-sanityinc-solarized
  :ensure t)

(use-package zenburn-theme
  :ensure t)

(if window-system
    (load-theme 'sanityinc-solarized-dark t))

(use-package abbrev
  :diminish abbrev-mode
  :config
  ;; Enable and configure abbreviations checkout the emacs wiki for more info!
  ;; https://www.emacswiki.org/emacs/AbbrevMode
  (setq-default
   abbrev-mode t
   save-abbrevs 'silent
   abbrev-file-name (concat user-emacs-directory ".abbrev-file")))

;;; Model editing (kinda like vim)
(use-package god-mode
  :ensure t
  :diminish god-local-mode
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

;;; Incrementally select text
(use-package expand-region
  :ensure t
  :bind
  ("C-;" . er/expand-region)
  ("C-:" . er/contract-region))

;;; Easily wrap selected regions
;; (use-package wrap-region
;;   :ensure t
;;   :diminish wrap-region-mode
;;   :config
;;   (wrap-region-global-mode t)
;;   (wrap-region-add-wrappers
;;    '(("(" ")")
;;      ("[" "]")
;;      ("{" "}")
;;      ("<" ">")
;;      ("'" "'")
;;      ("\"" "\"")
;;      ("‘" "’"   "q")
;;      ("“" "”"   "Q")
;;      ("_" "_"   "u"   markdown-mode)  ; underline
;;      ("**" "**" "b"   markdown-mode)  ; bolden
;;      ("*" "*"   "i"   markdown-mode)  ; italics
;;      ("`" "`"   "c"   markdown-mode)  ; code
;;      ("begin\n" "\nend;\n" "b" opascal-mode)
;;      )))

;;; Jump to text
(use-package ace-jump-mode
  :ensure t
  :bind
  ("M-i" . ace-jump-word-mode))

;;; An auto completion framework
(use-package company
  :ensure t
  :bind
  ("C-," . company-complete)
  :config
  (global-company-mode))

;;; Amazing git integration for emacs
(use-package magit
  :ensure t
  :bind
  ("<f10>" . magit))

;;; Toolbox for operating on projects
(use-package projectile
  :ensure t
  :config

  (use-package projectile-ripgrep
    :ensure t)

  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;;; Show available functions / keys at the bottom of the screen
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

;; ;;; Display a better status bar
;; (use-package powerline
;;   :ensure t
;;   :config
;;   (setq powerline-default-separator 'wave)
;;   (powerline-default-theme))

;;; Trying a modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;;; Remove active modes from the status bar so it't not so cluttered
(use-package diminish
  :ensure t
  :config
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode))

;;; Delete all whitespace with one command
(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; TODO configure this
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (use-package yasnippet-snippets
    :ensure t))

;; (use-package yaml-mode
;;   :ensure t)

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

;; TODO Take a good look at helm mode. It seems to be a very powerfull tool
;; (use-package helm
;;   :ensure t
;;   :bind
;;   ("M-x" . helm-M-x)
;;   ("C-x b" . helm-mini)
;;   ("C-x C-f" . helm-find-files)
;;   :diminish helm-mode
;;   :config
;;   (require 'helm-config)
;;   (helm-mode 1)
;;   (setq helm-buffers-fuzzy-matching t
;;         helm-recentf-fuzzy-match    t))

(use-package python
  :ensure t)


(use-package vertico
  :ensure t
  :config
  (vertico-mode))

;; (use-package pdf-tools
;;   :config
;;   (pdf-tools-install))

;; (use-package unicode-fonts
;;   :ensure t
;;   :config
;;   (unicode-fonts-setup))

;; (use-package dumb-jump
;;   :ensure t
;;   :config
;;   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;; Load my startup file as the first buffer
(find-file "~/Sync")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Abbrev Mode ;;;;
;;;;;;;;;;;;;;;;;;;;;

(define-abbrev-table 'opascal-mode-abbrev-table '(
                                                  ("todo" "TODO -oGregory" nil 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auto Generated Code ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
