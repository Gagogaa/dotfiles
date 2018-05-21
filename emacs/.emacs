;;;;     8888888888 888b     d888        d8888  .d8888b.   .d8888b.  
;;;;     888        8888b   d8888       d88888 d88P  Y88b d88P  Y88b 
;;;;     888        88888b.d88888      d88P888 888    888 Y88b.      
;;;;     8888888    888Y88888P888     d88P 888 888         "Y888b.   
;;;;     888        888 Y888P 888    d88P  888 888            "Y88b. 
;;;;     888        888  Y8P  888   d88P   888 888    888       "888 
;;;; d8b 888        888   "   888  d8888888888 Y88b  d88P Y88b  d88P 
;;;; Y8P 8888888888 888       888 d88P     888  "Y8888P"   "Y8888P"  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Built-In Customizations ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Change a buch of the defualt settings
(setq-default
 indent-tabs-mode nil                   ; Insert spaces not tabs
 tab-width 4                            ; Set tab size to 4 spaces
 ring-bell-function 'ignore             ; Turn off the aweful bell
 delete-by-moving-to-trash t            ; Move files to trash instead of deleting them
 vc-follow-symlinks t                   ; Auto follow sym-links
 gc-cons-threshold 50000000             ; Speed up emacs by makeing it's garbage collector run less often
 initial-scratch-message ""             ; I know what scratch is for
 truncate-lines t                       ; Turn off line wrapping
 c-default-style "linux"                ; Customize c mode for the indentation style that I like
 c-basic-offset 2                       ; Set c indentation width
 whitespace-style '(tabs tab-mark)      ; Highlight only tabs in whitespace mode
 terminal "gnome-terminal"              ; Default terminal emulator
 echo-keystrokes 0                      ; Don't show keystrokes in the minibuffer
 ;; Move emacs backup files to a different directory instead of the current directory
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 ;; For use later when using save-desktop-mode
 desktop-dirname user-emacs-directory
 desktop-base-file-name ".emacs.desktop"
 desktop-base-lock-name (concat desktop-dirname desktop-base-file-name ".lock")
 ;; Just in case I need to enable debugging
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

;;; Setup fonts
(set-default-font "Fira Code 10")
;;; Set a better korean font
(set-fontset-font t 'unicode "Baekmuk Dotum" nil 'prepend)

;;; Start emacs in fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

;;; Show matching parentheses
(setq show-paren-delay 0)
(show-paren-mode t)

;;; Highlight the current line
(global-hl-line-mode)

;;; Stop blinking the cursor
(blink-cursor-mode -1)

;;; Show battery status in the menu bar
(display-battery-mode t)

;;; Show column numbers
(column-number-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Convenance ;;;;
;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'pascal-mode-hook 'opascal-mode)
;; (add-hook 'opascal-mode-hook '(lambda () (setq opascal-indent-level 2)))

;;; Save files when tabbing out of emacs
(add-hook 'focus-out-hook '(lambda () (save-some-buffers t)))

;;; Refresh buffer if file changes on disk
(auto-revert-mode)

;;; Save the location in files between sessions
(save-place-mode)

;;; Insert matching symbols (inserts a closing "]" when entering "[")
(electric-pair-mode)

;; Save open files and layout for the next time emacs open
(unless (file-exists-p desktop-base-lock-name)
  (desktop-save-mode t))

;;; Replace the annoying "yes or no" prompts with just "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)
;;; Switch out list-buffers with the newer ibuffer
(defalias 'list-buffers 'ibuffer)

;;; Use "interactive do" it makes menuing MUCH easier
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-use-filename-at-point 'guess)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom Functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun install-use-package ()
  "Install use-package package manager if its not installed."
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Keybindings ;;;;
;;;;;;;;;;;;;;;;;;;;;

;; TODO Rename the current buffer and its visiting file if any.

;;; Change keys in the global map
(set-keys global-map
          '(("<f1>" . call-last-kbd-macro)
            ("S-<f1>" . toggle-kbd-macro-recording)
            ("<f2>" . multi-occur-in-matching-buffers)
            ("<f3>" . calc)
            ;; Look into dired+
            ("<f4>" . (lambda () (interactive) (dired ".")))
            ("M-<f4>" . delete-frame)
            ("<f5>" . (lambda () (interactive) (shell-command terminal)))
            ("M-o" . other-window)
            ("M-O" . (lambda () (interactive) (other-window -1)))
            ("C-|" . (lambda () (interactive) (split-window-right) (balance-windows)))
            ("C--" . (lambda () (interactive) (split-window-below) (balance-windows)))
            ("C-M-<backspace>" . (lambda () (interactive) (delete-window) (balance-windows)))
            ("C-x C-o" . transpose-windows)
            ("C-x C-k" . kill-this-buffer)
            ("C-S-k" . (lambda () (interactive) (move-beginning-of-line nil) (kill-line 1)))
            ;; ("M-n" . forward-paragraph)
            ;; ("M-p" . backward-paragraph)
            ("M-n" .(lambda () (interactive) (next-line 10)))
            ("M-p" .(lambda () (interactive) (previous-line 10)))
            ("C-}" . next-buffer)
            ("C-{" . previous-buffer)
            ))

;;; Make my own keymap
(setq ctl-z-map (make-sparse-keymap))

(global-set-key (kbd "C-z") ctl-z-map)

(set-keys ctl-z-map
          '(("k" . kill-emacs)
            ("s" . (lambda () (interactive) (switch-to-buffer "*scratch*")))
            ("w" . save-buffer)
            ("e" . (lambda () (interactive) (find-file user-init-file)))
            ("p" . package-list-packages)
            ("a" . align-regexp)
            ("h" . eshell)
            ("l" . view-lossage)
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

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t))

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-tomorrow-bright t))

;; (use-package color-theme-sanityinc-solarized
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-solarized-dark t))

;; (use-package dracula-theme
;;   :ensure t
;;   :config
;;   (load-theme 'dracula t))


(use-package darktooth-theme
  :ensure t
  :config
  (load-theme 'darktooth t))


;;; Incrementally select text
(use-package expand-region
  :ensure t
  :bind
  ("C-h" . er/expand-region)
  ("C-S-H" . er/contract-region))

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

;;; Snippets... no more hand writing boilerplate code
(use-package yasnippet
  :ensure t
  :config
  ;; To add snippets due so under .emacs.d/snippets/my-mode/
  (yas-global-mode 1))

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
     ("_" "_"   "u" markdown-mode)  ; underline
     ("**" "**" "b" markdown-mode)  ; bolden
     ("*" "*"   "i" markdown-mode)  ; italics
     ("`" "`"   "c" markdown-mode)  ; code
     )))

;;; A really awesome plugin for editing multiple files at the same time
(use-package multifiles
  :ensure t
  :bind ("C-!" . mf/mirror-region-in-multifile))

(use-package rust-mode
  :ensure t)

;;; An auto completion framework
(use-package company
  :ensure t
  :bind
  ("C-," . company-complete)
  :config
  ;; TODO configure this so the popup only happens when I trigger it
  (global-company-mode))

;;; Jump to text
(use-package ace-jump-mode
  :ensure t
  :bind
  ("M-i" . ace-jump-word-mode))

;;; Vim like code folding
(use-package vimish-fold
  :ensure t
  :bind
  ("C-z v f" . vimish-fold)
  ("C-z v u" . vimish-fold-unfold)
  ("C-z v U" . vimish-fold-unfold-all)
  ("C-z v d" . vimish-fold-delete)
  ("C-z v D" . vimish-fold-delete-all)
  ("C-z v r" . vimish-fold-refold)
  ("C-z v R" . vimish-fold-refold-all)
  ("C-z v t" . vimish-fold-toggle)
  ("C-z v T" . vimish-fold-toggle-all))

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  :bind
  ("C-z m m"       . mc/edit-lines)
  ("C->"           . mc/mark-next-like-this)
  ("C-<"           . mc/mark-previous-like-this)
  ("C-c C-<"       . mc/mark-all-like-this)
  ("C-S-<mouse-1>" . mc/add-cursor-on-click)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auto Generated Code ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
