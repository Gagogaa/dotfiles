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
      '("http://ergoemacs.org/emacs/emacs.html"
        "http://sachachua.com/blog/"
        "https://www.masteringemacs.org/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Built-In Customizations ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Change a buch of the default settings
(setq-default
 indent-tabs-mode nil                   ; Insert spaces not tabs
 tab-width 4                            ; Set tab size to 4 spaces
 ring-bell-function 'ignore             ; Turn off the aweful bell
 delete-by-moving-to-trash t            ; Move files to trash instead of deleting them
 vc-follow-symlinks t                   ; Auto follow sym-links
 gc-cons-threshold 50000000             ; Speed up emacs by makeing it's garbage collector run less often
 initial-scratch-message ""             ; I know what scratch is for
 truncate-lines t                       ; Turn off line wrapping
 c-default-style "bsd"                  ; Customize c mode for the indentation style that I like
 c-basic-offset 4                       ; Set c indentation width
 terminal-command "gnome-terminal"      ; Default terminal emulator
 echo-keystrokes 0                      ; Don't show keystrokes in the minibuffer
 ;; Move emacs backup files to a different directory instead of the current directory
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 ;; Just in case I need to enable debugging
 ;; debug-on-error t
 python-shell-interpreter "python3"
 abbrev-mode t
 abbrev-file-name (concat user-emacs-directory ".abbrev-file")
 save-abbrevs 'silent
 inhibit-startup-echo-area-message t    ; No startup message
 extended-command-suggest-shorter nil   ; Don't suggest shorter commands
 inferior-lisp-program "sbcl"
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
(set-default-font "Ubuntu Mono 9")
;; (set-default-font "InconsolataGo 7")
;;; Set a better korean font
(set-fontset-font t 'unicode "Baekmuk Dotum" nil 'prepend)
(set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)

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
;; (display-battery-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Convenance ;;;;
;;;;;;;;;;;;;;;;;;;;

(add-hook 'pascal-mode-hook 'opascal-mode)
(add-hook 'opascal-mode-hook '(lambda () (setq opascal-indent-level 2)))

;;; Remove whitespace from line ends when saving files
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Save files when tabbing out of emacs
(add-hook 'focus-out-hook '(lambda () (save-some-buffers t)))

;;; Refresh buffer if file changes on disk
(auto-revert-mode)

;;; Save the location in files between sessions
(save-place-mode)

;;; Insert matching symbols (inserts a closing "]" when entering "[")
(electric-pair-mode)

;;; Replace the annoying "yes or no" prompts with just "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)
;;; Switch out list-buffers with the newer ibuffer
(defalias 'list-buffers 'ibuffer)
;;; Only split windows right
(defalias 'split-window-below 'split-window-right)

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

;;; Testing this one out for clearing eshell
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          '(lambda()
             (local-set-key (kbd "C-l") 'eshell-clear-buffer)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Keybindings ;;;;
;;;;;;;;;;;;;;;;;;;;;

;; TODO Rename the current buffer and its visiting file if any.

;;; Change keys in the global map
(set-keys global-map
          '(("<f1>" . call-last-kbd-macro)
            ("S-<f1>" . toggle-kbd-macro-recording)
            ("<f2>" . multi-occur-in-matching-buffers) ;; This is replaced by projectile!
            ("<f3>" . calc)
            ;; Look into dired+
            ("<f4>" . (lambda () (interactive) (dired ".")))
            ("M-<f4>" . delete-frame)
            ("<f5>" . (lambda () (interactive) (shell-command terminal-command)))
            ("<f7>" . run-python)
            ("<f8>" . revert-buffer)
            ("<f12>" . global-display-line-numbers-mode)
            ("M-o" . other-window)
            ("M-O" . (lambda () (interactive) (other-window -1)))
            ("C-|" . (lambda () (interactive) (split-window-right) (balance-windows)))
            ("C--" . (lambda () (interactive) (split-window-below) (balance-windows)))
            ("C-M-<backspace>" . (lambda () (interactive) (delete-window) (balance-windows)))
            ("C-x C-o" . transpose-windows)
            ("C-x C-k" . kill-this-buffer)
            ("C-S-k" . (lambda () (interactive) (move-beginning-of-line nil) (kill-line 1)))
            ("C-M-o" . ff-find-other-file)

            ("M-t" . (lambda () (interactive) (jump-to-register 'r)))

            ("M-1" . (lambda () (interactive) (point-to-register 'r) (jump-to-register '1)))
            ("C-M-1" . (lambda () (interactive) (point-to-register '1)))

            ("M-2" . (lambda () (interactive) (point-to-register 'r) (jump-to-register '2)))
            ("C-M-2" . (lambda () (interactive) (point-to-register '2)))

            ("M-3" . (lambda () (interactive) (point-to-register 'r) (jump-to-register '3)))
            ("C-M-3" . (lambda () (interactive) (point-to-register '3)))

            ("M-4" . (lambda () (interactive) (point-to-register 'r) (jump-to-register '4)))
            ("C-M-4" . (lambda () (interactive) (point-to-register '4)))

            ("M-5" . (lambda () (interactive) (point-to-register 'r) (jump-to-register '5)))
            ("C-M-5" . (lambda () (interactive) (point-to-register '5)))

            ("M-6" . (lambda () (interactive) (point-to-register 'r) (jump-to-register '6)))
            ("C-M-6" . (lambda () (interactive) (point-to-register '6)))

            ("M-7" . (lambda () (interactive) (point-to-register 'r) (jump-to-register '7)))
            ("C-M-7" . (lambda () (interactive) (point-to-register '7)))

            ("M-8" . (lambda () (interactive) (point-to-register 'r) (jump-to-register '8)))
            ("C-M-8" . (lambda () (interactive) (point-to-register '8)))

            ("M-9" . (lambda () (interactive) (point-to-register 'r) (jump-to-register '9)))
            ("C-M-9" . (lambda () (interactive) (point-to-register '9)))

            ("M-0" . (lambda () (interactive) (point-to-register 'r) (jump-to-register '0)))
            ("C-M-0" . (lambda () (interactive) (point-to-register '0)))
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
            ("b" . list-abbrevs)
            ("f" . find-file-at-point)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Downloaded Packages ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auto-install from git url!
;; (auto-install-from-url "https://raw.github.com/jamcha-aa/auto-org-md/master/auto-org-md.el")

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

(if window-system

    (use-package moe-theme
      :ensure t
      :config
      (load-theme 'moe-dark t))

  ;; (use-package color-theme-sanityinc-tomorrow
  ;;   :ensure t
  ;;   :config
  ;;   (load-theme 'sanityinc-tomorrow-night t))

  ;; (use-package dracula-theme
  ;;     :ensure t
  ;;     :config
  ;;     (load-theme 'dracula t))

  ;; (use-package color-theme-sanityinc-solarized
  ;;   :ensure t
  ;;   :config
  ;;   (load-theme 'sanityinc-solarized-dark t))

  ;; (use-package zenburn-theme
  ;;   :ensure t
  ;;   :config
  ;;   (load-theme 'zenburn t))
  )

;;; Incrementally select text
(use-package expand-region
  :ensure t
  :bind
  ("C-h" . er/expand-region)
  ("C-S-H" . er/contract-region))

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
     ("_" "_"   "u"   markdown-mode)  ; underline
     ("**" "**" "b"   markdown-mode)  ; bolden
     ("*" "*"   "i"   markdown-mode)  ; italics
     ("`" "`"   "c"   markdown-mode)  ; code
     ("begin\n" "\nend\n" "b" opascal-mode)
     )))

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

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  :bind
  ("C-z m"         . mc/mark-all-like-this)
  ("C->"           . mc/mark-next-like-this)
  ("C-<"           . mc/mark-previous-like-this))

(use-package rust-mode
  :ensure t)

(use-package paredit
  :ensure t)

(use-package elpy
  :ensure t
  :config
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

  (use-package flycheck
    :ensure t)

  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  (use-package jinja2-mode
    :ensure t)

  (use-package py-autopep8
    :ensure t)

  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

  (elpy-enable))

(use-package magit
  :ensure t
  :bind
  ("<f10>" . magit))

(use-package projectile
  :ensure t
  :config

  (use-package projectile-ripgrep
    :ensure t)

  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (projectile-mode +1))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'wave)
  (powerline-default-theme))

(use-package diminish
  :ensure t
  :config
  (diminish 'abbrev-mode)
  (diminish 'auto-revert-mode)
  (diminish 'god-local-mode)
  (diminish 'wrap-region-mode)
  (diminish 'which-key-mode)
  (diminish 'yas-minor-mode)
  (diminish 'company-mode)
  (diminish 'helm-mode)

  ;; For the plugins that load when the buffer is created
  (add-hook 'company-mode-hook (lambda () (diminish 'company-mode)))
  (add-hook 'hungry-delete-mode-hook (lambda () (diminish 'hungry-delete-mode)))
  (add-hook 'beacon-mode-hook (lambda () (diminish 'beacon-mode)))
  )

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

(use-package helm
  :ensure t
  :bind
  ("M-x" . helm-M-x)
  :config
  (helm-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auto Generated Code ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
