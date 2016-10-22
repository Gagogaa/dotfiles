;; Settings
(tool-bar-mode -1)
(setq-default tab-width 2)

;; Pakcage
(require 'package)

;; Add some package servers
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; Check to see if packages are installed
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

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages
(package-initialize)

(ensure-package-installed 'evil 'helm 'evil-leader)

(require 'evil-leader)
(global-evil-leader-mode) 			;; enable evil-leader... before enabling evil-mode
(evil-leader/set-leader "<SPC>") 		;; set spacebar as the leader key
(evil-leader/set-key				;; <leader> "KEY"
 "e" 'find-file					;; e -open a file
 "b" 'switch-to-buffer 				;; b -buffer switcher
 "k" 'kill-buffer				;; k -kill buffer
 "w" 'save-buffer)				;; w -save buffer

(require 'evil)
(evil-mode 1)        ;; enable evil-mode

;; ------------------------------------------------------------------------------

;; Color Scheme stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(package-selected-packages
   (quote
    (mic-paren evil-leader color-theme-sanityinc-solarized ## svg-clock helm evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

