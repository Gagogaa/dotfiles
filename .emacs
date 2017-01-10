;;; Package
(require 'package)

;;; Add some package servers
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(package-initialize)
;; (setq package-enable-at-startup nil)

;;; Ensure that use-package is installed
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(org-babel-load-file (expand-file-name ".my-emacs.org"))

;;; -------------- AUTO GENERATED CODE -------------- 

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 (quote
		(eshell-manual 2048-game ac-emmet auto-complete org-bullets counsel swiper avy lorem-ipsum control-lock try monokai-theme use-package)))
 '(ring-bell-function (quote ignore)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-candidate-face ((t (:inherit highlight))))
 '(ac-candidate-mouse-face ((t (:inherit highlight))))
 '(ac-completion-face ((t (:inherit default))))
 '(ac-gtags-candidate-face ((t (:inherit ac-candidate-face))))
 '(ac-gtags-selection-face ((t (:inherit ac-selection-face))))
 '(ac-selection-face ((t (:inherit isearch))))
 '(avy-lead-face ((t (:inherit lazy-highlight))))
 '(avy-lead-face-0 ((t (:inherit lazy-highlight))))
 '(avy-lead-face-2 ((t (:inherit lazy-highlight))))
 '(ivy-current-match ((t (:inherit isearch))))
 '(ivy-minibuffer-match-face-1 ((t (:inherit secondary-selection))))
 '(ivy-minibuffer-match-face-2 ((t (:inherit highlight))))
 '(ivy-minibuffer-match-face-3 ((t (:inherit isearch))))
 '(ivy-minibuffer-match-face-4 ((t (:inherit secondary-selection))))
 '(org-level-1 ((t (:inherit default :foreground "#FD971F"))))
 '(org-level-2 ((t (:inherit default :foreground "#A6E22E"))))
 '(org-level-3 ((t (:inherit default :foreground "#66D9EF"))))
 '(org-level-4 ((t (:inherit default :foreground "#E6DB74"))))
 '(popup-scroll-bar-background-face ((t (:inherit highlight))))
 '(popup-scroll-bar-foreground-face ((t (:inherit lazy-highlight)))))

