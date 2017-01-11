;;; Package
(require 'package)

;;; Add some package servers
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
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
 '(ansi-color-names-vector
	 ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
	 (quote
		("c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" default)))
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
	 (quote
		(("#3C3D37" . 0)
		 ("#679A01" . 20)
		 ("#4BBEAE" . 30)
		 ("#1DB4D0" . 50)
		 ("#9A8F21" . 60)
		 ("#A75B00" . 70)
		 ("#F309DF" . 85)
		 ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
	 (quote
		(markdown-mode eshell-manual 2048-game ac-emmet auto-complete org-bullets counsel swiper avy lorem-ipsum control-lock try monokai-theme use-package)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(ring-bell-function (quote ignore))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
	 (quote
		((20 . "#F92672")
		 (40 . "#CF4F1F")
		 (60 . "#C26C0F")
		 (80 . "#E6DB74")
		 (100 . "#AB8C00")
		 (120 . "#A18F00")
		 (140 . "#989200")
		 (160 . "#8E9500")
		 (180 . "#A6E22E")
		 (200 . "#729A1E")
		 (220 . "#609C3C")
		 (240 . "#4E9D5B")
		 (260 . "#3C9F79")
		 (280 . "#A1EFE4")
		 (300 . "#299BA6")
		 (320 . "#2896B5")
		 (340 . "#2790C3")
		 (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
	 (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

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

