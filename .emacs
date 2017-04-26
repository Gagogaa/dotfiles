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
 '(ansi-color-faces-vector
	 [default bold shadow italic underline bold bold-italic bold])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
	 (quote
		("f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" default)))
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
		(zenburn-theme color-theme-sanityinc-solarized powerline which-key beacon ace-mc w3 engine-mode hl-todo ample-zen-theme solarized-theme git-gutter flycheck markdown-mode eshell-manual 2048-game ac-emmet auto-complete org-bullets counsel swiper avy lorem-ipsum control-lock try monokai-theme use-package)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(ring-bell-function (quote ignore))
 '(weechat-color-list
	 (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-candidate-mouse-face ((t (:inherit highlight))))
 '(ac-completion-face ((t (:inherit default))))
 '(ac-gtags-candidate-face ((t (:inherit ac-candidate-face))))
 '(ac-gtags-selection-face ((t (:inherit ac-selection-face)))))

(put 'narrow-to-region 'disabled nil)
