(setq package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
 ("melpa" . "http://melpa.org/packages/")))

(global-set-key (kbd "M-o") 'other-window)
(windmove-default-keybindings)

(global-set-key (kbd "M-[") 'tab-bar-history-back)
(global-set-key (kbd "M-]") 'tab-bar-history-forward)

(package-initialize)
(require 'ergoemacs-mode)

(setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
(setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
(ergoemacs-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes '(leuven))
 '(desktop-save t)
 '(desktop-save-mode t)
 '(fido-mode t)
 '(global-tab-line-mode t)
 '(org-startup-indented t)
 '(package-selected-packages '(ergoemacs-mode cmake-mode))
 '(tab-bar-history-mode t)
 '(tab-bar-mode t)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
