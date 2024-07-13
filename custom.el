(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(desktop-save t)
 '(desktop-save-mode t)
 '(display-fill-column-indicator-column 80)
 '(fido-mode t)
 '(global-display-fill-column-indicator-mode t)
 '(global-tab-line-mode t)
 '(ispell-dictionary nil)
 '(openwith-associations
   '(("\\.\\(doc\\|docx\\)\\'" "libreoffice.writer"
      (file))
     ("\\.pdf\\'" "acroread"
      (file))
     ("\\.mp3\\'" "xmms"
      (file))
     ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer"
      ("-idx" file))
     ("\\.\\(?:jp?g\\|png\\)\\'" "display"
      (file))))
 '(org-cycle-inline-images-display nil)
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file-other-frame)
     (wl . wl-other-frame)))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail org-tempo ol-w3m))
 '(org-startup-indented nil)
 '(org-tidy-protect-overlay nil)
 '(package-selected-packages
   '(org-babel-eval-in-repl ob-clojurescript ob-async async paredit modus-themes openwith org-tidy cider treemacs-all-the-icons treemacs clojure-mode magit))
 '(save-interprogram-paste-before-kill t)
 '(tab-bar-history-mode t)
 '(tab-bar-mode t)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
