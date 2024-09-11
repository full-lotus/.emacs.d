(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes '(leuven))
 '(custom-safe-themes
   '("42b221449475e41bc04c5663164ffc5d1672c53163630d41d57ef27d5a01cca9" "21055a064d6d673f666baaed35a69519841134829982cbbb76960575f43424db" default))
 '(desktop-save t)
 '(desktop-save-mode t)
 '(display-fill-column-indicator-column 80)
 '(fido-mode t)
 '(global-display-fill-column-indicator-mode t)
 '(global-tab-line-mode t)
 '(openwith-associations '(("\\.\\(doc\\|docx\\)\\'" "libreoffice.writer" (file))))
 '(org-agenda-files
   '("/mnt/sda2/projects/dev/form-slap/README.org" "/home/german/Desktop/zigiwave/work-todo.org" "/mnt/sda2/projects/dev/job-search/resume/german-nurlygaianov.org" "/home/german/Desktop/zigiwave/work-todo-archive.org" "/mnt/sda2/projects/dev/cascad/design-review.org"))
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
   '(org ayu-theme org-babel-eval-in-repl ob-clojurescript ob-async async paredit clj-refactor modus-themes openwith org-tidy cider treemacs-all-the-icons treemacs clojure-mode magit))
 '(safe-local-variable-values
   '((cider-clojure-cli-parameters . "-M:cider/nrepl:dev")
     (cider-clojure-cli-parameters . "-M:dev")
     (cider-clojure-cli-parameters . "-X dev/-main")
     (cider-clojure-cli-parameters "-A:dev")
     (cider-clojure-cli-parameters "-X:dev/-main")
     (cider-clojure-cli-parameters "-X dev/-main")
     (cider-clojure-cli-aliases . ":dev")
     (cider-offer-to-open-cljs-app-in-browser)
     (cider-shadow-watched-builds ":dev")
     (cider-shadow-default-options . ":dev")
     (cider-default-cljs-repl . shadow)
     (cider-preferred-build-tool . shadow-cljs)
     (cider-clojure-cli-parameters . "-A:dev")
     (cider-merge-sessions quote project)
     (cider-merge-sessions 'project)
     (cider-clojure-cli-global-options . "-A:dev")
     (cider-clojure-cli-global-options . "-A:dev -X dev/-main")
     (cider-clojure-cli-global-options . "-A:dev -Sdeps {:deps{nrepl/nrepl{:mvn/version\"1.0.0\"}cider/cider-nrepl{:mvn/version\"0.44.0\"}}:aliases{:cider/nrepl{:main-opts[\"-m\"\"nrepl.cmdline\"\"--middleware\"\"[cider.nrepl/cider-middleware,shadow.cljs.devtools.server.nrepl/middleware]\"]}}} -M:cider/nrepl -X dev/-main")
     (cider-inject-dependencies-at-jack-in)
     (cider-clojure-cli-global-options . " -A:dev -X dev/-main --middleware \"[cider.nrepl/cider-middleware,shadow.cljs.devtools.server.nrepl/middleware]\"")
     (cider-preferred-build-tool . clojure-cli)
     (cider-clojure-cli-global-options . " -A:dev -X dev/-main --middleware [cider.nrepl/cider-middleware,shadow.cljs.devtools.server.nrepl/middleware]")
     (cider-preferred-build-tool . clj)))
 '(save-interprogram-paste-before-kill t)
 '(tab-bar-close-button-show nil)
 '(tab-bar-history-mode t)
 '(tab-bar-mode t)
 '(tab-line-close-button-show nil)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-bar ((t (:inherit variable-pitch :background "light sky blue" :foreground "black"))))
 '(tab-bar-tab ((t (:inherit tab-bar :box (:line-width (1 . 1) :style released-button)))))
 '(tab-bar-tab-inactive ((t (:inherit tab-bar-tab :background "light sky blue"))))
 '(tab-line ((t (:inherit variable-pitch :background "light sky blue" :foreground "black" :height 0.9))))
 '(tab-line-highlight ((t (:background "cyan" :foreground "black" :box (:line-width (1 . 1) :style released-button)))))
 '(tab-line-tab ((t (:inherit tab-line :box (:line-width (1 . 1) :style released-button)))))
 '(tab-line-tab-current ((t (:inherit tab-line-tab :background "cyan1"))))
 '(tab-line-tab-inactive ((t (:inherit tab-line-tab :background "light sky blue"))))
 '(tab-line-tab-inactive-alternate ((t (:inherit tab-line-tab-inactive :background "light sky blue")))))
