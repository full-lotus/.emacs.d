(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(cider-font-lock-dynamically '(macro function var core))
 '(counsel-rg-base-command
   '("rg" "--multiline" "--multiline-dotall" "--max-columns" "240"
     "--with-filename" "--no-heading" "--line-number" "--color"
     "never" "%s"))
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("9ab9441566f7c3b059a205a7c5fad32a58422c2695815436d8cc087860b8c2e5"
     "7613ef56a3aebbec29618a689e47876a72023bbd1b8393efc51c38f5ed3f33d1"
     "703a3469ae4d2a83fd5648cac0058d57ca215d0fea7541fb852205e4fae94983"
     "c7a926ad0e1ca4272c90fce2e1ffa7760494083356f6bb6d72481b879afce1f2"
     "8a0eba8f06120605030161981032469d87a9832402e4441daa91296aa463f124"
     "42b221449475e41bc04c5663164ffc5d1672c53163630d41d57ef27d5a01cca9"
     "21055a064d6d673f666baaed35a69519841134829982cbbb76960575f43424db"
     default))
 '(desktop-save t)
 '(desktop-save-mode t)
 '(display-fill-column-indicator-column 80)
 '(fido-mode t)
 '(global-display-fill-column-indicator-mode t)
 '(openwith-associations '(("\\.\\(doc\\|docx\\)\\'" "libreoffice.writer" (file))))
 '(org-agenda-files
   '("/mnt/sda2/projects/dev/kombinacija/README.org"
     "/mnt/sda2/projects/dev/form-slap/README.org"))
 '(org-cycle-inline-images-display nil)
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news) (file . find-file-other-frame)
     (wl . wl-other-frame)))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus ol-info ol-irc
	     ol-mhe ol-rmail org-tempo ol-w3m))
 '(org-startup-indented nil)
 '(org-tidy-protect-overlay nil)
 '(package-selected-packages
   '(async ayu-theme cider clj-refactor clojure-mode command-log-mode
	   counsel dired-ranger ivy magit modus-themes ob-async
	   ob-clojurescript openwith org org-babel-eval-in-repl
	   org-tidy paredit pcre2el replace-symbol smex treemacs
	   treemacs-all-the-icons wgrep))
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
     (cider-clojure-cli-global-options
      . "-A:dev -Sdeps {:deps{nrepl/nrepl{:mvn/version\"1.0.0\"}cider/cider-nrepl{:mvn/version\"0.44.0\"}}:aliases{:cider/nrepl{:main-opts[\"-m\"\"nrepl.cmdline\"\"--middleware\"\"[cider.nrepl/cider-middleware,shadow.cljs.devtools.server.nrepl/middleware]\"]}}} -M:cider/nrepl -X dev/-main")
     (cider-inject-dependencies-at-jack-in)
     (cider-clojure-cli-global-options
      . " -A:dev -X dev/-main --middleware \"[cider.nrepl/cider-middleware,shadow.cljs.devtools.server.nrepl/middleware]\"")
     (cider-preferred-build-tool . clojure-cli)
     (cider-clojure-cli-global-options
      . " -A:dev -X dev/-main --middleware [cider.nrepl/cider-middleware,shadow.cljs.devtools.server.nrepl/middleware]")
     (cider-preferred-build-tool . clj)))
 '(save-interprogram-paste-before-kill t)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "medium aquamarine"))))
 '(font-lock-constant-face ((t (:foreground "saddle brown"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-keyword-face ((t (:foreground "violet red"))))
 '(font-lock-string-face ((t (:foreground "dark violet"))))
 '(font-lock-type-face ((t (:foreground "dark orange"))))
 '(font-lock-variable-name-face ((t (:foreground "blue1"))))
 '(link ((t (:foreground "dark cyan" :underline t))))
 '(mode-line ((t (:background "lavender" :foreground "black" :box (:line-width (1 . -1) :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "lavender" :foreground "black" :box (1 . -1) :weight light))))
 '(org-block ((t (:extend t :background "lavender" :foreground "black"))))
 '(org-block-begin-line ((t (:extend t :foreground "dim gray" :slant oblique))))
 '(org-block-end-line ((t (:inherit org-block-begin-line :extend t))))
 '(org-level-1 ((t (:extend nil :foreground "dark violet" :height 1.5))))
 '(org-level-2 ((t (:extend nil :foreground "blue"))))
 '(org-level-3 ((t (:extend nil :foreground "deep sky blue"))))
 '(org-level-4 ((t (:extend nil :foreground "forest green"))))
 '(org-level-5 ((t (:extend nil :foreground "gold2"))))
 '(org-level-6 ((t (:extend nil :foreground "dark orange"))))
 '(org-level-7 ((t (:extend nil :foreground "red"))))
 '(org-level-8 ((t (:extend nil :foreground "black"))))
 '(org-meta-line ((t (:foreground "dim gray" :slant oblique))))
 '(tab-bar ((t (:inherit variable-pitch :background "lavender" :foreground "black"))))
 '(tab-bar-tab ((t (:inherit tab-bar :background "MediumPurple1" :foreground "white" :box (:line-width (1 . 1) :style released-button)))))
 '(tab-bar-tab-group-current ((t (:inherit bold :background "MediumPurple1" :foreground "white" :box (:line-width (2 . -2) :color "#ffffff")))))
 '(tab-bar-tab-group-inactive ((t (:background "lavender" :foreground "#193668" :box (:line-width (2 . -2) :color "lavender")))))
 '(tab-bar-tab-inactive ((t (:inherit tab-bar-tab :background "lavender"))))
 '(tab-line ((t (:inherit variable-pitch :background "lavender" :foreground "black" :height 0.9))))
 '(tab-line-highlight ((t (:background "MediumPurple1" :foreground "white" :box (:line-width (1 . 1) :style released-button)))))
 '(tab-line-tab ((t (:inherit tab-line :box (:line-width (1 . 1) :style released-button)))))
 '(tab-line-tab-current ((t (:inherit tab-line-tab :background "MediumPurple1" :foreground "white"))))
 '(tab-line-tab-inactive ((t (:inherit tab-line-tab :background "lavender"))))
 '(tab-line-tab-inactive-alternate ((t (:inherit tab-line-tab-inactive :background "lavender")))))
