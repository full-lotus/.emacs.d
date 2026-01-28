(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-font-lock-dynamically '(macro function var core))
 '(counsel-rg-base-command
	 '("rg" "--multiline" "--multiline-dotall" "--max-columns" "240"
		 "--with-filename" "--no-heading" "--line-number" "--color" "never" "%s"))
 '(custom-enabled-themes nil)
 '(custom-safe-themes
	 '("9ab9441566f7c3b059a205a7c5fad32a58422c2695815436d8cc087860b8c2e5"
		 "7613ef56a3aebbec29618a689e47876a72023bbd1b8393efc51c38f5ed3f33d1"
		 "703a3469ae4d2a83fd5648cac0058d57ca215d0fea7541fb852205e4fae94983"
		 "c7a926ad0e1ca4272c90fce2e1ffa7760494083356f6bb6d72481b879afce1f2"
		 "8a0eba8f06120605030161981032469d87a9832402e4441daa91296aa463f124"
		 "42b221449475e41bc04c5663164ffc5d1672c53163630d41d57ef27d5a01cca9"
		 "21055a064d6d673f666baaed35a69519841134829982cbbb76960575f43424db" default))
 '(openwith-associations '(("\\.\\(doc\\|docx\\)\\'" "libreoffice.writer" (file))))
 '(org-agenda-files
	 '("/mnt/sda2/projects/_self/monitoring/personal-journaling/notes.org"))
 '(package-selected-packages
	 '(all-the-icons-completion all-the-icons-dired async beacon bm cape casual-avy
															cider clojure-mode command-log-mode consult corfu
															dired-ranger embark embark-consult flycheck
															flycheck-clj-kondo iedit keyfreq magit marginalia
															ob-async ob-clojurescript openwith orderless
															pcre2el poly-org posframe puni rainbow-delimiters
															timeout transient treemacs treemacs-all-the-icons
															undo-fu-session vertico wgrep yasnippet))
 '(safe-local-variable-directories
	 '("/mnt/sda2/projects/dev/kombinacija/code"
		 "/mnt/sda2/projects/dev/simple-clojure-project/"))
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
		 (cider-shadow-default-options . ":dev") (cider-default-cljs-repl . shadow)
		 (cider-preferred-build-tool . shadow-cljs)
		 (cider-clojure-cli-parameters . "-A:dev")
		 (cider-merge-sessions quote project) (cider-merge-sessions 'project)
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
 '(winner-mode t))
