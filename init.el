;; --------------- emacs Customize module hacks ---------------
(defun setq-and-tell-customize (&rest var-val-pairs)
  "Set variable value and make sure Customize registers this change. Use
instead of setq, to avoid confusion in Customize interface"
  (message "%s" var-val-pairs)
  (while var-val-pairs
    (let ((var (pop var-val-pairs))
          (val (pop var-val-pairs)))
      (set var val)
      (put var 'customized-value
           (list (custom-quote (symbol-value var)))))))
;; -----------------------------------------------------------------------------



;; --------------- package management ---------------
(setq-and-tell-customize 'package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
   ("non-gnu" . "https://elpa.nongnu.org/nongnu/")
   ("melpa" . "http://melpa.org/packages/")))

; add all installed packages to load-path
(package-initialize)
; fetch the list of packages available, must run package-initialize first
(unless package-archive-contents
  (package-refresh-contents))

; list non-default packages
(custom-set-variables
 '(package-selected-packages
   '(org-babel-eval-in-repl ivy counsel smex wgrep
     ob-clojurescript ob-async async wgrep
     openwith org-tidy cider treemacs-all-the-icons treemacs
     clojure-mode magit command-log-mode posframe pcre2el dired-ranger
     flycheck flycheck-clj-kondo rainbow-delimiters casual-avy puni)))

; install all non-default packages
(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

; load all non-default packages
(dolist (pkg package-selected-packages)
  (require pkg nil 'noerror))
;; -----------------------------------------------------------------------------



;; --------------- other elisp customization files ---------------
(add-to-list 'load-path "~/.emacs.d/elisp/")
(load "~/.emacs.d/elisp/replace+.el")

;; this prevents replace+ being limited to an active region
(setq-and-tell-customize 'search/replace-region-as-default-flag t)
;; -----------------------------------------------------------------------------



;; --------------- general global settings  ---------------
;; use avy through Transient menu
(keymap-global-set "M-g" #'casual-avy-tmenu)

(keymap-global-set "M-SPC" #'avy-goto-word-0)

;; make some multi-chord commands repeatable with 1 key press
;; for example - repeat C-x <LEFT> (previous-buffer) with just <LEFT>
(repeat-mode 1)

;; use SSH through daemon launched on startup
(setenv "SSH_AUTH_SOCK" (concat (getenv "XDG_RUNTIME_DIR") "/ssh-agent.socket"))


;; save command history, e. g. for execute-extended-command
(savehist-mode 1)

;; use Ivy + Counsel + Swiper for better completion/search
;; settings taken from here https://github.com/abo-abo/swiper
(ivy-mode)
(setq-and-tell-customize 'ivy-use-virtual-buffers t)
(setq-and-tell-customize 'enable-recursive-minibuffers t)

;; disable icomplete to fix error:
;; Error in post-command-hook (icomplete-post-command-hook):
;; (wrong-number-of-arguments #<subr counsel-ag-function> 3)
(defun ivy-icomplete (f &rest r)
  (icomplete-mode -1)
  (unwind-protect
      (apply f r)
    (icomplete-mode 1)))

(advice-add 'ivy-read :around #'ivy-icomplete)

;; wgrep allows to convert ivy-occur buffer to editable, to get VS Code-like
;; search and replace experience
(use-package wgrep
  :ensure t
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; counsel-mode improves many stanard completion-related features
(counsel-mode)

;; fix counsel-rg not displaying errors properly
(with-eval-after-load 'counsel
  (advice-add 'counsel-rg
              :around
              (lambda (func &rest args)
                (cl-flet ((filter-func (code) (if (= code 2) 0 code)))
                  (unwind-protect
                      (progn (advice-add 'process-exit-status :filter-return #'filter-func)
                             (apply func args))
                    (advice-remove 'process-exit-status #'filter-func))))))

;; use smex to show command history in counsel-M-x
(smex-initialize)
(setq-and-tell-customize 'smex-history-use-recent-first t)

;; Enable orderless matching for execute-extended-command
(setq-and-tell-customize 'ivy-re-builders-alist
                         '((counsel-M-x . ivy--regex-ignore-order)
                           (t . ivy--regex-plus)))

;; Drop beginning-of-string anchor ^ from execute-extended-command
(with-eval-after-load 'ivy
  (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) ""))

;; enter an input that matches one of the candidates instead of this candidate
(setq-and-tell-customize 'ivy-use-selectable-prompt t)



(defun get-initial-input-for-replace ()
  nil)
(setq-and-tell-customize 'search/replace-default-fn
                         'get-initial-input-for-replace)

(defun query-replace-regexp-with-initial-input (input)
  (eval
   '(let ((original-fn (symbol-function 'initial-input-for-replace)))
      (fset 'get-initial-input-for-replace (lambda () (regexp-quote input)))
      (unwind-protect
          (call-interactively 'query-replace-regexp)
        (fset 'get-initial-input-for-replace original-fn)))
   t))

(defun rapid-replace (search-fn)
  "Opens up wgrep buffer with query-replace-regexp started"
  (interactive)
  (eval
   '(let* ((thing (ivy-thing-at-point))
           (search-str (read-string "Enter at least 3 chars to replace: " thing)))
      (run-at-time
       nil nil
       (lambda ()
         (run-at-time
          nil nil
          (lambda ()
            (run-at-time
             nil nil
             (lambda ()
               (query-replace-regexp-with-initial-input search-str)
               ))
            (ivy-wgrep-change-to-wgrep-mode)))
         (ivy-occur)))
      (funcall search-fn search-str))
   t))

(defun rapid-replace-in-git-repo ()
  (interactive)
  (rapid-replace 'counsel-git-grep))

(defun rapid-replace-ripgrep ()
  (interactive)
  (rapid-replace 'counsel-rg))

(defun regex-fn-ivy-thing-at-point (regex-fn)
  "Run `counsel-git-grep` with ivy-thing-at-point as the initial input."
  (interactive)
  (let ((thing (ivy-thing-at-point)))
    (when (use-region-p)
      (deactivate-mark))
    (funcall regex-fn (regexp-quote thing))))

(defun counsel-git-grep-ivy-thing-at-point ()
  (interactive)
  (regex-fn-ivy-thing-at-point 'counsel-git-grep))

(defun counsel-rg-ivy-thing-at-point ()
  (interactive)
  (regex-fn-ivy-thing-at-point 'counsel-rg))

;; some of those hotkeys are redundant because of counsel-mode
;; but I'm not sure which I can drop, and to lazy to check one-by-one
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c J") 'counsel-git-grep-ivy-thing-at-point)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c r") 'counsel-rg)
(global-set-key (kbd "C-c R") 'counsel-rg-ivy-thing-at-point)
(global-set-key (kbd "C-S-h") 'rapid-replace-ripgrep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)



;; hide useless title bar, but allow resizing
(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))

;; taken from Adam James' guide:
;; https://gist.github.com/adam-james-v/7a61612ce0649afc78513f54b337d8c9

;; separate custom-file because it's a mess, with everything in one place
(setq-and-tell-customize 'custom-file (concat user-emacs-directory "custom.el"))
;; we are loading at the top of init.el, to set custom variables nice and
;; separate, grouping relevant settings together
(load custom-file 'noerror)

(custom-set-variables
 ;; blinking cursor is annoying
 '(blink-cursor-mode nil)

 ;; save Emacs frame configuration on exit
 '(desktop-save-mode t)

 ;; to see when a line is longer than 80 symbols
 '(display-fill-column-indicator-column 80)
 '(global-display-fill-column-indicator-mode t)

 ;; continiously display completions in minibuffer while typing
 '(fido-mode t)

 ;; copy kill ring entries to OS clipboard
 '(save-interprogram-paste-before-kill t))


(setq-and-tell-customize 'make-backup-files nil) ; stop creating backup~ files
(setq-and-tell-customize 'auto-save-default nil) ; stop creating #autosave# files
(setq-and-tell-customize 'create-lockfiles nil) ; no lockfiles

;; The auto-revert setting is enabled because tangle / detangle for literate
;; programming will change contents of files. If the file is open in a buffer,
;; I want it to automatically show the change without asking me every time.
(global-auto-revert-mode t)
(tool-bar-mode 0)
(scroll-bar-mode 1) ;; enable vertical scroll bars
(horizontal-scroll-bar-mode 1) ;; enable horizontal scroll bars
(delete-selection-mode 1) ;; delete active region on yanking (paste)

(global-set-key (kbd "C-z") 'undo) ;; make undo hotkey familiar

;; supposedly good defaults from here:
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#use-better-defaults
(setq-and-tell-customize
 'ad-redefinition-action 'accept        ; Silence warnings for redefinition
 'auto-window-vscroll nil               ; Lighten vertical scroll
 'confirm-kill-emacs 'yes-or-no-p       ; Confirm before exiting Emacs
 'display-time-default-load-average nil ; Don't display load average
 'display-time-mode 0                   ; Display time in frames
 'display-time-format "%H:%M"           ; Format the time string
 'fill-column 80                        ; Set width for automatic line breaks
 'display-line-numbers-type nil
 'column-number-mode t
 'help-window-select t                      ; Focus new help windows when opened
 'indent-tabs-mode nil                      ; Stop using tabs to indent
 'inhibit-startup-screen t                  ; Disable start-up screen
 'initial-scratch-message ""                ; Empty the initial *scratch* buffer
 'left-margin-width 1 'right-margin-width 1 ; Add left and right margins
 'mouse-yank-at-point t                     ; Yank at point rather than pointer
 'ns-use-srgb-colorspace nil                ; Don't use sRGB colors
 'select-enable-clipboard t             ; Merge system's and Emacs' clipboard
 'sentence-end-double-space nil
 ;; try to reuse existing window for opening buffers              ; End a sentence after a dot and a space
 'show-trailing-whitespace nil          ; Display trailing whitespaces
 'split-height-threshold nil            ; Disable vertical window splitting
 'split-width-threshold 1               ; Disable horizontal window splitting
 'tab-width 4                           ; Set width for tabs
 'uniquify-buffer-name-style 'forward   ; Uniquify buffer names
 'window-combination-resize t           ; Resize windows proportionally
 'x-stretch-cursor t                    ; Stretch cursor to the glyph width
 'scroll-step 1
 'scroll-conservatively 10000

 'display-buffer-alist
 '(
   ;; treemacs is a file navigator, typically glued to the left screen side
   ("\\*Treemacs-.*?" (display-buffer-in-direction) (direction . left))

   ;; Org Src buffers should pop up in the same window
   ("\\*Org Src.*\\*"
    (display-buffer-same-window
     display-buffer-use-least-recent-window))

   ;; open cider-test-report in another window, to keep the relevant tests open
   ("\\*cider-test-report\\*"
    (display-buffer-use-least-recent-window)
    (inhibit-switch-frame . t))

   ;; open cider-error in the same window, to keep the relevant code open
   ("\\*cider-error\\*"
    (display-buffer-same-window)
    (inhibit-same-window . nil)
    (inhibit-switch-frame . t))

   ;; open *Help* buffers in another window
   ("\\*Help\\*"
    (display-buffer-use-least-recent-window)
    (inhibit-same-window . t))

   ;; without this, transient buffer breaks window layout
   ;; it happens because of conflicts with lower settings
   (".*transient.*"
    (display-buffer-in-side-window)
    (side . bottom)
    (inhibit-same-window . t)
    (window-parameters (no-other-window . t)))

   ;; open search matches from ivy-occur buffer in another window
   ((major-mode . ivy-occur-grep-mode)
    (display-buffer-use-least-recent-window)
    (inhibit-same-window . t)
    (inhibit-switch-frame . t))

   ;; open scratch buffers in selected frame, existing windows
   ("\\*.*\\*"
    (display-buffer-same-window
     display-buffer-use-some-window)
    (inhibit-same-window . nil)
    (inhibit-switch-frame . t))

   ((major-mode . dired-mode)
    (display-buffer-same-window
     display-buffer-reuse-window
     display-buffer-use-least-recent-window)
    (inhibit-same-window . nil))

   ;; do not switch windows in org-mode, e. g. for org-mark-ring-goto
   ((major-mode . org-mode)
    (display-buffer-same-window
     display-buffer-reuse-window
     display-buffer-use-least-recent-window)
    (inhibit-same-window . nil))

   ;; do not switch windows when opening source-files
   ((major-mode . clojure-mode)
    (display-buffer-same-window
     display-buffer-reuse-window
     display-buffer-use-least-recent-window)
    (inhibit-same-window . nil))

   ;; open Cider REPL in the same window and frame
   ((major-mode . cider-repl-mode)
    (display-buffer-same-window
     display-buffer-reuse-window
     display-buffer-use-least-recent-window)
    (inhibit-same-window . nil)
    (inhibit-switch-frame . t))

   ;; prevent all other buffers from opening new windows and switching frames
   (".*"
    (display-buffer-use-least-recent-window
     display-buffer-some-window)
    (inhibit-same-window . nil)
    (inhibit-switch-frame . t))
   )
 )

;; don't open new buffers when navigating dirs in dired
(setq-and-tell-customize 'dired-kill-when-opening-new-dired-buffer t)

;; use screen flash instead of annoying error sound
(setq-and-tell-customize 'visible-bell t)

(display-time-mode 0)                             ; Enable time in the mode-line
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(menu-bar-mode 0)                                 ; Disable the menu bar
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding

;; Apparently Garbage Collecting when out of focus can make emacs feel faster. I’ll try that.
(add-hook 'focus-out-hook #'garbage-collect)

;; Always show completions
(setq-and-tell-customize 'completion-auto-help 'always)
;; Auto-select *Completions* buffer
(setq-and-tell-customize 'completion-auto-select 'second-tab)
;; Make <TAB> invoke completions list, when code is already idented properly
(setq-and-tell-customize 'tab-always-indent 'complete)


;; Don't clutter main Emacs folder with session. files
(defun emacs-session-filename (session-id)
  "Return the file name of the session file for SESSION-ID."
  (expand-file-name (concat "session." session-id)
                    "~/.emacs.d/sessions/"))
;; -----------------------------------------------------------------------------



;; --------------- programming  ---------------
;; check syntax globally
(global-flycheck-mode)

;; enable colorful parens globally
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; --------------- Clojure  ---------------
(setq-and-tell-customize
      'nrepl-hide-special-buffers t
      'cider-repl-clear-help-banner t
      'cider-font-lock-dynamically '(macro core function var)
      'cider-popup-stacktraces nil
      'cider-repl-popup-stacktraces t
      'cider-repl-use-pretty-printing t
      'cider-repl-pop-to-buffer-on-connect t
      'cider-repl-display-help-banner nil)

;; Allow cider-repl to be cleared with shortcut
(add-hook 'cider-repl-mode-hook
      '(lambda () (define-key cider-repl-mode-map (kbd "C-c M-b")
            'cider-repl-clear-buffer)))

(add-hook 'clojure-mode-hook #'cider-mode)

(add-hook 'cider-mode-hook (lambda () (show-paren-mode 1)))
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'puni-mode)
(add-hook 'cider-repl-mode-hook #'puni-mode)
(add-hook 'cider-mode-hook #'imenu-add-menubar-index)



;; I write tests in the same ns as source code, source and tests must be close
(setq-and-tell-customize 'cider-test-infer-test-ns (lambda (ns) ns))



;; use sexp editing hotkeys from VS Code Calva, to which I am used to
(define-key puni-mode-map (kbd "C-<right>") 'puni-forward-sexp)
(define-key puni-mode-map (kbd "C-<left>") 'puni-backward-sexp)
(define-key puni-mode-map (kbd "C-M-.") 'puni-slurp-forward)
(define-key puni-mode-map (kbd "C-M-,") 'puni-barf-forward)
(define-key puni-mode-map (kbd "C-M->") 'puni-barf-backward)
(define-key puni-mode-map (kbd "C-M-<") 'puni-slurp-backward)

(define-key cider-repl-mode-map [C-return] nil)
;; those hotkeys are also from VS Code Calva

(define-key cider-mode-map [C-return] 'cider-eval-sexp-at-point)
(define-key cider-mode-map (kbd "M-<RET>") 'cider-eval-defun-at-point)
(define-key cider-repl-mode-map [C-return] 'cider-eval-sexp-at-point)
(define-key cider-repl-mode-map (kbd "M-<RET>") 'cider-eval-defun-at-point)
;; -----------------------------------------------------------------------------



;; --------------- ClojureScript ---------------
(add-hook 'clojurescript-mode #'puni-mode)
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------


;; --------------- file management  ---------------
;; mode that enables choosing program-to-open-with based on file extensions
(openwith-mode t)

;; trying these setting to be able to edit files as root
(require 'tramp)
(setq tramp-default-method "sudoedit")

(custom-set-variables
  ;; associate file extensions (regex) to program-to-open-with
  '(openwith-associations
    '(("\\.\\(doc\\|docx\\)\\'" "libreoffice.writer"
        (file))
      ;; ("\\.pdf\\'" "acroread"
      ;;  (file))
      ;; ("\\.mp3\\'" "xmms"
      ;;  (file))
      ;; ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer"
      ;;  ("-idx" file))
      ;; ("\\.\\(?:jp?g\\|png\\)\\'" "display"
      ;;  (file))
      )))
;; -----------------------------------------------------------------------------



;; --------------- window/tab navigation ---------------
(global-set-key (kbd "C-o") 'other-window)
(define-key dired-mode-map (kbd "C-o") 'other-window)
(define-key ivy-occur-mode-map (kbd "C-o") 'other-window)
(define-key ivy-occur-grep-mode-map (kbd "C-o") 'other-window)
(global-set-key (kbd "M-o") 'other-frame)
(global-set-key (kbd "C-S-b") 'treemacs)
(global-set-key (kbd "C-x w") 'bury-buffer)

(custom-set-variables
 '(winner-mode t))
;; -----------------------------------------------------------------------------



;; --------------- org mode ---------------
(require 'org)

(eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (clojurescript . t)
     (emacs-lisp . t)
     (shell . t))))

(setq-and-tell-customize 'org-support-shift-select 'always)
(setq-and-tell-customize 'org-agenda-restore-windows-after-quit t)


;; ;; prevent window/frame config changes during detangling
;; (defun org-babel-detangle-no-buffer-pop-up (orig-fun &rest args)
;;   (save-excursion
;;     (let ((display-buffer-alist
;;            '((".*" (display-buffer-no-window) (allow-no-window . t)))))
;;       (apply orig-fun args))))

;; (advice-add 'org-babel-detangle :around #'org-babel-detangle-no-buffer-pop-up)

;; ;; detangle on each file save, doing this by hand is tedious
;; (add-hook 'after-save-hook 'org-babel-detangle)



;; Tangling can be set to occur automatically on save. This makes things way
;; simpler. Additionally, we set up todos to be moved to the agenda on save.
;; This is just to keep things organized if todos are added to project org
;; files

;; Tangle on save only occurs if the buffer being saved is an Org-Mode file.

(defun org-babel-clojure-cider-current-ns ())

(defun tangle-on-save-org-mode-file ()
  (when (and (string-match-p
              (regexp-quote ".org") (message "%s" (current-buffer)))
             (not (string-match-p
                   (regexp-quote "[") (message "%s" (current-buffer)))))
    (org-babel-tangle)))

(add-hook 'after-save-hook 'tangle-on-save-org-mode-file)

(defun to-agenda-on-save-org-mode-file ()
  (when (string= (message "%s" major-mode) "org-mode")
    (org-agenda-file-to-front)))

(add-hook 'after-save-hook 'to-agenda-on-save-org-mode-file)
;; disable new buffer pop-up caused by tangling
(defun org-babel-tangle-no-buffer-pop-up (orig-fun &rest args)
  (save-excursion
    (let ((display-buffer-alist
           '((".*" (display-buffer-no-window) (allow-no-window . t)))))
      (apply orig-fun args))))

(advice-add 'org-babel-tangle :around 'org-babel-tangle-no-buffer-pop-up)



(defun org-collapse-above-level (level)
  "Collapse all headings above LEVEL in the current buffer."
  (interactive "nEnter level to collapse above: ")
  (org-map-entries
   (lambda ()
     (let ((heading-level (org-current-level)))
       (when (and heading-level (> (+ heading-level 1) level))
         (hide-subtree))))
   t 'file))

(define-key org-mode-map (kbd "C-c h") 'org-collapse-above-level)



;; override broken fn, which deletes comments from clojure code
(add-to-list 'load-path "~/.emacs.d/elisp/")
(load "~/.emacs.d/elisp/ob-clojure-fix.el")
(eval-after-load 'org
  (with-eval-after-load 'org
    (advice-add 'org-babel-expand-body:clojure
		:override #'org-babel-expand-body:clojure_fixed)))

;; Some backends for code execution need to be set.
(setq-and-tell-customize
 ;; open src blocks in the same window as parent .org file
 'org-src-window-setup 'current-window
 'org-babel-clojure-backend 'cider
 'org-babel-clojure-sync-nrepl-timeout nil)



;; open the file specified by the :tangle header argument
(defun org-babel-open-tangle-file ()
  (interactive)
  (let* ((info (org-babel-get-src-block-info))
         (tangle (cdr (assoc :tangle (nth 2 info)))))
    (when tangle
      (find-file tangle))))

(define-key org-mode-map (kbd "C-c o") 'org-babel-open-tangle-file)


(setq-and-tell-customize
      'org-startup-folded nil
      'org-hide-emphasis-markers nil
      'org-edit-src-content-indentation 0
      'org-src-tab-acts-natively t
      ;; Show syntax highlighting per language native mode in *.org
      'org-src-fontify-natively t
      ;; For languages with significant whitespace like Python:
      'org-src-preserve-indentation t
      'org-confirm-babel-evaluate nil
      )

(add-hook 'org-mode-hook 'show-paren-mode)

;; Trying to fix weird org syntax problems. This just lets Org ignore < and >
;; characters as if they were regular words. This is necessary because in
;; Clojure I want to make functions with -> in the name and Org was always
;; insisting on pairing <>. This caused any other paren matching to stop
;; working. It sucked.
(defun my-angle-bracket-fix ()
  (modify-syntax-entry ?< "w")
  (modify-syntax-entry ?> "w"))
(add-hook 'org-mode-hook 'my-angle-bracket-fix)



;; It’s extremely useful to split code blocks to quickly add org-mode text
;; between the src. The default binding is C-c C-v C-d, which is somewhat
;; annoying. I think M-s in org-mode should do the trick.

;; Split Org Block using M-s
(define-key org-mode-map (kbd "M-s") 'org-babel-demarcate-block)

;; toggle puni-mode manually
(define-key org-mode-map (kbd "M-P") 'puni-mode)
;;
(define-key org-mode-map (kbd "C-c a") 'org-agenda)


;; Sets M-<return> to evaluate code blocks in the REPL
(defun org-meta-return-around (org-fun &rest args)
  "Run `ober-eval-in-repl' if in source code block,
  `ober-eval-block-in-repl' if at header,
  and `org-meta-return' otherwise."
    (if (org-in-block-p '("src"))
        (let* ((point (point))
               (element (org-element-at-point))
               (area (org-src--contents-area element))
               (beg (copy-marker (nth 0 area))))
          (if (< point beg)
              (ober-eval-block-in-repl)
            (ober-eval-in-repl)))
      (apply org-fun args)))

(advice-add 'org-meta-return :around #'org-meta-return-around)

;; Prevent eval in repl from moving cursor to the REPL
(with-eval-after-load "eval-in-repl"
  (setq eir-jump-after-eval nil))



;; Remove the function which causes text to pop around when pressing tab.
;; This is annoying and confusing.
(remove-hook 'org-cycle-hook
             'org-optimize-window-after-visibility-change)



;; When a file is modified externally, emacs does not show this change
;; by default. Instead, when you try to edit it will ask you to modify or
;; revert. Since Tangling files changes src code automatically, it is more
;; effective to automatically revert any buffers which have src files open.
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
		 (file-exists-p (buffer-file-name))
		 (not (buffer-modified-p)))
	(revert-buffer t t t) )))
  (message "Refreshed open files."))
(add-hook 'after-save-hook 'revert-all-buffers)



;; The following code is from:
;; https://www.wisdomandwonder.com/article/10630/how-fast-can-you-tangle-in-org-mode
;; It basically boils down to adjusting garbage collection settings at key times during an org file save. Not strictly necessary, but nice to have.

(setq help/default-gc-cons-threshold gc-cons-threshold)
(defun help/set-gc-cons-threshold (&optional multiplier notify)
  "Set `gc-cons-threshold' either to its default value or a
   `multiplier' thereof."
  (let* ((new-multiplier (or multiplier 1))
         (new-threshold (* help/default-gc-cons-threshold
                           new-multiplier)))
    (setq gc-cons-threshold new-threshold)
    (when notify (message "Setting `gc-cons-threshold' to %s"
                          new-threshold))))
(defun help/double-gc-cons-threshold () "Double `gc-cons-threshold'." (help/set-gc-cons-threshold 2))
(add-hook 'org-babel-pre-tangle-hook #'help/double-gc-cons-threshold)
(add-hook 'org-babel-post-tangle-hook #'help/set-gc-cons-threshold)



(custom-set-variables
 ;; do not display inline images when doing org-cycle
 '(org-cycle-inline-images-display nil)

 '(org-link-frame-setup
    '((vm . vm-visit-folder-other-frame)
      (vm-imap . vm-visit-imap-folder-other-frame)
      (gnus . org-gnus-no-new-news)
      ;; open file links in another frame
      (file . find-file-other-frame)
      (wl . wl-other-frame)))

 ;; disable org-mode indentation, to keep lines under 80 characters
 '(org-startup-indented nil)

 ;; not sure how this variable was set, decided not to mess with it
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus
             ol-info ol-irc ol-mhe ol-rmail org-tempo ol-w3m))
  '(org-tidy-protect-overlay nil))

(defun my/org-checkbox-todo ()
  "Switch header TODO state to DONE when all checkboxes are ticked,
to TODO otherwise"
  (let ((todo-state (org-get-todo-state)) beg end)
    (unless (not todo-state)
      (save-excursion
    (org-back-to-heading t)
    (setq beg (point))
    (end-of-line)
    (setq end (point))
    (goto-char beg)
    (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                   end t)
        (if (match-end 1)
        (if (equal (match-string 1) "100%")
            (unless (string-equal todo-state "DONE")
              (org-todo 'done))
          (unless (string-equal todo-state "TODO")
            (org-todo 'todo)))
          (if (and (> (match-end 2) (match-beginning 2))
               (equal (match-string 2) (match-string 3)))
          (unless (string-equal todo-state "DONE")
            (org-todo 'done))
        (unless (string-equal todo-state "TODO")
          (org-todo 'todo)))))))))

(add-hook 'org-checkbox-statistics-hook 'my/org-checkbox-todo)



(defun my/org-add-ids-to-headlines-in-file ()
  "Add ID properties to all headlines in the current file which
do not already have one."
  (interactive)
  (org-map-entries 'org-id-get-create))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'my/org-add-ids-to-headlines-in-file
		      nil 'local)))

;; package for hiding :PROPERTIES: and other drawers
;; since I add :ID: to every header automatically, org-mode is cluttered
;; without this package
(use-package org-tidy
  :ensure t
  :hook
  (org-mode . org-tidy-mode))

(defun my/copy-id-to-clipboard()
  "Copy the ID property value to killring,
if no ID is there then create a new unique ID.
This function works only in org-mode buffers.

The purpose of this function is to easily construct id:-links to
org-mode items. If its assigned to a key it saves you marking the
text and copying to the killring."
       (interactive)
       (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
     (setq mytmpid (funcall 'org-id-get-create))
     (kill-new mytmpid)
     (message "Copied %s to killring (clipboard)" mytmpid)
       ))

(global-set-key (kbd "<f5>") 'my/copy-id-to-clipboard)
;; -----------------------------------------------------------------------------



;; --------------- emacs introspection ---------------
(setq dw/command-window-frame nil)

(defun dw/toggle-command-window ()
  "Show overlay frame which shows invoked functions and their hotkeys"
  (interactive)
  (if dw/command-window-frame
      (progn
        (posframe-delete-frame clm/command-log-buffer)
        (setq dw/command-window-frame nil))
    (progn
      (global-command-log-mode t)
      (with-current-buffer
          (setq clm/command-log-buffer
                (get-buffer-create " *command-log*"))
        (text-scale-set -1))
      (setq dw/command-window-frame
            (posframe-show
             clm/command-log-buffer
             :position `(,(- (x-display-pixel-width) 450) . 15)
             :width 38
             :height 5
             :min-width 38
             :min-height 5
             :internal-border-width 2
             :internal-border-color "#c792ea"
             :override-parameters '((parent-frame . nil)))))))
;; -----------------------------------------------------------------------------
