;; Sync init.el changes to customize
;; I use Emacs Customize GUI, because it's convenient. I want to see actual info
;; there, that's why I use this fn instead of =setq=


;; [[file:init.org::*Sync init.el changes to customize][Sync init.el changes to customize:1]]
(defun setq-mark-as-customized (&rest var-val-pairs)
  "Set variable value and make sure Customize registers this change. Use
instead of setq, to avoid confusion in Customize interface"
  (message "%s" var-val-pairs)
  (while var-val-pairs
    (let ((var (pop var-val-pairs))
          (val (pop var-val-pairs)))
      (set var val)
      (put var 'customized-value
           (list (custom-quote (symbol-value var)))))))
;; Sync init.el changes to customize:1 ends here

;; Separate custom.el from init.el

;; [[file:init.org::*Separate custom.el from init.el][Separate custom.el from init.el:1]]
;; separate custom-file because it's a mess, with everything in one place
(setq-mark-as-customized 'custom-file (concat user-emacs-directory "custom.el"))

;; we are loading at the top of init.el, to set custom variables nice and
;; separate, grouping relevant settings together
(load custom-file 'noerror)
;; Separate custom.el from init.el:1 ends here

;; Set URLs from which we search and download packages

;; [[file:init.org::*Set URLs from which we search and download packages][Set URLs from which we search and download packages:1]]
(setq-mark-as-customized
 'package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
   ("non-gnu" . "https://elpa.nongnu.org/nongnu/")
   ("melpa" . "http://melpa.org/packages/")))
;; Set URLs from which we search and download packages:1 ends here

;; Init package.el, fetch available packages from URLs

;; [[file:init.org::*Init package.el, fetch available packages from URLs][Init package.el, fetch available packages from URLs:1]]
;; start package system (package.el, package dirs, available packages cache)
(package-initialize)

;; fetch the list of packages available, must run package-initialize first
(unless package-archive-contents
  (package-refresh-contents))
;; Init package.el, fetch available packages from URLs:1 ends here

;; Install and load all non-default packages

;; [[file:init.org::*Install and load all non-default packages][Install and load all non-default packages:1]]
;; add non-default packages that we need
(custom-set-variables
 '(package-selected-packages
   '(ivy counsel smex wgrep
     ob-clojurescript ob-async async wgrep
     openwith org-tidy cider treemacs-all-the-icons treemacs
     clojure-mode magit command-log-mode posframe pcre2el dired-ranger
     flycheck flycheck-clj-kondo rainbow-delimiters casual-avy puni
     poly-org
     )))

;; install all non-default packages
(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))


;; require all non-default packages
(dolist (pkg package-selected-packages)
  (require pkg nil 'noerror))
;; Install and load all non-default packages:1 ends here

;; Emacs startup/exit

;; [[file:init.org::*Emacs startup/exit][Emacs startup/exit:1]]
(setq-mark-as-customized
 ;; Don't display load average
 'display-time-default-load-average nil

 ;; Disable start-up screen
 'inhibit-startup-screen t

 ;; Empty the initial *scratch* buffer
 'initial-scratch-message ""

 ;; Confirm before exiting Emacs
 'confirm-kill-emacs 'yes-or-no-p
 )
;; Emacs startup/exit:1 ends here

;; Elisp

;; [[file:init.org::*Elisp][Elisp:1]]
;; the dir with elisp files to be loaded
(add-to-list 'load-path "~/.emacs.d/elisp/")

(setq-mark-as-customized
 ;; Silence warnings for redefinition
 'ad-redefinition-action 'accept
 )

(defmacro comment (&rest body)
  "A simple way to comment out code in Emacs Lisp."
  nil)
;; Elisp:1 ends here

;; Use SSH through daemon launched on startup

;; [[file:init.org::*Use SSH through daemon launched on startup][Use SSH through daemon launched on startup:1]]
(setenv "SSH_AUTH_SOCK" (concat (getenv "XDG_RUNTIME_DIR") "/ssh-agent.socket"))
;; Use SSH through daemon launched on startup:1 ends here

;; Sound

;; [[file:init.org::*Sound][Sound:1]]
;; use screen flash instead of annoying error sound
(setq-mark-as-customized
 'visible-bell t
 )
;; Sound:1 ends here

;; History

;; [[file:init.org::*History][History:1]]
;; save command history, e. g. for execute-extended-command
(savehist-mode 1)
;; use smex to show command history in counsel-M-x
(smex-initialize)
(setq-mark-as-customized 'smex-history-use-recent-first t)
;; History:1 ends here

;; Command/key press log

;; [[file:init.org::*Command/key press log][Command/key press log:1]]
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
;; Command/key press log:1 ends here

;; Easier yes/no

;; [[file:init.org::*Easier yes/no][Easier yes/no:1]]
;; Replace yes/no prompts with y/n
(fset 'yes-or-no-p 'y-or-n-p)
;; Easier yes/no:1 ends here

;; Easy (1 button) command repeat

;; [[file:init.org::*Easy (1 button) command repeat][Easy (1 button) command repeat:1]]
;; make some multi-chord commands repeatable with 1 key press
;; for example - repeat C-x <LEFT> (previous-buffer) with just <LEFT>
(repeat-mode 1)
;; Easy (1 button) command repeat:1 ends here

;; Emacs temporary files

;; [[file:init.org::*Emacs temporary files][Emacs temporary files:1]]
;; stop creating backup~ files
(setq-mark-as-customized 'make-backup-files nil)

;; stop creating #autosave# files
(setq-mark-as-customized 'auto-save-default nil)

;; no lockfiles
(setq-mark-as-customized 'create-lockfiles nil)

;; Don't clutter main Emacs folder with session. files
(defun emacs-session-filename (session-id)
  "Return the file name of the session file for SESSION-ID."
  (expand-file-name (concat "session." session-id)
                    "~/.emacs.d/sessions/"))
;; Emacs temporary files:1 ends here

;; Navigation

;; [[file:init.org::*Navigation][Navigation:1]]
;; use avy through Transient menu
(keymap-global-set "M-g" #'casual-avy-tmenu)

(keymap-global-set "M-SPC" #'avy-goto-word-0)
;; Navigation:1 ends here

;; Display

;; [[file:init.org::*Display][Display:1]]
;; blinking cursor is annoying
(blink-cursor-mode -1)


;; Default to utf-8 encoding
(set-default-coding-systems 'utf-8)

(setq-mark-as-customized
 ;; to see when a line is longer than 80 symbols
 'display-fill-column-indicator-column 80

 ;; set width for automatic line breaks
 'fill-column 80

 ;; Add left and right margins
 'left-margin-width 1 'right-margin-width 1

 ;; don't display line numbers at their beginning
 ;; 'display-line-numbers-type nil

 ;; show column number in minibuffer
 'column-number-mode t

 ;; Display trailing whitespaces
 'show-trailing-whitespace nil

 ;; Set width for tabs
 'tab-width 4

 ;; Stretch cursor to the glyph width
 'x-stretch-cursor t
 )

(global-display-fill-column-indicator-mode t)
;; Display:1 ends here

;; Editing

;; [[file:init.org::*Editing][Editing:1]]
;; make undo hotkey familiar
(global-set-key (kbd "C-z") 'undo)

;; delete active region on yanking (paste)
(delete-selection-mode 1)

(setq-mark-as-customized
 ;; saves OS clipboard content before kill, to prevent overwriting it
 'save-interprogram-paste-before-kill t

 ;; sync system's and Emacs' clipboard
 'select-enable-clipboard t

 ;; Stop using tabs to indent
 'indent-tabs-mode nil

 ;; Yank at point rather than pointer
 'mouse-yank-at-point t

 ;; End a sentence after a dot and a space
 'sentence-end-double-space nil
 )


;; delete all trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Editing:1 ends here

;; Syncing with filesystem (reverting)

;; [[file:init.org::*Syncing with filesystem (reverting)][Syncing with filesystem (reverting):1]]
;; The auto-revert setting is enabled because tangle / detangle for literate
;; programming will change contents of files. If the file is open in a buffer,
;; I want it to automatically show the change without asking me every time.
(global-auto-revert-mode t)


;; When a file is modified externally, emacs does not show this change
;; by default. Instead, when you try to edit it will ask you to modify or
;; revert. Since Tangling files changes src code automatically, it is more
;; effective to automatically revert any buffers which have src files open.
;; (defun revert-all-buffers ()
;;   "Refreshes all open buffers from their respective files."
;;   (interactive)
;;   (dolist (buf (buffer-list))
;;     (with-current-buffer buf
;;       (when (and (buffer-file-name)
;; 		 (file-exists-p (buffer-file-name))
;; 		 (not (buffer-modified-p)))
;; 	(revert-buffer t t t) )))
;;   (message "Refreshed open files."))
;; (add-hook 'after-save-hook 'revert-all-buffers)
;; Syncing with filesystem (reverting):1 ends here

;; Hide

;; [[file:init.org::*Hide][Hide:1]]
;; hide useless title bar, but allow resizing
(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))

;; I don't use toolbar
(tool-bar-mode 0)

(setq-mark-as-customized
 ;; don't display time in frames
 'display-time-mode 0

 ;; don't use sRGB colors
 'ns-use-srgb-colorspace nil
 )
;; Hide:1 ends here

;; Splitting/resizing

;; [[file:init.org::*Splitting/resizing][Splitting/resizing:1]]
(setq-mark-as-customized
 ;; Disable vertical window splitting
 'split-height-threshold nil

 ;; Disable horizontal window splitting
 'split-width-threshold 1

 ;; Resize windows proportionally
 'window-combination-resize t
 )
;; Splitting/resizing:1 ends here

;; Creating buffers and choosing how to display them

;; [[file:init.org::*Creating buffers and choosing how to display them][Creating buffers and choosing how to display them:1]]
(custom-set-variables
 '(winner-mode t))

(defun my/switch-buffer-here ()
  "Switch buffer in the current window, ignoring `display-buffer-alist`."
  (interactive)
  (let ((display-buffer-alist nil)) ;; Temporarily disable display-buffer-alist
    (call-interactively 'switch-to-buffer)))

(global-set-key (kbd "C-x b") #'my/switch-buffer-here)

(setq-mark-as-customized
 ;; Uniquify buffer names
 'uniquify-buffer-name-style 'forward

 ;; Focus new help windows when opened
 'help-window-select t

 ;; don't open new buffers when navigating dirs in dired
 'dired-kill-when-opening-new-dired-buffer t

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
;; Creating buffers and choosing how to display them:1 ends here

;; Scrolling

;; [[file:init.org::*Scrolling][Scrolling:1]]
(scroll-bar-mode 1) ;; enable vertical scroll bars
(horizontal-scroll-bar-mode 1) ;; enable horizontal scroll bars

(setq-mark-as-customized
 ;; Lighten vertical scroll
 'auto-window-vscroll nil

 'scroll-step 1
 'scroll-conservatively 10000
 )
;; Scrolling:1 ends here

;; Save/load frame configuration on exit/start

;; [[file:init.org::*Save/load frame configuration on exit/start][Save/load frame configuration on exit/start:1]]
(desktop-save-mode t)
;; Save/load frame configuration on exit/start:1 ends here

;; Navigation

;; [[file:init.org::*Navigation][Navigation:1]]
(global-set-key (kbd "C-o") 'other-window)
(define-key dired-mode-map (kbd "C-o") 'other-window)
(define-key ivy-occur-mode-map (kbd "C-o") 'other-window)
(define-key ivy-occur-grep-mode-map (kbd "C-o") 'other-window)
(global-set-key (kbd "M-o") 'other-frame)
(global-set-key (kbd "C-S-b") 'treemacs)
(global-set-key (kbd "C-x w") 'bury-buffer)

(defun modi/multi-pop-to-mark (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))
(advice-add 'pop-to-mark-command :around
            #'modi/multi-pop-to-mark)

(setq set-mark-command-repeat-pop t)
;; Navigation:1 ends here

;; Enable ivy and counsel modes

;; [[file:init.org::*Enable ivy and counsel modes][Enable ivy and counsel modes:1]]
;; use Ivy + Counsel + Swiper for better completion/search
;; settings taken from here https://github.com/abo-abo/swiper
(ivy-mode)
(counsel-mode)
(setq-mark-as-customized 'ivy-use-virtual-buffers t)
(setq-mark-as-customized 'enable-recursive-minibuffers t)
;; Enable ivy and counsel modes:1 ends here

;; Disable icomplete mode when calling ivy-read, to fix a conflict

;; [[file:init.org::*Disable icomplete mode when calling ivy-read, to fix a conflict][Disable icomplete mode when calling ivy-read, to fix a conflict:1]]
;; disable icomplete to fix error:
;; Error in post-command-hook (icomplete-post-command-hook):
;; (wrong-number-of-arguments #<subr counsel-ag-function> 3)
(defun ivy-icomplete (f &rest r)
  (icomplete-mode -1)
  (unwind-protect
      (apply f r)
    (icomplete-mode 1)))

(advice-add 'ivy-read :around #'ivy-icomplete)
;; Disable icomplete mode when calling ivy-read, to fix a conflict:1 ends here

;; Fix counsel-rg not displaying errors properly

;; [[file:init.org::*Fix counsel-rg not displaying errors properly][Fix counsel-rg not displaying errors properly:1]]
(with-eval-after-load 'counsel
  (advice-add
   'counsel-rg
   :around
   (lambda (func &rest args)
     (cl-flet ((filter-func (code) (if (= code 2) 0 code)))
       (unwind-protect
           (progn (advice-add 'process-exit-status :filter-return #'filter-func)
                  (apply func args))
         (advice-remove 'process-exit-status #'filter-func))))))
;; Fix counsel-rg not displaying errors properly:1 ends here

;; Customize ivy matching behavior

;; [[file:init.org::*Customize ivy matching behavior][Customize ivy matching behavior:1]]
;; Enable orderless matching for execute-extended-command
(setq-mark-as-customized 'ivy-re-builders-alist
                         '((counsel-M-x . ivy--regex-ignore-order)
                           (t . ivy--regex-plus)))

;; Drop beginning-of-string anchor ^ from execute-extended-command
(with-eval-after-load 'ivy
  (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) ""))

;; allow selecting the prompt itself as command candidate
(setq-mark-as-customized 'ivy-use-selectable-prompt t)
;; Customize ivy matching behavior:1 ends here

;; Add commands for searching the current symbol under cursor

;; [[file:init.org::*Add commands for searching the current symbol under cursor][Add commands for searching the current symbol under cursor:1]]
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
;; Add commands for searching the current symbol under cursor:1 ends here

;; Configure ivy and counsel hotkeys



;; [[file:init.org::*Configure ivy and counsel hotkeys][Configure ivy and counsel hotkeys:1]]
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
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
;; Configure ivy and counsel hotkeys:1 ends here

;; Code completion

;; [[file:init.org::*Code completion][Code completion:1]]
;; Always show completions
(setq-mark-as-customized 'completion-auto-help 'always)

;; Auto-select *Completions* buffer
(setq-mark-as-customized 'completion-auto-select 'second-tab)

;; Make <TAB> invoke completions list, when code is already idented properly
(setq-mark-as-customized 'tab-always-indent 'complete)
;; Code completion:1 ends here

;; Wgrep

;; [[file:init.org::*Wgrep][Wgrep:1]]
;; wgrep allows to convert ivy-occur buffer to editable, to get VS Code-like
;; search and replace experience
(use-package wgrep
  :ensure t
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))
;; Wgrep:1 ends here

;; Replace+.el
;; Allows to invoke replace fn with a search regex in the list of defaults.
;; The defaults are cycled in minibuffer with up/down arrows.


;; [[file:init.org::*Replace+.el][Replace+.el:1]]
(load "~/.emacs.d/elisp/replace+.el")

;; this prevents replace+ being limited to an active region
(setq-mark-as-customized 'search/replace-region-as-default-flag t)

(defun get-initial-input-for-replace ()
  nil)
(setq-mark-as-customized 'search/replace-default-fn
                         'get-initial-input-for-replace)

(defun query-replace-regexp-with-initial-input (input)
  (eval
   '(let ((original-fn (symbol-function 'initial-input-for-replace)))
      (fset 'get-initial-input-for-replace (lambda () (regexp-quote input)))
      (unwind-protect
          (call-interactively 'query-replace-regexp)
        (fset 'get-initial-input-for-replace original-fn)))
   t))
;; Replace+.el:1 ends here

;; Start replace with preview in one keystroke

;; [[file:init.org::*Start replace with preview in one keystroke][Start replace with preview in one keystroke:1]]
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

(global-set-key (kbd "C-S-h") 'rapid-replace-ripgrep)
;; Start replace with preview in one keystroke:1 ends here

;; Syntax checking

;; [[file:init.org::*Syntax checking][Syntax checking:1]]
;; check syntax globally
(global-flycheck-mode)
;; Syntax checking:1 ends here

;; Puni-mode

;; [[file:init.org::*Puni-mode][Puni-mode:1]]
;; use sexp editing hotkeys from VS Code Calva, to which I am used to
(define-key puni-mode-map (kbd "C-<right>") 'puni-forward-sexp)
(define-key puni-mode-map (kbd "C-<left>") 'puni-backward-sexp)
(define-key puni-mode-map (kbd "C-M-.") 'puni-slurp-forward)
(define-key puni-mode-map (kbd "C-M-,") 'puni-barf-forward)
(define-key puni-mode-map (kbd "C-M->") 'puni-barf-backward)
(define-key puni-mode-map (kbd "C-M-<") 'puni-slurp-backward)

;; toggle puni-mode manually
(define-key org-mode-map (kbd "M-P") 'puni-mode)

;; use puni-mode with Clojure
(add-hook 'clojurescript-mode #'puni-mode)
(add-hook 'clojure-mode #'puni-mode)
(add-hook 'cider-mode-hook #'puni-mode)
(add-hook 'cider-repl-mode-hook #'puni-mode)
;; Puni-mode:1 ends here

;; Display

;; [[file:init.org::*Display][Display:1]]
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; always highlight matching paren
(show-paren-mode 1)
;; Display:1 ends here

;; CIDER

;; [[file:init.org::*CIDER][CIDER:1]]
(setq-mark-as-customized
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

;; use CIDER in every Clojure buffer
(add-hook 'clojure-mode-hook #'cider-mode)
(add-hook 'clojurescript-mode-hook #'cider-mode)

(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'imenu-add-menubar-index)

;; I write tests in the same ns as source code, source and tests must be close
(setq-mark-as-customized 'cider-test-infer-test-ns (lambda (ns) ns))

;; use the same hotkeys as in VS Code Calva
(define-key cider-repl-mode-map [C-return] nil)
(define-key cider-mode-map [C-return] 'cider-eval-sexp-at-point)
(define-key cider-mode-map (kbd "M-<RET>") 'cider-eval-defun-at-point)
(define-key cider-repl-mode-map [C-return] 'cider-eval-sexp-at-point)
(define-key cider-repl-mode-map (kbd "M-<RET>") 'cider-eval-defun-at-point)
;; CIDER:1 ends here

;; Initialization

;; [[file:init.org::*Initialization][Initialization:1]]
  ;; (require 'org)

  (eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((clojure . t)
       (clojurescript . t)
       (emacs-lisp . t)
       (shell . t))))

  (setq-mark-as-customized
   'org-startup-folded nil

   ;; not sure how/why this variable was set, decided not to mess with it
   'org-modules '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus
  			ol-info ol-irc ol-mhe ol-rmail org-tempo ol-w3m)

   ;; protection of overlay messes up editing sometimes
   'org-tidy-protect-overlay nil

   ;; disable org-mode indentation, to keep lines under 80 characters
   'org-startup-indented nil)
;; Initialization:1 ends here

;; Display

;; [[file:init.org::*Display][Display:1]]
;; hide :PROPERTIES: and other drawers
;; (use-package org-tidy
;;   :ensure t
;;   :hook
;;   (org-mode . org-tidy-mode))
;; Display:1 ends here

;; Navigation

;; [[file:init.org::*Navigation][Navigation:1]]
(global-set-key (kbd "M--") #'org-mark-ring-goto)
(global-set-key (kbd "M-=") #'org-mark-ring-push)


(setq-mark-as-customized
 ;; open src blocks in the same window as parent .org file
 'org-src-window-setup 'current-window

 ;; do not display inline images when doing org-cycle
 'org-cycle-inline-images-display nil

 'org-link-frame-setup '((vm . vm-visit-folder-other-frame)
			 (vm-imap . vm-visit-imap-folder-other-frame)
			 (gnus . org-gnus-no-new-news)
			 ;; open file links in another frame
			 (file . find-file-other-frame)
			 (wl . wl-other-frame)))

;; Remove the function which causes text to pop around when pressing tab.
;; This is annoying and confusing.
(remove-hook 'org-cycle-hook
             'org-optimize-window-after-visibility-change)

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

;; open the file specified by the :tangle header argument
(defun org-babel-open-tangle-file ()
  (interactive)
  (let* ((info (org-babel-get-src-block-info))
         (tangle (cdr (assoc :tangle (nth 2 info))))
	 (src (cdr (assoc :src (nth 2 info)))))
    (if (not (equal tangle "no"))
	(find-file tangle)
      (when src (find-file src)))))
(define-key org-mode-map (kbd "C-c o") 'org-babel-open-tangle-file)
;; Navigation:1 ends here

;; Editing

;; [[file:init.org::*Editing][Editing:1]]
(setq-mark-as-customized
 'org-support-shift-select 'always
 'org-hide-emphasis-markers nil
 'org-edit-src-content-indentation 0
 'org-src-tab-acts-natively t

 ;; Show syntax highlighting per language native mode in *.org
 'org-src-fontify-natively t

 ;; For languages with significant whitespace like Python:
 'org-src-preserve-indentation t
 )

;; Trying to fix weird org syntax problems. This just lets Org ignore < and >
;; characters as if they were regular words. This is necessary because in
;; Clojure I want to make functions with -> in the name and Org was always
;; insisting on pairing <>. This caused any other paren matching to stop
;; working. It sucked.
(defun my-angle-bracket-fix ()
  (modify-syntax-entry ?< "w")
  (modify-syntax-entry ?> "w"))
(add-hook 'org-mode-hook 'my-angle-bracket-fix)

;; It’s useful to split code blocks to quickly add org-mode text
;; between the src. The default binding is C-c C-v C-d, which is somewhat
;; annoying. I think M-s in org-mode should do the trick.

;; Split Org Block using M-s
(define-key org-mode-map (kbd "M-s") 'org-babel-demarcate-block)
;; Editing:1 ends here

;; Eval in Cider REPL with proper namespace
;; To figure out the namespace of a code block:

;; 1. We create a graph, where edges are noweb refs.
;; 2. Using the graph, we find root code block, which points to source file
;; 3. In the source file, we find =(ns ...)= macro, which gets us a namespace.
;;    To do that, we =(advice-add 'cider-current-ns)== - if we are
;;    inside :noweb-ref block, ns search function is called on the :tangle file


;; [[file:init.org::*Eval in Cider REPL with proper namespace][Eval in Cider REPL with proper namespace:1]]
;; use Cider REPL to eval Clojure code in org-mode
(load "~/.emacs.d/elisp/cider-eval-in-org-mode.el")
;; Eval in Cider REPL with proper namespace:1 ends here

;; Babel settings *DISABLED*

;; [[file:init.org::*Babel settings *DISABLED*][Babel settings *DISABLED*:1]]
;; not sure if I need the code below since I'm using straight-up CIDER, not
;; Babel, to execute Clojure in org-mode

;; (defun org-babel-clojure-cider-current-ns ())
;; 'org-confirm-babel-evaluate nil

;; ;; Sets M-<return> to evaluate code blocks in the REPL
;; (defun org-meta-return-around (org-fun &rest args)
;;   "Run `ober-eval-in-repl' if in source code block,
;;   `ober-eval-block-in-repl' if at header,
;;   and `org-meta-return' otherwise."
;;     (if (org-in-block-p '("src"))
;;         (let* ((point (point))
;;                (element (org-element-at-point))
;;                (area (org-src--contents-area element))
;;                (beg (copy-marker (nth 0 area))))
;;           (if (< point beg)
;;               (ober-eval-block-in-repl)
;;             (ober-eval-in-repl)))
;;       (apply org-fun args)))

;; (advice-add 'org-meta-return :around #'org-meta-return-around)


;; ;; Prevent eval in repl from moving cursor to the REPL
;; (with-eval-after-load "eval-in-repl"
;;   (setq eir-jump-after-eval nil))
;; Babel settings *DISABLED*:1 ends here

;; Tangling

;; [[file:init.org::*Tangling][Tangling:1]]
;; Tangling can be set to occur automatically on save. This makes things way
;; simpler. Additionally, we set up todos to be moved to the agenda on save.
;; This is just to keep things organized if todos are added to project org
;; files

;; Tangle on save only occurs if the buffer being saved is an Org-Mode file.
(defun tangle-on-save-org-mode-file ()
  (when (and (string-match-p
              (regexp-quote ".org") (message "%s" (current-buffer)))
             (not (string-match-p
                   (regexp-quote "[") (message "%s" (current-buffer)))))
    (org-babel-tangle)))
(add-hook 'after-save-hook 'tangle-on-save-org-mode-file)

;; disable new buffer pop-up caused by tangling
(defun org-babel-tangle-no-buffer-pop-up (orig-fun &rest args)
  (save-excursion
    (let ((display-buffer-alist
           '((".*" (display-buffer-no-window) (allow-no-window . t)))))
      (apply orig-fun args))))
(advice-add 'org-babel-tangle :around 'org-babel-tangle-no-buffer-pop-up)

;; override broken fn, which deletes comments from clojure code
(load "~/.emacs.d/elisp/ob-clojure-fix.el")
(eval-after-load 'org
  (with-eval-after-load 'org
    (advice-add 'org-babel-expand-body:clojure
		:override #'org-babel-expand-body:clojure_fixed)))

(defun dont-tangle-in-source-blocks (orig-fun &rest args)
  "Disable poly-org when tangling, to avoid triggering a bug.
See issue https://github.com/polymode/poly-org/issues/53
This function:
1. Disables poly-mode if we are inside org source block
2. Executes the original `org-babel-tangle` function
3. Restores poly-org-mode, font-lock, scroll position, and removes a mark"
  (let ((scroll-pos (window-start)))
    (if (org-in-src-block-p)
	(progn
	  (poly-org-mode -1)
	  (unwind-protect
	      (comment (apply orig-fun args))
	    (poly-org-mode t)
	    (font-lock-fontify-buffer)
	    (set-mark nil)
	    (set-window-start (selected-window) scroll-pos)
	    ))
      (apply orig-fun args))))

(advice-add 'org-babel-tangle :around #'dont-tangle-in-source-blocks)
;; Tangling:1 ends here

;; Detangling *DISABLED*

;; [[file:init.org::*Detangling *DISABLED*][Detangling *DISABLED*:1]]
;; ;; prevent window/frame config changes during detangling
;; (defun org-babel-detangle-no-buffer-pop-up (orig-fun &rest args)
;;   (save-excursion
;;     (let ((display-buffer-alist
;;            '((".*" (display-buffer-no-window) (allow-no-window . t)))))
;;       (apply orig-fun args))))

;; (advice-add 'org-babel-detangle :around #'org-babel-detangle-no-buffer-pop-up)

;; ;; detangle on each file save, doing this by hand is tedious
;; (add-hook 'after-save-hook 'org-babel-detangle)
;; Detangling *DISABLED*:1 ends here

;; Performance optimizations *DISABLED*

;; [[file:init.org::*Performance optimizations *DISABLED*][Performance optimizations *DISABLED*:1]]
;; ;; Apparently Garbage Collecting when out of focus can make emacs feel faster. I’ll try that.
;; (add-hook 'focus-out-hook #'garbage-collect)

;; ;; The following code is from:
;; ;; https://www.wisdomandwonder.com/article/10630/how-fast-can-you-tangle-in-org-mode
;; ;; It basically boils down to adjusting garbage collection settings at key times during an org file save. Not strictly necessary, but nice to have.

;; (setq help/default-gc-cons-threshold gc-cons-threshold)
;; (defun help/set-gc-cons-threshold (&optional multiplier notify)
;;   "Set `gc-cons-threshold' either to its default value or a
;;    `multiplier' thereof."
;;   (let* ((new-multiplier (or multiplier 1))
;;          (new-threshold (* help/default-gc-cons-threshold
;;                            new-multiplier)))
;;     (setq gc-cons-threshold new-threshold)
;;     (when notify (message "Setting `gc-cons-threshold' to %s"
;;                           new-threshold))))
;; (defun help/double-gc-cons-threshold () "Double `gc-cons-threshold'." (help/set-gc-cons-threshold 2))
;; (add-hook 'org-babel-pre-tangle-hook #'help/double-gc-cons-threshold)
;; (add-hook 'org-babel-post-tangle-hook #'help/set-gc-cons-threshold)
;; Performance optimizations *DISABLED*:1 ends here

;; Todo and agenda

;; [[file:init.org::*Todo and agenda][Todo and agenda:1]]
(defun to-agenda-on-save-org-mode-file ()
  (when (string= (message "%s" major-mode) "org-mode")
    (org-agenda-file-to-front)))

(add-hook 'after-save-hook 'to-agenda-on-save-org-mode-file)

(setq-mark-as-customized 'org-agenda-restore-windows-after-quit t)

(define-key org-mode-map (kbd "C-c a") 'org-agenda)

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
;; Todo and agenda:1 ends here

;; Permissions

;; [[file:init.org::*Permissions][Permissions:1]]
;; trying these setting to be able to edit files as root
(require 'tramp)
(setq tramp-default-method "sudoedit")
;; Permissions:1 ends here

;; Extensions

;; [[file:init.org::*Extensions][Extensions:1]]
;; mode that enables choosing program-to-open-with based on file extensions
(openwith-mode t)

(custom-set-variables
  ;; associate file extensions (regex) to program-to-open-with
  '(openwith-associations
    '(("\\.\\(doc\\|docx\\)\\'" "libreoffice.writer"
        (file))
      )))
;; Extensions:1 ends here
