;; --------------- emacs customization ---------------
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
(setq-and-tell-customize package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
   ("non-gnu" . "https://elpa.nongnu.org/nongnu/")
   ("melpa" . "http://melpa.org/packages/")))

; activate all the packages
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(custom-set-variables
 '(package-selected-packages
   '(org-babel-eval-in-repl
     ob-clojurescript ob-async async paredit
     modus-themes openwith org-tidy cider treemacs-all-the-icons treemacs
     clojure-mode magit)))
;; -----------------------------------------------------------------------------



;; --------------- general global settings  ---------------
;; taken from Adam James' guide:
;; https://gist.github.com/adam-james-v/7a61612ce0649afc78513f54b337d8c9

;; separate custom-file because it's a mess, with everything in one place
(setq-and-tell-customize custom-file (concat user-emacs-directory "custom.el"))
;; we are loading at the top of init.el, to set custom variables nice and
;; separate, grouping relevant settings together
(load custom-file 'noerror)

(custom-set-variables
 ;; blinking cursor is annoying
 '(blink-cursor-mode nil)

 ;; save Emacs frame configuration on exit
 '(desktop-save t)
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
(scroll-bar-mode 0)
(cua-mode 1)

;; supposedly good defaults from here:
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#use-better-defaults
(load-theme 'modus-operandi-deuteranopia t)

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 auto-window-vscroll nil                          ; Lighten vertical scroll
 confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
 display-time-default-load-average nil            ; Don't display load average
 display-time-mode 0                              ; Display time in frames
 display-time-format "%H:%M"                      ; Format the time string
 fill-column 80                                   ; Set width for automatic line breaks
 scroll-bar-mode nil
 display-line-numbers-type nil
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Stop using tabs to indent
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 left-margin-width 1 right-margin-width 1         ; Add left and right margins
 mouse-yank-at-point t                            ; Yank at point rather than pointer
 ns-use-srgb-colorspace nil                       ; Don't use sRGB colors
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 show-trailing-whitespace nil                     ; Display trailing whitespaces
 split-height-threshold nil                       ; Disable vertical window splitting
 split-width-threshold 1                          ; Disable horizontal window splitting
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Resize windows proportionally
 x-stretch-cursor t                               ; Stretch cursor to the glyph width
 scroll-step 1
 scroll-conservatively 10000)

(delete-selection-mode 1)                         ; Replace region when inserting text
(display-time-mode 0)                             ; Enable time in the mode-line
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(menu-bar-mode 0)                                 ; Disable the menu bar
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding

;; Apparently Garbage Collecting when out of focus can make emacs feel faster. I’ll try that.
(add-hook 'focus-out-hook #'garbage-collect)
;; -----------------------------------------------------------------------------



;; --------------- programming settings  ---------------
;; -----------------------------------------------------------------------------



;; --------------- file management  ---------------
;; mode that enables choosing program-to-open-with based on file extensions
(require 'openwith)
(openwith-mode t)

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
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-b") 'treemacs)

(custom-set-variables
  ;; enable tab-bar-mode
  '(tab-bar-mode t)
  ;; enable tabs globally
  '(global-tab-line-mode t)
  ;; enable tab history
  '(tab-bar-history-mode t)
  ;; records window configuration changes, allowing to undo/redo the changes
  '(winner-mode t))

;; hotkeys to navigate tab history
(global-set-key (kbd "M-[") 'tab-bar-history-back)
(global-set-key (kbd "M-]") 'tab-bar-history-forward)


;; open treemacs when emacs starts
(add-hook 'emacs-startup-hook 'treemacs)
;; -----------------------------------------------------------------------------



;; --------------- org mode ---------------
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (clojure . t)))
;; Show syntax highlighting per language native mode in *.org
(setq-and-tell-customize 'org-src-fontify-natively t)
;; For languages with significant whitespace like Python:
(setq-and-tell-customize 'org-src-preserve-indentation t)

(custom-set-variables
 ;; do not display inline images when doing org-cycle
 '(org-cycle-inline-images-display nil)

 ;; open file links in another frame
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file-other-frame)
     (wl . wl-other-frame)))

 ;; disable org-mode indentation, to keep lines under 80 characters
 '(org-startup-indented nil)

 ;; not sure how this variable was set, decided not to mess with it
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus
             ol-info ol-irc ol-mhe ol-rmail org-tempo ol-w3m)))

;; make the windmove function active in locations where Org mode does not have
;; special functionality on S-<cursor>
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)


(defun my/org-checkbox-todo ()
  "Switch header TODO state to DONE when all checkboxes are ticked,
to TODO otherwise"
  (let ((todo-state (org-get-todo-state)) beg end)
    (unless (not todo-state)
      (save-excursion
    (org-back-to-heading t)
    (setq-and-tell-customize 'beg (point))
    (end-of-line)
    (setq-and-tell-customize 'end (point))
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
     (setq-and-tell-customize 'mytmpid (funcall 'org-id-get-create))
     (kill-new mytmpid)
     (message "Copied %s to killring (clipboard)" mytmpid)
       ))

(global-set-key (kbd "<f5>") 'my/copy-id-to-clipboard)
;; -----------------------------------------------------------------------------
