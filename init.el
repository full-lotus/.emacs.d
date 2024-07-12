;; --------------- package management ---------------
(setq package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
   ("non-gnu" . "https://elpa.nongnu.org/nongnu/")
   ("melpa" . "http://melpa.org/packages/")))
(package-initialize)



;; --------------- window/tab navigation ---------------
(global-set-key (kbd "M-o") 'other-window)

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
(setq org-src-fontify-natively t)
;; For languages with significant whitespace like Python:
(setq org-src-preserve-indentation t)


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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes '(leuven))
 '(desktop-save t)
 '(desktop-save-mode t)
 '(display-fill-column-indicator-column 80)
 '(fido-mode t)
 '(global-display-fill-column-indicator-mode t)
 '(global-tab-line-mode t)
 '(ispell-dictionary nil)
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
   '(org-tidy cider treemacs-all-the-icons treemacs clojure-mode magit))
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
