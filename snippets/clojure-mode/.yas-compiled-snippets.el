;;; Compiled snippets and support files for `clojure-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'clojure-mode
										 '(("<clj-snitch"
												"(comment\n  (require '[snitch.core :refer [defn* defmethod* *fn *let]]) $0\n\n  (require '[portal.api :as p])\n  (def p (portal.api/open))\n  (add-tap #'portal.api/submit) ; Add portal as a tap> target\n\n  (tap> :hello) ; Start tapping out values\n  (portal.api/clear) ; Clear all values\n  (tap> :world) ; Tap out more values\n  (prn @p) ; bring selected value back into repl\n\n  (remove-tap #'portal.api/submit) ; Remove portal from tap> targetset\n  (portal.api/close) ; Close the inspector when done\n  )\n"
												"clojure snitch and portal" nil nil nil
												"/home/german/.emacs.d/snippets/clojure-mode/clojure-snitch-portal"
												nil nil)))


;;; Do not edit! File generated at Fri Nov  7 11:48:05 2025
