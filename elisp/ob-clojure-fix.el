(load "ob-clojure.el")

(defun org-babel-expand-body:clojure_fixed (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* ((vars (org-babel--get-vars params))
         (backend-override (cdr (assq :backend params)))
         (org-babel-clojure-backend
          (cond
           (backend-override (intern backend-override))
           (org-babel-clojure-backend org-babel-clojure-backend)
           (t (user-error "You need to customize `org-babel-clojure-backend'
or set the `:backend' header argument"))))
	 (ns (or (cdr (assq :ns params))
		 (if (eq org-babel-clojure-backend 'cider)
		     (or cider-buffer-ns
			 (let ((repl-buf (cider-current-connection)))
			   (and repl-buf (buffer-local-value
					  'cider-buffer-ns repl-buf))))
		   org-babel-clojure-default-ns)))
	 (result-params (cdr (assq :result-params params)))
	 (print-level nil)
	 (print-length nil)
	 ;; Remove comments, they break (let [...] ...) bindings
	 ;; (body (replace-regexp-in-string "^[ 	]*;+.*$" "" body))
	 (body (org-trim
		(concat
		 ;; Source block specified namespace :ns.
		 (and (cdr (assq :ns params)) (format "(ns %s)\n" ns))
		 ;; Variables binding.
		 (if (null vars) (org-trim body)
		   (format "(let [%s]\n%s)"
			   (mapconcat
			    (lambda (var)
			      (format "%S '%S" (car var) (cdr var)))
			    vars
			    "\n      ")
			   body))))))
    (if (or (member "code" result-params)
	    (member "pp" result-params))
	(format "(clojure.pprint/pprint (do %s))" body)
      body)))
