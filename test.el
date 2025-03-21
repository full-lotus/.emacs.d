;;; org-code-block-tools.el --- Tools for org code block analysis

;; Author: Claude
;; Keywords: org-mode, literate programming

;;; Commentary:
;; This package provides tools for analyzing code blocks in org-mode documents.
;; It includes functions to:
;; - Build a graph of relationships between code blocks through noweb references
;; - Display these relationships in an org buffer for easy navigation
;; - Find the ultimate tangle destination of any code block

;;; Code:

(require 'org)
(require 'org-element)
(require 'cl-lib)

;;; Core Functions

(defun org-code-block-graph-buffer ()
  "Analyze the current org buffer and return an adjacency list of code block
relationships.
Each node represents a code block with a name derived from :noweb-ref or :tangle
headers. Edges represent noweb inclusion relationships (A -> B means B includes
A via noweb)."
  (save-excursion
    (save-restriction
      (widen)
      ;; All blocks with their properties
      (let ((blocks '())
            ;; The final graph representation
            (adjacency-list '()))
        
        ;; First pass: Collect all blocks with their relevant attributes
        (org-element-map (org-element-parse-buffer) 'src-block
          (lambda (src-block)
            (let* ((properties (org-element-property :parameters src-block))
                   (noweb-ref (when (string-match ":noweb-ref +\\([^ ]+\\)"
                                                 properties)
                                (match-string 1 properties)))
                   (tangle (when (string-match ":tangle +\\([^ ]+\\)"
                                              properties)
                             (match-string 1 properties)))
                   (body (org-element-property :value src-block))
                   (block-name (or noweb-ref tangle)))
              
              ;; Only include blocks that have a name (tangle or noweb-ref)
              (when block-name
                (push (list :name block-name 
                            :noweb-ref noweb-ref
                            :tangle tangle
                            :body body)
                      blocks)))))
        
        ;; Second pass: Find noweb references in each block
        (dolist (block blocks)
          (let ((block-name (plist-get block :name))
                (outgoing-edges '()))
            
            ;; Look for noweb references like <<name>> in the block body
            (dolist (ref-block blocks)
              (let ((ref-name (plist-get ref-block :noweb-ref)))
                ;; Only consider blocks with a noweb-ref
                (when (and ref-name 
                           ;; Check if this block's body contains a reference
                           (string-match (format "<<%s>>" ref-name)
                                        (plist-get block :body)))
                  ;; Found a reference - add an edge from ref-block to this block
                  (push ref-name outgoing-edges))))
            
            ;; Add this block to the adjacency list
            (push (cons block-name outgoing-edges) adjacency-list)))
        
        adjacency-list))))

(defun org-get-block-headers-at-point ()
  "Get the noweb-ref and tangle headers for the source block at point.
Returns a plist with :noweb-ref and :tangle properties, or nil if not in
a source block."
  (let ((element (org-element-at-point)))
    (when (eq (org-element-type element) 'src-block)
      (let* ((params (org-element-property :parameters element))
             (noweb-ref (when (string-match ":noweb-ref +\\([^ ]+\\)" params)
                          (match-string 1 params)))
             (tangle (when (string-match ":tangle +\\([^ ]+\\)" params)
                       (match-string 1 params))))
        (list :noweb-ref noweb-ref :tangle tangle)))))

(defun org-find-tangle-roots-for-block (block-name graph)
  "Find all root :tangle files that include BLOCK-NAME.
This function traverses the GRAPH to find all paths from the given block
to root nodes. A root node is one that has a :tangle header but is not
included by other blocks."
  (let ((result '())
        (visited '()))
    
    ;; Helper function to traverse the graph recursively
    (cl-labels 
        ((find-roots 
          (node)
          ;; Skip if we've already visited this node
          (unless (member node visited)
            (push node visited)
            
            ;; Find nodes that include the current node
            (let ((parents '()))
              (dolist (entry graph)
                (let ((parent-name (car entry))
                      (children (cdr entry)))
                  (when (member node children)
                    (push parent-name parents))))
              
              ;; If no parents found, this might be a root node
              (if (null parents)
                  ;; Check if this is a :tangle node (not a :noweb-ref only)
                  (when (not (org-is-noweb-ref-only node graph))
                    (push node result))
                ;; Recursively check all parents
                (dolist (parent parents)
                  (find-roots parent)))))))
      
      ;; Start traversal from the given block
      (find-roots block-name))
    
    ;; Return the list of root files
    result))

(defun org-is-noweb-ref-only (node-name graph)
  "Check if NODE-NAME only has a :noweb-ref without a :tangle header.
This examines all code blocks in the buffer to determine if the node has
both a :noweb-ref and :tangle, or just a :noweb-ref."
  (let ((is-noweb-ref-only t))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (and is-noweb-ref-only
                    (re-search-forward 
                     (format "\\(:noweb-ref\\|:tangle\\) +%s\\>" node-name)
                     nil t))
          (let ((element (org-element-at-point)))
            (when (eq (org-element-type element) 'src-block)
              (let* ((params (org-element-property :parameters element))
                     (has-tangle 
                      (string-match 
                       (format ":tangle +%s\\>" node-name) params)))
                ;; If we find the node has a :tangle header, it's not noweb-only
                (when has-tangle
                  (setq is-noweb-ref-only nil))))))))
    is-noweb-ref-only))

;;; Interactive Functions

;;;###autoload
(defun display-org-code-block-graph ()
  "Display the code block relationship graph for the current org buffer.
This function analyzes all code blocks in the current buffer that have
either a :noweb-ref or :tangle header, and shows how they're connected
through noweb references."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "This command only works in org-mode buffers"))
  
  (let ((graph (org-code-block-graph-buffer)))
    (if (null graph)
        (message "No code blocks with :noweb-ref or :tangle found in this buffer")
      (with-current-buffer (get-buffer-create "*Org Code Block Graph*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (org-mode)
          (insert "#+TITLE: Code Block Relationship Graph\n")
          (insert "#+AUTHOR: Generated by org-code-block-tools\n")
          (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
          
          ;; Insert graph information in org format
          ;; Reverse to maintain original order
          (dolist (node (reverse graph))
            (let ((node-name (car node))
                  (edges (cdr node)))
              (insert (format "* %s\n" node-name))
              (if edges
                  (progn
                    (insert "  Includes blocks:\n")
                    (dolist (edge edges)
                      (insert (format "  - [[#%s][%s]]\n" edge edge))))
                (insert "  No blocks included\n"))
              (insert "\n")))
          
          ;; Make buffer read-only with special mode for navigation
          (special-mode)
          (org-mode)
          (goto-char (point-min))
          (org-overview)
          (org-cycle-overview)
          (switch-to-buffer-other-window (current-buffer))))
      (message "Code block graph generated. Press TAB to expand/collapse nodes."))))

;;;###autoload
(defun org-find-tangle-root-for-current-block ()
  "Find the tangle destination for the source block at point.
For blocks with :noweb-ref, traces through the dependency graph to find root
:tangle files. For blocks with direct :tangle headers, returns that value."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "This command only works in org-mode buffers"))
  
  ;; Get the current source block's headers
  (let ((headers (org-get-block-headers-at-point)))
    (if (not headers)
        (message "Not in a source block")
      
      (let ((noweb-ref (plist-get headers :noweb-ref))
            (tangle (plist-get headers :tangle)))
        
        (cond
         ;; Case 1: Block has direct :tangle header
         (tangle
          (message "Block will be directly tangled to: %s" tangle))
         
         ;; Case 2: Block has :noweb-ref but no :tangle
         (noweb-ref
          ;; Build the graph and search for destinations
          (let* ((graph (org-code-block-graph-buffer))
                 (root-files (org-find-tangle-roots-for-block noweb-ref graph)))
            
            (cond
             ((null root-files)
              (message "No root :tangle file found for %s" noweb-ref))
             
             ((= (length root-files) 1)
              (message "Block %s will be tangled to: %s" 
                       noweb-ref
                       (car root-files)))
             
             (t
              (message "Block %s will be tangled to multiple files: %s"
                       noweb-ref
                       (mapconcat #'identity root-files ", "))))))
         
         ;; Case 3: No relevant headers
         (t
          (message "Block has neither :tangle nor :noweb-ref header")))))))

(defun org-cider-find-ns-from-tangle-file (filename)
  "Find the namespace in a Clojure file specified by FILENAME.
Attempts to parse the specified file and extract its namespace."
  (when (and filename (not (string-empty-p filename)))
    (with-temp-buffer
      (condition-case nil
          (progn
            (insert-file-contents filename)
            (clojure-mode)
            (clojure-find-ns))
        (error nil)))))

(defun org-cider-current-ns-advice (orig-fun &rest args)
  "Advice function for `cider-current-ns` that handles org-mode source blocks.
When in an org-mode source block, tries to determine the namespace from the
tangled destination file before falling back to the original function behavior.
ORIG-FUN is the original `cider-current-ns` function.
ARGS are the original arguments passed to `cider-current-ns`."
  (if (and (eq major-mode 'org-mode)
           (fboundp 'org-get-block-headers-at-point))
      (save-excursion
        (let* ((element (org-element-at-point))
               (is-src-block (eq (org-element-type element) 'src-block))
               (lang (when is-src-block 
                       (org-element-property :language element))))
          
          ;; Only proceed with Clojure-related blocks
          (if (and is-src-block 
                   (member lang '("clojure" "clj" "cljs" "cljc")))
              ;; Try to find the tangle destination
              (let ((headers (org-get-block-headers-at-point)))
                (if headers
                    (let ((tangle (plist-get headers :tangle))
                          (noweb-ref (plist-get headers :noweb-ref))
                          ns-from-tangle)
                      
                      ;; Case 1: Direct tangle header
                      (if tangle
                          (setq ns-from-tangle 
                                (org-cider-find-ns-from-tangle-file tangle))
                        
                        ;; Case 2: Noweb reference - use graph traversal
                        (when (and noweb-ref 
                                   (fboundp 'org-code-block-graph-buffer)
                                   (fboundp 'org-find-tangle-roots-for-block))
                          (let* ((graph (org-code-block-graph-buffer))
                                 (roots (org-find-tangle-roots-for-block 
                                         noweb-ref graph)))
                            ;; Just use the first root file we find
                            (when (car roots)
                              (setq ns-from-tangle
                                    (org-cider-find-ns-from-tangle-file 
                                     (car roots)))))))
                      
                      ;; Return the namespace or fall back to original function
                      (or ns-from-tangle (apply orig-fun args)))
                  
                  ;; No headers found
                  (apply orig-fun args)))
            
            ;; Not in a Clojure source block
            (apply orig-fun args))))
    
    ;; Not in org-mode, use original function
    (apply orig-fun args)))

;; Install the advice
(advice-add 'cider-current-ns :around #'org-cider-current-ns-advice)

(provide 'org-code-block-tools)
;;; org-code-block-tools.el ends here
