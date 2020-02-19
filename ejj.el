;;
;; Emacs Junk for Java
;;

(defun initialize-java-class-list ()
  (with-temp-buffer
    (insert-file-contents "/Users/nealsid/jre-classes.txt")
    (setq java-classes (split-string (buffer-string) "\n"))))

(defun add-java-class (use-killring)
  (interactive
   (cond
    ((equal current-prefix-arg '(4))
     (list t))
    (t (list nil))))

  (insert (ido-completing-read "class> " java-classes nil nil (if use-killring (current-kill 0) nil))))

(initialize-java-class-list)
(global-set-key (kbd "M-J") 'add-java-class)

(defun add-try-catch (catch-clause-count)
  (interactive
   (cond
    ((numberp current-prefix-arg)
     (list current-prefix-arg))
    (t (list 1))))
  (save-excursion
    (goto-char (region-beginning))
    (insert "try {
"))
  (goto-char (region-end))
  (insert (apply 'concat (make-list catch-clause-count "} catch () {
")))
  (insert "}
")
  (save-excursion
    (indent-region (region-beginning) (region-end))))

(global-set-key (kbd "C-c j t") 'add-try-catch)

(defun return-import-for-symbol-at-point ()
  (interactive)
  (let* ((cur-symbol (thing-at-point 'symbol t))
	 (results (seq-filter (apply-partially 'cl-search cur-symbol)
			      java-classes))
	 (resultslen (length results)))
    (cond
     ((eq 0 resultslen) (message "Symbol not found"))
     ((eq 1 resultslen) (seq-elt results 0))
     (t (ido-completing-read "choose> " results)))))

(defun find-region-of-import-statements()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^import")
    (beginning-of-line)
    (let ((import-beginning (point)))
      (goto-char (point-max))
      (re-search-backward "^import")
      (end-of-line)
      (list import-beginning (point)))))

;; Need to remove duplicates from import list after adding.
(defun add-import-for-symbol-at-point ()
  (interactive)
  (let ((import (return-import-for-symbol-at-point)))
    (add-import-and-sort import)
    (message "Added %s" import)))

(defun add-import-and-sort (java-class-name)
  (interactive)
  (save-excursion
    (let* ((import-markers (find-region-of-import-statements))
	   (imports-beginning (elt import-markers 0))
	   (imports-end (elt import-markers 1)))
      (goto-char imports-end)
      (insert (concat "
import " java-class-name ";"))
      (sort-lines nil imports-beginning (point)))))