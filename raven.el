;;; raven.el --- Efficient selection and navigation -*- lexical-binding: t -*-

;;; Commentary:

;; Utilities for selecting from lists of candidates in the minibuffer.
;; Stupidly simple.
;; Multiple sources, minus the bloat.

;;; Code:
(require 'dash)
(require 'subr-x)
(require 'seq)
(require 'recentf)
(when (featurep 'evil)
  (require 'evil))

(defgroup raven nil
  "Efficient selection and navigation."
  :group 'convenience)

(defgroup raven-faces nil
  "Faces for `raven'."
  :group 'raven
  :group 'faces)

(defface raven-source-name
  '((t :underline t
       :foreground "white"
       :weight bold))
  "Face used to highlight source names.")

(defface raven-highlight
  '((t :inherit highlight))
  "Face used to highlight the current selection.")

(defvar raven-minibuffer-lines 20
  "Number of lines to display in the minibuffer.")

(defvar raven--sources '())
(defvar raven--last nil)
(defvar raven--matching '())
(defvar raven--source 0)
(defvar raven--index 0)
(defvar raven--action nil)
(defvar raven--result nil)

(defvar raven-minibuffer-map (make-sparse-keymap))
(define-key raven-minibuffer-map (kbd "C-g") 'keyboard-escape-quit)
(define-key raven-minibuffer-map (kbd "C-c") 'keyboard-escape-quit)
(define-key raven-minibuffer-map (kbd "<return>") 'raven-do)
(define-key raven-minibuffer-map (kbd "<backtab>") 'raven-previous)
(define-key raven-minibuffer-map (kbd "<tab>") 'raven-next)
(define-key raven-minibuffer-map (kbd "<up>") 'raven-previous)
(define-key raven-minibuffer-map (kbd "<down>") 'raven-next)
(define-key raven-minibuffer-map (kbd "<prior>") 'raven-previous-source)
(define-key raven-minibuffer-map (kbd "<next>") 'raven-next-source)

(when (fboundp 'evil-define-key)
  (evil-define-key 'normal raven-minibuffer-map
		   (kbd "k") 'raven-previous
		   (kbd "j") 'raven-next
		   (kbd "K") 'raven-previous-source
		   (kbd "J") 'raven-next-source))

(defun raven-minibuffer-line (str)
  "Write STR to the minibuffer."
  (goto-char (point-max))
  (insert (concat "\n" str)))

(defun raven-minibuffer-line-face (str face)
  "Write STR to the minibuffer in FACE."
  (let ((before (point)))
    (raven-minibuffer-line str)
    (goto-char before)
    (forward-line)
    (put-text-property (line-beginning-position) (point-max) 'face face)))

(defun raven-minibuffer-clear ()
  "Clear minibuffer."
  (save-excursion
    (goto-char (minibuffer-prompt-end))
    (delete-region (line-end-position) (point-max))))

(defun raven-minibuffer-input ()
  "Get current minibuffer input."
  (buffer-substring-no-properties
   (minibuffer-prompt-end)
   (line-end-position)))

(defun raven-source (name candidates actions)
  "Create a new source named NAME with the given CANDIDATES and ACTIONS.

CANDIDATES is a list of (<display> . <value>).
<display> is either a string, a symbol, or a pair of a string and a face.
If <display> is a symbol, the candidate always matches, and it is displayed
as its value.
This is useful for creating \"dummy\" candidates by ignoring the argument to
the action (which will be the <display> symbol rather than <value>, for dispatch
and instead referencing `raven-input'.
ACTIONS is a list of actions, which can be:
- functions taking candidate values as arguments
- pairs of key strings and such functions"
  (cons name (cons candidates (cons actions (raven-actions-keymap actions)))))

(defun raven-source-name (source)
  "Return the name of SOURCE."
  (car source))

(defun raven-source-candidates (source)
  "Return the candidates of SOURCE."
  (cadr source))

(defun raven-source-actions (source)
  "Return the actions of SOURCE."
  (caddr source))

(defun raven-source-keymap (source)
  "Return the action keymap of SOURCE."
  (cdddr source))

(defun raven-candidate-display (candidate)
  "Return the display of CANDIDATE."
  (cond ((consp candidate) (car candidate))
        (t candidate)))

(defun raven-candidate-display-string (candidate)
  "Return the display name string of CANDIDATE."
  (let ((display (raven-candidate-display candidate)))
    (cond ((consp display) (car display))
          ((stringp display) display)
          ((keywordp display) (raven-candidate-value candidate))
          ((symbolp candidate) (symbol-name candidate))
          (t (error "Invalid candidate display %s for candidate %s" display candidate)))))

(defun raven-candidate-value (candidate)
  "Return the value of CANDIDATE."
  (if (consp candidate)
      (cdr candidate)
    candidate))

(defun raven-action-function (action)
  "Return the function associated with ACTION."
  (if (functionp action)
      action
    (cdr action)))

(defun raven-actions-keymap (actions)
  "Return a keymap for ACTIONS."
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap raven-minibuffer-map)
    (mapc (lambda (action)
            (unless (functionp action)
              (define-key keymap (car action)
                (lambda () (interactive)
                  (raven-do (cdr action))))))
          actions)
    keymap))

(defun raven-pattern-regex (pattern)
  "Convert PATTERN into a regular expression."
  (apply 'string-join
         (mapcar (lambda (w) (concat "\\(" w "\\)")) (split-string pattern))
         '(".*")))

(defun raven-match (candidate regex)
  "Determine whether CANDIDATE is a match for REGEX."
  (let ((display (raven-candidate-display candidate)))
    (cond ((keywordp display) t)
          ((or (symbolp display) (stringp display) (consp display))
           (string-match-p regex (raven-candidate-display-string candidate))))))

(defun raven-matching-candidates (candidates regex)
  "Return the candidates in CANDIDATES matching REGEX."
  (cond ((functionp candidates) (funcall candidates regex))
        (t (seq-filter (lambda (c) (raven-match c regex)) candidates))))

(defun raven-matching-sources (sources regex)
  "Return the sources in SOURCES matching REGEX."
  (let* ((matches (mapcar (lambda (s)
                            (raven-source
                             (raven-source-name s)
                             (raven-matching-candidates
                              (raven-source-candidates s)
                              regex)
                             (raven-source-actions s)))
                          sources)))
    (seq-filter 'raven-source-candidates matches)))

(defun raven-display-source (source)
  "Display SOURCE."
  (when source
    (raven-minibuffer-line-face (raven-source-name source) 'raven-source-name)
    (mapc (lambda (c) (let ((display (raven-candidate-display c)))
                        (if (consp display)
                            (raven-minibuffer-line-face (car display)
                                                        (cdr display))
                          (raven-minibuffer-line
                           (raven-candidate-display-string c)))))
          (raven-source-candidates source))))

(defun raven-nearby (sources)
  "Return candidates in SOURCES that are close to the selected candidate."
  (let* ((adjacent
          (mapcar
           (lambda (s)
             (cond ((and (< (cdr s) (+ raven--source raven-minibuffer-lines))
                         (> (cdr s) raven--source))
                    (cons (car s) 'g))
                   ((= (cdr s) raven--source)
                    (cons (car s) 'e))
                   (t nil)))
           (-zip-pair sources (number-sequence 0 (length sources))))))
    (mapcar
     (lambda (s)
       (when s
         (let* ((candidates (raven-source-candidates (car s))))
           (raven-source
            (raven-source-name (car s))
            (cond ((eq (cdr s) 'g)
                   (-take raven-minibuffer-lines candidates))
                  (t
                   (cl-loop for i from (max (- raven--index
                                               (- (/ raven-minibuffer-lines 2) 1))
                                            0)
                            for j in (-take
                                      raven-minibuffer-lines
                                      (-drop
                                       (- raven--index
                                          (- (/ raven-minibuffer-lines 2) 1))
                                       candidates))
                            collect (if (= i raven--index)
                                        (cons
                                         (cons
                                          (raven-candidate-display-string j)
                                          'raven-highlight)
                                         (raven-candidate-value j))
                                      j))))
            (raven-source-actions (car s))))))
     adjacent)))

(defun raven-update-transient-map ()
  "Update the transient keymap to match the current source."
  (let ((source (car (nthcdr raven--source raven--matching))))
    (set-transient-map (raven-source-keymap source))))

(defun raven-minibuffer-render ()
  "Draw matching candidates to minibuffer."
  (save-excursion
    (let ((pattern (raven-minibuffer-input)))
      ;; Check if the pattern is valid.
      (condition-case err
	  (string-match-p pattern "")
	(invalid-regexp
	 ;; Nope, it's not.  Probably the user typed \ in order to fix a file
	 ;; extension like entering raven\.el.  Print the error and simply use
	 ;; the old pattern until it becomes valid again.
	 (minibuffer-error-function err "" 'raven-minibuffer-render)
	 (setq pattern raven--last)))
      (unless (string= pattern raven--last)
        (setq raven--last pattern
              raven--index 0
              raven--source 0
              raven--matching (raven-matching-sources
                               raven--sources
                               (raven-pattern-regex pattern)))))
    (mapc 'raven-display-source (raven-nearby raven--matching))
    (goto-char (minibuffer-prompt-end))
    (put-text-property (line-end-position) (point-max) 'readonly t))
  (raven-update-transient-map))

(defun raven-minibuffer-setup (initial)
  "Ready minibuffer for completion with INITIAL as initial input."
  (add-hook 'pre-command-hook 'raven-minibuffer-clear nil t)
  (add-hook 'post-command-hook 'raven-minibuffer-render nil t)
  (setq-local max-mini-window-height raven-minibuffer-lines)
  (when initial
    (save-excursion
      (minibuffer-prompt-end)
      (insert initial)))
  (end-of-line)
  (raven-update-transient-map)
  (when (fboundp 'evil-insert-state)
    (evil-insert-state)))

(defun raven-previous-source ()
  "Move to the previous source."
  (interactive)
  (setq raven--index 0)
  (setq raven--source (if (= raven--source 0)
                          (- (length raven--matching) 1)
                        (- raven--source 1))))

(defun raven-next-source ()
  "Move to the next source."
  (interactive)
  (setq raven--index 0)
  (setq raven--source (% (+ raven--source 1) (length raven--matching))))

(defun raven-previous ()
  "Move to the previous candidate."
  (interactive)
  (let* ((new-source-index (if (= raven--source 0)
                               (- (length raven--matching) 1)
                             (- raven--source 1)))
         (source (car (nthcdr new-source-index raven--matching))))
    (setq raven--index (- raven--index 1))
    (when (< raven--index 0)
      (setq raven--index (- (length (raven-source-candidates source)) 1)
            raven--source new-source-index))))

(defun raven-next ()
  "Move to the next candidate."
  (interactive)
  (let* ((source (car (nthcdr raven--source raven--matching))))
    (setq raven--index (+ raven--index 1))
    (when (= raven--index (length (raven-source-candidates source)))
      (setq raven--index 0
            raven--source (% (+ raven--source 1) (length raven--matching))))))

(defun raven-do (&optional action-function)
  "Act upon selected candidate.
If ACTION-FUNCTION is given use it, otherwise use the first action for the candidate."
  (interactive)
  (let* ((source (car (nthcdr raven--source raven--matching)))
         (candidate (car (nthcdr raven--index (raven-source-candidates source)))))
    (setq raven--action (if action-function
                            action-function
                          (let ((actions (raven-source-actions source)))
                            (if actions
                                (raven-action-function (car actions))
                              (lambda (x) x))))
          raven--result (cond ((symbolp (raven-candidate-display candidate))
                               (raven-candidate-display candidate))
                              (t
                               (raven-candidate-value candidate)))))
  (exit-minibuffer))

;;;###autoload
(cl-defun raven (sources &key prompt initial)
  "Select a candidate and run an action using SOURCES.
Display PROMPT as the prompt, or \"pattern: \" if not given.
Use INITIAL as the initial input."
  (setq raven--sources sources
        raven--last nil
        raven--matching (raven-matching-sources sources "")
        raven--source 0
        raven--index 0
        raven--action nil
        raven--result nil)
  (let ((inhibit-message t))
    (minibuffer-with-setup-hook
        (apply-partially 'raven-minibuffer-setup initial)
      (read-from-minibuffer (or prompt "pattern: ") nil raven-minibuffer-map)))
  (funcall raven--action raven--result))

;;;###autoload
(defun raven-completing-read (prompt collection &optional predicate require-match
                                     initial-input hist def inherit-input-method)
  "Replacement for `completing-read'.
PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD have the same meaning as in `completing-read'."
  (ignore predicate require-match)
  (or
   (cond ((functionp collection)
          (read-string prompt initial-input hist def inherit-input-method))
         ((hash-table-p collection)
          (raven (list (raven-source "Completions"
                                     (hash-table-keys collection)
                                     '()))
                 :prompt prompt
                 :initial initial-input))
         ((obarrayp collection)
          (let ((candidates (list)))
            (mapatoms (lambda (x) (push (symbol-name x) candidates)) collection)
            (raven (list (raven-source "Completions" candidates '()))
                   :prompt prompt
                   :initial initial-input)))
         (t (raven (list (raven-source "Completions"
                                       (mapcar (lambda (x)
                                                 (let ((y (if (consp x) (car x) x)))
                                                   (if (symbolp y) (symbol-name y) y)))
                                               collection)
                                       '()))
                   :prompt prompt
                   :initial initial-input)))
   (raven-input)))

(defun raven-input () "Return last minibuffer input." raven--last)

(defvar raven-extended-command-actions
  (list (lambda (c)
          (add-to-list 'extended-command-history (symbol-name c))
          (command-execute c))
        (cons (kbd "C-h") 'describe-function)))

;;;###autoload
(defun raven-extended-commands-source ()
  "Source for extended commands (`M-x')."
  (raven-source "Commands"
                (lambda (r) (apropos-internal r 'commandp))
                raven-extended-command-actions))

;;;###autoload
(defun raven-extended-command-history-source ()
  "Source for extended command history."
  (raven-source "Command History"
                (mapcar (lambda (s) (cons s (intern-soft s)))
                        extended-command-history)
                raven-extended-command-actions))

;;;###autoload
(defun raven-apropos-command-source ()
  "Source for command lookup."
  (raven-source "Commands"
                (lambda (r) (apropos-internal r 'commandp))
                '(describe-function)))

;;;###autoload
(defun raven-apropos-function-source ()
  "Source for function lookup."
  (raven-source "Functions"
                (lambda (r) (apropos-internal r 'fboundp))
                '(describe-function)))

;;;###autoload
(defun raven-apropos-variable-source ()
  "Source for variable lookup."
  (raven-source "Variables"
                (lambda (r) (apropos-internal r (lambda (x) (and (boundp x) (not (keywordp x))))))
                '(describe-variable)))

(defvar raven-buffer-actions
  (list 'switch-to-buffer
        (cons (kbd "M-D") 'kill-buffer)))

;;;###autoload
(defun raven-buffers-source (&optional sort-pred)
  "Source for open buffers.
An optional SORT-FN may be provided to sort the buffers (see
`sort')."
  (raven-source "Buffers"
                (mapcar 'buffer-name
			(if sort-pred
			    (sort (buffer-list) sort-pred)
			  (buffer-list)))
                raven-buffer-actions))

;;;###autoload
(defun raven-create-buffer-source ()
  "Dummy source to create a buffer."
  (raven-source "Other"
                '((:new . "Create buffer"))
                (list (lambda (_) (switch-to-buffer (raven-input))))))

(defvar raven-file-actions
  (list 'find-file
        (cons (kbd "M-D") (lambda (f)
                            (when (y-or-n-p (concat "Delete file " f "? "))
                              (delete-file f))))))

;;;###autoload
(defun raven-files-source ()
  "Source for files in current directory."
  (raven-source "Files"
                (directory-files default-directory)
                raven-file-actions))

;;;###autoload
(defun raven-create-file-source ()
  "Dummy source to create a file."
  (raven-source "Other"
                '((:new . "Create file"))
                (list (lambda (_) (find-file (raven-input))))))

;;;###autoload
(defun raven-recentf-source ()
  "Source for recentf."
  (raven-source "Recent Files"
                recentf-list
                raven-file-actions))

;; project.el is in Emacs since version 25.1, I think.
(when (and (fboundp 'project-current)
	   (fboundp 'project-files))
  (defun raven-project-source ()
    "Source for project files using Emacs's `project.el'."
    (raven-source "Project Files"
		  (when-let (p (project-current))
		    (mapcar #'file-relative-name
			    (project-files p)))
		  raven-file-actions)))

;;;###autoload
(defun raven-M-x ()
  "Preconfigured `raven' interface to replace `execute-external-command'."
  (interactive)
  (raven (list (raven-extended-command-history-source)
               (raven-extended-commands-source))))

;;;###autoload
(defun raven-apropos (&optional initial)
  "Preconfigured `raven' interface to replace `apropos'.
INITIAL is the initial text to match."
  (interactive)
  (raven (list (raven-apropos-command-source)
               (raven-apropos-function-source)
               (raven-apropos-variable-source))
         :initial (concat "^" (if initial initial (thing-at-point 'symbol t)))))

;;;###autoload
(defun raven-for-buffers ()
  "Preconfigured `raven' interface for open buffers and recentf."
  (interactive)
  (raven (list (raven-buffers-source)
               (raven-recentf-source)
               (raven-create-buffer-source))))

;;;###autoload
(defun raven-for-files ()
  "Preconfigured `raven' interface for files in the current directory."
  (interactive)
  (raven (list (raven-files-source)
               (raven-create-file-source))))

;;;###autoload
(defun raven-read-file-name (prompt &optional dir default-filename mustmatch initial predicate)
  "Replacement for `read-file-name'.
PROMPT, DIR, DEFAULT-FILENAME, MUSTMATCH, INITIAL and PREDICATE have the same
meaning as in `read-file-name'."
  (ignore default-filename mustmatch predicate)
  (let ((d (if dir dir default-directory)))
    (concat d (raven (list (raven-source "Files" (directory-files d) '())
                           (raven-source "Other"
                                         '((:new . "New file"))
                                         (list (lambda (_) (raven-input)))))
                     :prompt prompt
                     :initial initial))))

(provide 'raven)
;;; raven.el ends here
