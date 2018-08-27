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
(require 'evil)

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
(define-key raven-minibuffer-map (kbd "C-g") 'minibuffer-keyboard-quit)
(define-key raven-minibuffer-map (kbd "C-c") 'minibuffer-keyboard-quit)
(define-key raven-minibuffer-map (kbd "<return>") 'raven-do)
(define-key raven-minibuffer-map (kbd "<backtab>") 'raven-previous)
(define-key raven-minibuffer-map (kbd "<tab>") 'raven-next)
(define-key raven-minibuffer-map (kbd "<up>") 'raven-previous)
(define-key raven-minibuffer-map (kbd "<down>") 'raven-next)
(define-key raven-minibuffer-map (kbd "<prior>") 'raven-previous-source)
(define-key raven-minibuffer-map (kbd "<next>") 'raven-next-source)

(evil-define-key 'normal raven-minibuffer-map
  (kbd "k") 'raven-previous
  (kbd "j") 'raven-next
  (kbd "K") 'raven-previous-source
  (kbd "J") 'raven-next-source)

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
  "Return the display name of CANDIDATE."
  (if (consp candidate)
      (car candidate)
    candidate))

(defun raven-candidate-display-string (candidate)
  "Return the display name string of CANDIDATE."
  (cond ((consp (raven-candidate-display candidate))
         (car (raven-candidate-display candidate)))
        ((stringp (raven-candidate-display candidate))
         (raven-candidate-display candidate))
        (t (raven-candidate-value candidate))))

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

(defun raven-match (candidate pattern)
  "Determine whether CANDIDATE is a match for PATTERN.
Return match data if so; nil otherwise."
  (let ((display (raven-candidate-display candidate)))
    (cond ((or (stringp display) (consp display))
           (when (string-match
                  pattern
                  (raven-candidate-display-string candidate))
             (match-data)))
          (t t))))

(defun raven-matching-candidates (candidates pattern)
  "Return the candidates in CANDIDATES matching PATTERN."
  (seq-filter (lambda (c) (raven-match c pattern)) candidates))

(defun raven-matching-sources (sources pattern)
  "Return the sources in SOURCES matching PATTERN."
  (let* ((matches (mapcar (lambda (s)
                            (raven-source
                             (raven-source-name s)
                             (raven-matching-candidates
                              (raven-source-candidates s)
                              pattern)
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

(defun raven-minibuffer-setup ()
  "Ready minibuffer for completion."
  (add-hook 'pre-command-hook 'raven-minibuffer-clear nil t)
  (add-hook 'post-command-hook 'raven-minibuffer-render nil t)
  (setq-local max-mini-window-height raven-minibuffer-lines)
  (raven-update-transient-map)
  (evil-insert-state))

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

(defun raven (sources &optional prompt)
  "Select a candidate and run an action using SOURCES.
Display PROMPT as the prompt, or \"pattern: \" if not given."
  (setq raven--sources sources
        raven--last nil
        raven--matching (raven-matching-sources sources "")
        raven--source 0
        raven--index 0
        raven--action nil
        raven--result nil)
  (let ((inhibit-message t))
    (minibuffer-with-setup-hook
        'raven-minibuffer-setup
      (read-from-minibuffer (or prompt "pattern: ") nil raven-minibuffer-map)))
  (funcall raven--action raven--result))

(defun raven-completing-read (prompt collection &optional predicate require-match
                                     initial-input hist def inherit-input-method)
  "Replacement for `completing-read'.
PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD have the same meaning as in `completing-read'."
  (ignore predicate require-match initial-input hist def inherit-input-method)
  (cond ((functionp collection)
         (warn "Unsupported completion type. Try writing a custom source.")
         nil)
        ((hash-table-p collection)
         (raven (list (raven-source "Completions"
                                    (-zip-pair (hash-table-keys collection)
                                               (hash-table-values collection))
                                    '()))))
        ((obarrayp collection)
         (let ((candidates (list)))
           (mapatoms (lambda (x) (push (symbol-name x) candidates)) collection)
           (raven (list (raven-source "Completions" candidates '())))))
        (t (raven (list (raven-source "Completions" collection '()))
                  prompt))))

(defun raven-input () "Return last minibuffer input." raven--last)

(defvar raven-extended-command-actions
  (list (lambda (c)
          (add-to-list 'extended-command-history (symbol-name c))
          (command-execute c))
        (cons (kbd "C-h") 'describe-function)))

(defun raven-extended-commands-source ()
  "Source for extended commands (`M-x')."
  (raven-source "Commands"
                (mapcar (lambda (s) (cons (symbol-name s) s))
                        (apropos-internal "" 'commandp))
                raven-extended-command-actions))

(defun raven-extended-command-history-source ()
  "Source for extended command history."
  (raven-source "Command History"
                (mapcar (lambda (s) (cons s (intern-soft s)))
                        extended-command-history)
                raven-extended-command-actions))

(defvar raven-buffer-actions
  (list 'switch-to-buffer
        (cons (kbd "M-D") 'kill-buffer)))

(defun raven-buffers-source ()
  "Source for open buffers."
  (raven-source "Buffers"
                (mapcar 'buffer-name (buffer-list))
                raven-buffer-actions))

(defun raven-create-buffer-source ()
  "Dummy source to create a buffer."
  (raven-source "Other"
                '((t . "Create buffer"))
                (list (lambda (_) (switch-to-buffer (raven-input))))))

(defvar raven-file-actions
  (list 'find-file
        (cons (kbd "M-D") (lambda (f)
                            (when (y-or-n-p (concat "Delete file " f "? "))
                              (delete-file f))))))

(defun raven-files-source ()
  "Source for files in current directory."
  (raven-source "Files"
                (directory-files default-directory)
                raven-file-actions))

(defun raven-create-file-source ()
  "Dummy source to create a file."
  (raven-source "Other"
                '((t . "Create file"))
                (list (lambda (_) (find-file (raven-input))))))

(defun raven-recentf-source ()
  "Source for recentf."
  (raven-source "Recent Files"
                recentf-list
                raven-file-actions))

(defun raven-M-x ()
  "Preconfigured `raven' interface to replace `execute-external-command'."
  (interactive)
  (raven (list (raven-extended-command-history-source)
               (raven-extended-commands-source))
         "Command: "))

(defun raven-for-buffers ()
  "Preconfigured `raven' interface for open buffers and recentf."
  (interactive)
  (raven (list (raven-buffers-source)
               (raven-recentf-source)
               (raven-create-buffer-source))))

(defun raven-for-files ()
  "Preconfigured `raven' interface for files in the current directory."
  (interactive)
  (raven (list (raven-files-source)
               (raven-create-file-source))))

(provide 'raven)
;;; raven.el ends here
