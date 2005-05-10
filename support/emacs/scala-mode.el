;;; scala.el - Major mode for editing Scala code.
;;; $Id$

;;; TODO
;;; - make automatic indentation work in all cases
;;; - support more Emacs variants (especially XEmacs)

(require 'easymenu)
(require 'cl)
(require 'regexp-opt)

(defconst scala-mode-version "0.3 ($Revision$)")
(defconst scala-bug-e-mail "Michel.Schinz@epfl.ch")
(defconst scala-web-url "http://scala.epfl.ch/")

;; XEmacs compatibility
;; (note that XEmacs is not supported currently, the function below
;; only works around a single incompatibility).

;;(defun scala-regexp-opt-charset (cset)
;;  (regexp-opt-charset (if (integerp ?a)
;;                          cset
;;                        (mapcar #'char-to-string cset))))
(defun scala-regexp-opt-charset (chars)
  ;;
  ;; Return a regexp to match a character in CHARS.
  ;;
  ;; The basic idea is to find character ranges.  Also we take care in the
  ;; position of character set meta characters in the character set regexp.
  ;;
  (let* ((charmap (make-char-table 'case-table))
     (start -1) (end -2)
     (charset "")
     (bracket "") (dash "") (caret ""))
    ;;
    ;; Make a character map but extract character set meta characters.
    (dolist (char chars)
      (case char
    (?\]
     (setq bracket "]"))
    (?^
     (setq caret "^"))
    (?-
     (setq dash "-"))
    (otherwise
     (aset charmap char t))))
    ;;
    ;; Make a character set from the map using ranges where applicable.
    (map-char-table
     (lambda (c v)
       (when v
     (if (= (1- c) end) (setq end c)
       (if (> end (+ start 2))
           (setq charset (format "%s%c-%c" charset start end))
         (while (>= end start)
           (setq charset (format "%s%c" charset start))
           (incf start)))
       (setq start c end c))))
     charmap)
    (when (>= end start)
      (if (> end (+ start 2))
      (setq charset (format "%s%c-%c" charset start end))
    (while (>= end start)
      (setq charset (format "%s%c" charset start))
      (incf start))))
    ;;
    ;; Make sure a caret is not first and a dash is first or last.
    (if (and (string-equal charset "") (string-equal bracket ""))
    (concat "[" dash caret "]")
      (concat "[" bracket charset caret dash "]"))))

;; Customization

(defgroup scala
  nil
  "Mode to edit Scala code."
  :group 'languages)

(defcustom scala-indent-step 2
  "Indentation step."
  :type 'integer
  :group 'scala)

(defconst scala-number-re
  "[[:digit:]]+\\(\\.[[:digit:]]+\\)?\\([eE][+-]?[[:digit:]]+\\)?[fl]?"
  "Regular expression matching a Scala number (integer or float).")

(defconst scala-string-re
  "\"\\([^\"\\\\]\\|\\\\\.\\)*\""
  "Regular expression matching a Scala string literal.")

(defconst scala-char-re
  "'\\([^\\\\]\\|\\(\\\\[^']\\)\\)'"
  "Regular expression matching a Scala character literal.")

(defconst scala-literal-re
  (concat "\\(" "\\(" scala-number-re "\\)"
          "\\|" "\\(" scala-string-re "\\)"
          "\\|" "\\(" scala-char-re "\\)" "\\)")
  "Regular expression matching any Scala literal.")

(defconst scala-most-special-chars (mapcar 'identity "<>+-*/|@#%&!?$^`~")
  "List of almost all Scala special characters.
Not included in this list are the special characters which are
reserved keywords when used alone.")

(defconst scala-all-special-chars (append (mapcar 'identity ":;,=")
                                          scala-most-special-chars)
  "List of all Scala special characters.")

(defconst scala-most-special-char-re
  (scala-regexp-opt-charset scala-most-special-chars)
  "Regular expression matching a single Scala special character")

(defconst scala-all-special-char-re
  (scala-regexp-opt-charset scala-all-special-chars)
  "Regular expression matching a single Scala special character")

(defconst scala-keywords-re
  (regexp-opt '("abstract" "case" "class" "catch" "def" "do" "else"
                "extends" "final" "finally" "for" "if" "import" "new" "object"
                "override" "package" "private" "protected" "return"
                "sealed" "super" "this" "throw" "trait" "try" "type" "val" "var"
                "with" "while" "yield")
	      'words))

(defconst scala-constants-re
  (regexp-opt '("true" "false" "null") 'words))

(defconst scala-special-ident-re
  (concat "\\(" scala-all-special-char-re "\\{2,\\}"
          "\\|" scala-most-special-char-re "+"
          "\\)"))

(defconst scala-ident-re
  (let* ((varid-re "[[:alnum:]]+")
         (id-re (concat "\\(" varid-re "\\|" scala-special-ident-re "\\)")))
    (concat id-re
            "\\(" "_+" "\\(" id-re "\\)?" "\\)*"))
  "Regular expression matching a Scala identifier.")

(defconst scala-var-ident-re
  (concat "[[:lower:]][[:alnum:]]*" "\\(_" scala-ident-re "\\)*")
  "Relgular expression matching a Scala 'variable' identifier.")

(defconst scala-qual-ident-re
  (concat scala-ident-re "\\(" "\\." scala-ident-re "\\)*"))

(defconst scala-capitalized-ident-re
  (concat "\\(\\)\\([[:upper:]]" scala-ident-re "\\)"))

(defconst scala-expr-start-re
  (concat
   (regexp-opt '("if" "else" "for" "do" "yield") 'words) "\\|"
   (regexp-opt '("=" "=>") t)))

(defconst scala-expr-starter
  (mapcar (lambda (pair) (cons (car pair) (concat "\\<" (cdr pair) "\\>")))
          '(("else" . "if")
            ("yield" . "for")
            ("do" . "for")
            ("extends" . "class")
            ("with" . "class")
            ("=>" . "case"))))

(defconst scala-expr-middle-re
  (regexp-opt (mapcar #'car scala-expr-starter) 'words))

(defconst scala-compound-expr-re
  "\\<else\\s +if\\>")

(defun scala-special-char-p (char)
  (and char
       (string-match scala-all-special-char-re (string char))))

(defun scala-looking-at-special-identifier (regexp)
  (and (not (scala-special-char-p (char-before)))
       (looking-at regexp)
       (not (scala-special-char-p (char-after (match-end 0))))))

(defconst scala-comment-begin-or-end-re
  (concat "\\(" "^/\\*.*" "\\|" "^//.*" "\\|" ".*\\*/$" "\\)"))

(defun scala-search-special-identifier-forward (limit)
  (ignore-errors
    (while (and (search-forward-regexp scala-special-ident-re limit)
                (save-match-data
                  (string-match scala-comment-begin-or-end-re
                                (match-string-no-properties 0)))))
    t))

;; Movement

(defun scala-when-looking-at* (regexp &optional thunk)
  (let ((saved-match-data (match-data)))
    (if (looking-at regexp)
        (progn (goto-char (match-end 0))
               (set-match-data saved-match-data)
               (or (not thunk) (funcall thunk)))
      (set-match-data saved-match-data)
      nil)))

(defmacro scala-when-looking-at (regexp &rest body)
  (if body
      `(scala-when-looking-at* ,regexp (lambda () ,@body))
    `(scala-when-looking-at* ,regexp)))

(defun scala-forward-spaces (&optional limit)
  (if limit
      (save-restriction
        (narrow-to-region (point) limit)
        (forward-comment 100000))
    (forward-comment 100000)))

(defun scala-backward-spaces ()
  (forward-comment -100000))

(defun scala-looking-at-backward (re)
  (save-excursion
    (when (= 0 (skip-syntax-backward "w_")) (backward-char))
    (looking-at re)))

(defmacro scala-point-after (&rest body)
  `(save-excursion
     ,@body
     (point)))

(defmacro scala-move-if (&rest body)
  (let ((pt-sym (make-symbol "point"))
	(res-sym (make-symbol "result")))
    `(let ((,pt-sym (point))
	   (,res-sym ,(cons 'progn body)))
       (unless ,res-sym (goto-char ,pt-sym))
       ,res-sym)))

(defun scala-forward-ident ()
  ;; Move forward over an identifier.
  (scala-forward-spaces)
  (if (looking-at scala-ident-re)
      (goto-char (match-end 0))
    (forward-char))
  t)

(defun scala-backward-ident ()
  ;; Move backward over an identifier.
  (scala-backward-spaces)
  (if (scala-looking-at-backward scala-ident-re)
      (goto-char (match-beginning 0))
    (backward-char))
  t)

(defun scala-forward-qual-ident ()
  ;; Move forward over a qualifier identifier.
  (scala-forward-spaces)
  (if (looking-at scala-qual-ident-re)
      (goto-char (match-end 0))
    (forward-char))
  t)

(defun scala-backward-qual-ident ()
  ;; Move backward over a qualifier identifier.
  (scala-backward-spaces)
  (if (scala-looking-at-backward scala-qual-ident-re)
      (goto-char (match-beginning 0))
    (backward-char))
  t)

(defun scala-forward-simple-type ()
  ;; Move forward over a simple type (as defined by the grammar).
  ;; Works only when point is at the beginning of a simple type
  ;; (modulo initial spaces/comments).
  (cond ((eobp) nil)
        ((= (char-after) ?\()
         ;; Parenthesized type
         (forward-sexp)
         t)
        (t
         ;; Type designator
         (scala-forward-qual-ident)
         (scala-forward-spaces)
         (cond ((eobp) nil)
               ((= (char-after) ?\[)
                ;; Type arguments
                (forward-sexp))
               ((= (char-after) ?\#)
                ;; Type selection
                (forward-char)
                (scala-forward-ident)))
         t)))

(defun scala-forward-type1 ()
  ;; Move forward over a type1 (as defined by the grammar).
  ;; Works only when point is at the beginning of a type (modulo
  ;; initial spaces/comments).
  (scala-forward-spaces)
  (scala-when-looking-at "\\<class\\>"
                         (forward-word 1) (scala-forward-spaces))
  (scala-forward-simple-type)
  (while (scala-when-looking-at "\\s *\\<with\\>\\s *")
    (if (and (not (eobp)) (= (char-after) ?\{))
        (forward-sexp)                       ;skip refinement
      (scala-forward-simple-type)))
  t)

(defun scala-forward-type ()
  ;; Move forward over a type.
  (cond ((eobp) nil)
        ((= (char-after) ?\()
         ;; Function type (several arguments)
         (forward-sexp)
         (scala-when-looking-at "\\s *=>\\s *" (scala-forward-type))
         t)
        (t
         ;; Type1 or function type with one argument
         (scala-forward-type1)
         (scala-when-looking-at "\\s *=>\\s *" (scala-forward-type))
         t)))

(defun scala-forward-type-param ()
  ;; Move over a type parameter
  ;; variance
  (scala-when-looking-at "\\s *[-+]\\s *")
  (scala-forward-ident)
  ;; bounds
  (while (scala-when-looking-at "\\s *[<>][:%]\\s *")
    (scala-forward-type))
  t)

(defun scala-forward-literal ()
  ;; Move forward over an integer, float, character or string literal.
  (scala-forward-spaces)
  (scala-when-looking-at scala-literal-re)
  t)

;;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scala-parse-partial-sexp ()
  (parse-partial-sexp (point-min) (point)))

(defun scala-in-comment-p ()
  "Return t iff the point is inside a comment."
  ;; The two branches of the "if" below do not have the same behaviour
  ;; when the point is on the comment beginning/ending character(s).
  (if font-lock-mode
      (eq (get-text-property (point) 'face) 'font-lock-comment-face)
    (save-excursion (comment-beginning))))

(defun scala-in-string-p ()
  "Return t iff the point is inside a string."
  (if font-lock-mode
      (eq (get-text-property (point) 'face) 'font-lock-string-face)
    (let ((limit (point)))
      (beginning-of-line)
      (loop while (search-forward-regexp "\\(^\\|[^\\\\]\\)\"" limit 'move)
            count (not (scala-in-comment-p)) into quotes
            finally return (oddp quotes)))))

(defun scala-indentation ()
  "Return the suggested indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (or (and (scala-in-comment-p)
             (not (= (char-after) ?\/))
             (scala-comment-indentation))
        (scala-indentation-from-following)
        (scala-indentation-from-preceding)
        (scala-indentation-from-block)
        0)))

(defun scala-comment-indentation ()
  ;; Return suggested indentation inside of a comment.
  (forward-line -1)
  (beginning-of-line)
  (skip-syntax-forward " ")
  (if (looking-at "/\\*")
      (+ 1 (current-column))
    (current-column)))

(defun scala-block-indentation ()
  (let ((block-start-eol (scala-point-after (end-of-line)))
        (block-after-spc (scala-point-after (scala-forward-spaces))))
    (if (> block-after-spc block-start-eol)
        (+ (current-indentation) scala-indent-step)
      (current-column))))

(defun scala-indentation-from-following ()
  ;; Return suggested indentation based on the following part of the
  ;; current expression. Return nil if indentation cannot be guessed.
  (save-excursion
    (scala-forward-spaces (scala-point-after (end-of-line)))
    (cond
     ((eobp) nil)
     ((= (char-syntax (char-after)) ?\))
      (let ((parse-sexp-ignore-comments t))
        (goto-char (1+ (scan-sexps (1+ (point)) -1))))
      (- (scala-block-indentation) scala-indent-step))
     ((looking-at scala-expr-middle-re)
      ;; [...] this is a somewhat of a hack.
      (let ((matching-kw (cdr (assoc (match-string-no-properties 0)
                                     scala-expr-starter))))
        (while (and (search-backward-regexp matching-kw nil t)
                    (or (scala-in-comment-p) (scala-in-string-p)))))
      (scala-move-if (backward-word 1)
                     (looking-at scala-compound-expr-re))
      (current-column)))))

(defun scala-indentation-from-preceding ()
  ;; Return suggested indentation based on the preceding part of the
  ;; current expression. Return nil if indentation cannot be guessed.
  (save-excursion
    (scala-backward-spaces)
    (when (and (not (bobp))
               (or (eq (char-syntax (char-before)) ?\()
                   (progn
                     (when (eq (char-before) ?\))
                       (backward-sexp)
                       (scala-backward-spaces))
                     (scala-looking-at-backward scala-expr-start-re))))
      (+ (current-indentation) scala-indent-step))))

(defun scala-indentation-from-block ()
  ;; Return suggested indentation based on the current block.
  (save-excursion
    (let* ((state (scala-parse-partial-sexp))
           (block-start (nth 1 state)))
      (if (not block-start)
          0
        (goto-char (1+ block-start))
        (scala-block-indentation)))))

(defun scala-indent-line-to (column)
  "Indent current line to COLUMN and perhaps move point.
The point is moved iff it is currently in the indentation, in which
case it is brought to the end of that indentation. Otherwise it does
not move."
  (if (<= (current-column) (current-indentation))
      (indent-line-to column)
    (save-excursion (indent-line-to column))))

(defun scala-indent-line ()
  "Indent current line as smartly as possible.
When called repeatedly, indent each time one stop further on the right."
  (interactive)
  (if (or (eq last-command this-command)
          (eq last-command 'scala-undent-line))
      (scala-indent-line-to (+ (current-indentation) scala-indent-step))
    (let ((indentation (scala-indentation)))
      (scala-indent-line-to indentation))))

(defun scala-undent-line ()
  "Indent line to previous tab stop."
  (interactive)
  (scala-indent-line-to (max 0 (- (current-indentation) scala-indent-step))))

(defun scala-electric-brace ()
  "Insert a brace, and if alone on a non-comment line, reindent."
  (interactive)
  (let ((on-empty-line-p (save-excursion
                           (beginning-of-line)
                           (looking-at "^\\s *$"))))
    (call-interactively 'self-insert-command)
    (when on-empty-line-p (scala-indent-line))))

;;; Syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scala-mark-borders (funs)
  (loop for (fun . flag) in funs
        if flag collect (point-marker)
        while (funcall fun)
        if flag collect (point-marker)))

(defun scala-make-match (funs)
  (let ((start-mark (point-marker))
        (markers (scala-mark-borders funs))
        (end-mark (point-marker)))
    (cons start-mark (cons end-mark markers))))

(defconst scala-binding-end-re
  (regexp-opt '(":" "=" "=>" ";" "<-")))

(defun scala-match-and-skip-binding (limit)
  (skip-chars-forward " ()")
  (and (not (or (looking-at "\\<\\(extends\\|with\\)\\>\\|{")
                (scala-looking-at-special-identifier scala-binding-end-re)))
       (ignore-errors
         (save-restriction
           (narrow-to-region (point-min) limit)
           (let ((matches (scala-make-match
                           '((scala-forward-ident . t)
                             ((lambda ()
                                (scala-forward-spaces)
                                (when (scala-looking-at-special-identifier ":")
                                  (forward-char)
                                  (scala-forward-spaces)
                                  t)) . nil)
                             ((lambda ()
                                (scala-forward-type)
                                (scala-when-looking-at "\\s *\\*")
                                t) . t)))))
             (scala-when-looking-at "\\s *,")
             (set-match-data matches)))
         t)))

(defun scala-match-and-skip-ident (limit)
  (scala-forward-spaces)
  (when (and (not (looking-at scala-keywords-re))
             (looking-at scala-qual-ident-re))
    (goto-char (match-end 0))
    t))

(defun scala-match-and-skip-type-param (limit)
  (scala-when-looking-at "\\s *[[,]\\s *"
    (let ((matches (scala-make-match '((scala-forward-type-param . t)))))
      (scala-when-looking-at "\\s *\\]")
      (set-match-data matches)
      t)))

(defun scala-match-and-skip-result-type (limit)
  (scala-when-looking-at "\\s *:\\s *"
    (set-match-data (list (point-marker)
                          (progn (scala-forward-type) (point-marker))))
    t))

(defconst scala-pattern-end-re
  (regexp-opt '("if" "case" "class") 'words))

(defconst scala-pattern-end-special-re
  (regexp-opt '( "=>" "=" "<-") t))

(defun scala-match-and-skip-pattern (limit)
  (while (progn
           (skip-chars-forward "()[], ")
           (and (not (or (looking-at scala-pattern-end-re)
                         (scala-looking-at-special-identifier
                          scala-pattern-end-special-re)))
                (looking-at scala-literal-re)))
    (goto-char (match-end 0)))
  (and (not (or (looking-at scala-pattern-end-re)
                (scala-looking-at-special-identifier scala-pattern-end-special-re)))
       (let ((case-fold-search nil))
         (cond ((looking-at scala-capitalized-ident-re)
                (goto-char (match-end 0)))
               ((scala-match-and-skip-binding limit) t)))))

(defvar scala-font-lock-keywords
  `(;; keywords
    (,scala-keywords-re
     0 font-lock-keyword-face nil)

    ;; constants
    (,scala-constants-re
     0 ,(if (boundp 'font-lock-constant-face)
	    'font-lock-constant-face
	  'font-lock-keyword-face)
     nil)

    ;; modules
    (,(concat "\\<\\(module\\|object\\)\\>\\s *\\(" scala-ident-re "\\)")
     (2 font-lock-variable-name-face nil))

    ;; type definitions
    (,(concat "\\<type\\>\\s *\\(" scala-ident-re "\\)")
     (1 font-lock-type-face nil))

    ;; variables
    ("\\<var\\>"
     (scala-match-and-skip-binding (goto-char (match-end 0))
                                   nil
                                   (1 font-lock-variable-name-face nil)
                                   (2 font-lock-type-face nil t)))

    ;; functions
    (,(concat "\\(^\\|[^(,]\\)\\s *\\<def\\>" "\\s *" "\\(" scala-ident-re "\\)\\s *")
     (2 font-lock-function-name-face nil)
     (scala-match-and-skip-type-param (goto-char (match-end 0)) nil
                                      (1 font-lock-type-face nil t))
     (scala-match-and-skip-binding nil nil
                                   (1 font-lock-variable-name-face nil)
                                   (2 font-lock-type-face nil t))
     (scala-match-and-skip-result-type nil nil
                                       (0 font-lock-type-face nil)))

    ;; class definitions
    ("\\<\\(class\\|trait\\)\\>"
     (scala-match-and-skip-ident (goto-char (match-end 0)) nil
                                 (1 font-lock-type-face nil))
     (scala-match-and-skip-type-param nil nil
                                      (1 font-lock-type-face nil t))
     (scala-match-and-skip-binding nil nil
                                   (1 font-lock-variable-name-face nil)
                                   (2 font-lock-type-face nil t)))

    ;; "extends" and "with" clauses
    ("\\<\\(extends\\|with\\)\\>\\s *[^{]"
     (scala-match-and-skip-ident (goto-char (1- (match-end 0))) nil
                                 (0 font-lock-type-face nil))
     (scala-match-and-skip-type-param nil nil
                                      (1 font-lock-type-face nil t)))

    ;; patterns
    ("\\<\\(case\\|val\\)\\>\\s *"
     (scala-match-and-skip-pattern (goto-char (match-end 0)) nil
                                   (1 font-lock-variable-name-face nil)
                                   (2 font-lock-type-face nil t)))))


(defvar scala-font-lock-syntactic-keywords
  `((,scala-char-re (0 "\"" t nil))
    (scala-search-special-identifier-forward (0 "w" nil nil))))

;; Bug reporting

(defun scala-report-bug ()
  "Report a bug to the author of the Scala mode via e-mail.
The package used to edit and send the e-mail is the one selected
through `mail-user-agent'."
  (interactive)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report
     scala-bug-e-mail
     (concat "Scala mode v" scala-mode-version)
     '(scala-indent-step))))

;; Customization

(defun scala-customize ()
  "Customize Scala mode using the Customize package."
  (interactive)
  (customize-group 'scala))

(defun scala-interpreter-running-p ()
  "True iff a Scala interpreter is currently running in a buffer."
  ;; The following makes sure that we do not autoload
  ;; inferior-scala-mode just to check if the interpreter is running.
  (and (fboundp 'inferior-scala-mode)
       (let ((ism-def (symbol-function 'inferior-scala-mode)))
         (not (and (consp ism-def) (eq (car ism-def) 'autoload))))
       (scala-interpreter-running-p-1)))

(defun scala-browse-web-site ()
  "Browse the Scala home-page"
  (interactive)
  (require 'browse-url)
  (browse-url scala-web-url))

;;; Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-derived-mode scala-mode fundamental-mode "Scala"
  "Major mode for editing Scala code.

When started, run `scala-mode-hook'.

\\{scala-mode-map}"
  ;; Font lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        `(scala-font-lock-keywords
          nil
          nil
          ((?\_ . "w"))
          nil
          (font-lock-syntactic-keywords . ,scala-font-lock-syntactic-keywords)
          (parse-sexp-lookup-properties . t)))

  ;; Paragraph separation
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^\\s *$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)

  ;; Comment handling
  (make-local-variable 'comment-start)
  (setq comment-start "// ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-multi-line)
  (setq comment-multi-line nil)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *\\|//+ *")
  (make-local-variable 'comment-end-skip)
  (setq comment-end-skip " *\\*+/\\| *")

  ;; Misc
  (make-local-variable 'indent-line-function)
  (setq indent-line-function #'scala-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t))


;; Keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key scala-mode-map [tab] 'scala-indent-line)
(define-key scala-mode-map [(control tab)] 'scala-undent-line)
(define-key scala-mode-map [backspace] 'backward-delete-char-untabify)

(define-key scala-mode-map [(control c)(control l)] 'scala-load-file)
(define-key scala-mode-map [(control c)(control r)] 'scala-eval-region)
(define-key scala-mode-map [(control c)(control b)] 'scala-eval-buffer)

(define-key scala-mode-map [(control c)(control c)] 'comment-region)

(define-key scala-mode-map "}" 'scala-electric-brace)

(easy-menu-define scala-menu-bar scala-mode-map "Scala menu"
  '("Scala"
    ["Run interpreter..."
     run-scala (not (scala-interpreter-running-p))]
    ["Quit interpreter"
     scala-quit-interpreter (scala-interpreter-running-p)]
    ["Load file in interpreter..."
     scala-load-file (scala-interpreter-running-p)]
    ["Switch to interpreter"
     scala-switch-to-interpreter (scala-interpreter-running-p)]
    ["Evaluate region"
     scala-eval-region (and (scala-interpreter-running-p)
                            mark-active)]
    ["Evaluate buffer"
     scala-eval-buffer (scala-interpreter-running-p)]
    "---"
    ["Browse Scala Web site..."
     scala-browse-web-site t]
    ["Customize..."
     scala-customize t]
    ["Report bug..."
     scala-report-bug t]))

;; Syntax tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; strings and character literals
(modify-syntax-entry ?\" "\"" scala-mode-syntax-table)
(modify-syntax-entry ?\\ "\\" scala-mode-syntax-table)

;; different kinds of "parenthesis"
(modify-syntax-entry ?\( "()" scala-mode-syntax-table)
(modify-syntax-entry ?\[ "(]" scala-mode-syntax-table)
(modify-syntax-entry ?\{ "(}" scala-mode-syntax-table)
(modify-syntax-entry ?\) ")(" scala-mode-syntax-table)
(modify-syntax-entry ?\] ")[" scala-mode-syntax-table)
(modify-syntax-entry ?\} "){" scala-mode-syntax-table)

;; special characters
(modify-syntax-entry ?\_ "_" scala-mode-syntax-table)

(dolist (char scala-all-special-chars)
  (modify-syntax-entry char "." scala-mode-syntax-table))
(modify-syntax-entry ?\. "." scala-mode-syntax-table)

;; comments
(modify-syntax-entry ?\/  ". 124b" scala-mode-syntax-table)
(modify-syntax-entry ?\*  ". 23"   scala-mode-syntax-table)
(modify-syntax-entry ?\n "> b" scala-mode-syntax-table)
(modify-syntax-entry ?\r "> b" scala-mode-syntax-table)

(provide 'scala-mode)
