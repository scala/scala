;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Embedded Scala Script Evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Id$

;; Commands:
;;   \begin{scalaprogram}{<name>} ... \end{scalaprogram}
;;     begin a complete Scala program with given name
;;   \beginscalaprogram{<name>} ... \endscalaprogram
;;     begin a Scala program with given name, which can be omitted in
;;     which case the program is anonymous
;;   \scalaprogramoutput{<name>}
;;     insert the output of the given program, which must be ended
;;   \begin{scalacode} ... \end{scalacode}
;;     begin a section of Scala code which is added to the current
;;     program, and printed in the final result
;;   \begin{scalainvisiblecode} ... \end{scalainvisiblecode}
;;     like \begin{scalacode}, but the code doesn't get printed

;; TODO
;; - add interaction with siris using commands like:
;;   \beginscalainteraction, \endscalainteraction, visible/invisible code,
;;   visible code whose result is not visible

;; Diagnostics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define verbose? #t)

(define (message str . args)
  (when verbose?
        (apply format (error-output-port) str args)
        (newline (error-output-port))))

(define (fail line-num msg . args)
  (format #t "~a:~a: ~a" "<in-file>" line-num (apply format #f msg args)))

;; Syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax when
  (syntax-rules ()
    ((when cond body1 body2 ...)
     (if cond (begin body1 body2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless cond body1 body2 ...)
     (if (not cond) (begin body1 body2 ...)))))

;; Temporary directories / files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; AList associating temporary directories with program names.
(define temp-dirs '())

(define (get-temp-dir prog-name)
  (cond ((assoc prog-name temp-dirs) => cdr)
        (else
         (let ((dir (temp-file-iterate (lambda (dir) (create-directory dir) dir)
                                       "/tmp/temp-~a")))
           (set! temp-dirs (alist-cons prog-name dir temp-dirs))
           dir))))

(define (clean-temp-dirs)
  (for-each (lambda (file/dir)
              (let ((dir (cdr file/dir)))
                (message "Cleaning up temporary class directory (~a)" dir)
                (run (rm -rf ,dir))))
            temp-dirs))

(define (scala-file-name prog-name)
  (expand-file-name (replace-extension prog-name ".scala")
                    (get-temp-dir prog-name)))

(define (open-scala-program name)
  (open-output-file (scala-file-name name)))

(define (close-scala-program name port)
  (compile-scala-program name)
  (close-output-port port))

;; Compiling and running Scala programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-scala-program prog-name)
  (let ((file-name (scala-file-name prog-name))
        (classes-dir (get-temp-dir prog-name)))
    (message "Compiling file ~a to directory ~a" file-name classes-dir)
    (run (socos -d ,classes-dir ,file-name))))

(define (append-scala-program-output prog-name out-port)
  (let ((classes-dir (get-temp-dir prog-name)))
    (message "Running program ~a" prog-name)
    (with-env (("CLASSPATH" . ,(string-append classes-dir ":" (getenv "CLASSPATH"))))
      (write-string (run/string (java ,prog-name)) out-port))))


;; Main iteration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (begin-scala-code port)
  (format port "\\begin{verbatim}\n"))
(define (end-scala-code port)
  (format port "\\end{verbatim}\n"))

(define (scala-rx contents)
  (rx bos (* space) ,@contents (* space)))

(define (scala-begin-rx env-name)
  (scala-rx (string-append "\\begin{" env-name "}")))
(define (scala-end-rx env-name)
  (scala-rx (string-append "\\end{" env-name "}")))

(define scalatex
  (let ((prog-name-rx (rx (* (| alphanum ("_"))))))
    (lambda (in-port out-port)
      (awk (read-line in-port) (line) line-num ((prog-name #f)
                                                (prog-port #f)
                                                (expr #f)
                                                (in-fragment? #f)
                                                (copy? #t))
           ;; Program.
           ((regexp-search (scala-rx (rx "\\beginscalaprogram"
                                         "{" (submatch ,prog-name-rx) "}"))
                           line)
            => (lambda (match)
                 (let ((prog-name (or (match:substring match 1) "anonymous")))
                   (values prog-name
                           (open-scala-program prog-name)
                           #f
                           #f
                           copy?))))

           ((regexp-search (scala-rx "\\endscalaprogram") line)
            (close-scala-program prog-name prog-port)
            (values #f #f #f #f copy?))

           ((regexp-search (scala-rx (rx "\\begin{scalaprogram}"
                                         "{" (submatch ,prog-name-rx) "}"))
                           line)
            => (lambda (match)
                 (begin-scala-code out-port)
                 (let ((prog-name (or (match:substring match 1) "anonymous")))
                   (values prog-name
                           (open-scala-program prog-name)
                           #f
                           #t
                           #t))))

           ((regexp-search (scala-end-rx "scalaprogram") line)
            (end-scala-code out-port)
            (close-scala-program prog-name prog-port)
            (values #f #f #f #f copy?))

           ;; Insertion of program output.
           ((regexp-search (scala-rx (rx "\\scalaprogramoutput"
                                         "{" (submatch ,prog-name-rx) "}"))
                           line)
            => (lambda (match)
                 (let ((prog-name (match:substring match 1)))
                   (begin-scala-code out-port)
                   (append-scala-program-output prog-name out-port)
                   (end-scala-code out-port))
                 (values prog-name prog-port #f in-fragment? copy?)))

           ;; Visible code fragment.
           ((regexp-search (scala-begin-rx "scalacode") line)
            (begin-scala-code out-port)
            (values prog-name prog-port #f #t #t))

           ((regexp-search (scala-end-rx "scalacode") line)
            (end-scala-code out-port)
            (values prog-name prog-port #f #f #t))

           ;; Invisible code fragment.
           ((regexp-search (scala-begin-rx "scalainvisiblecode") line)
            (values prog-name prog-port #f #t #f))

           ((regexp-search (scala-end-rx "scalainvisiblecode") line)
            (values prog-name prog-port #f #f #t))

           (else
            (when in-fragment?
                  (write-string line prog-port)
                  (newline prog-port))
            (when copy?
                  (write-string line out-port)
                  (newline out-port))
            (values prog-name
                    prog-port
                    (and expr (string-append expr line))
                    in-fragment?
                    copy?))))))

(define (display-usage-and-exit prog)
  (format #t "Usage: ~a <in-file> <out-file>\n" prog))

(define (main cmd-line)
  (let ((prog (car cmd-line))
        (args (cdr cmd-line)))
    (if (= 2 (length args))
        (call-with-input-file (first args)
          (lambda (in-port)
            (call-with-output-file (second args)
              (lambda (out-port)
                (format out-port "%% DO NOT EDIT - AUTOMATICALLY GENERATED FILE\n")
                (format out-port "%% Generated from \"~a\" on ~a by ~a\n\n"
                        (first args)
                        (format-date "~Y-~m-~d [~H:~M]" (date))
                        (user-login-name))
                (scalatex in-port out-port)
                (clean-temp-dirs)))))
        (display-usage-and-exit prog))))
