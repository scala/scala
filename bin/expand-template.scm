#!/home/linuxsoft/bin/scsh \
-e main -s
!#

;; $Id$

(define expand
  (let ((variable-rx (rx "[#" (submatch (+ (| alphanum ("-")))) "#]")))
    (lambda (env in-port out-port)
      (awk (read-line in-port 'concat) (line) ()
           (#t (regexp-substitute/global out-port
                                         variable-rx
                                         line
                                         'pre
                                         (lambda (match)
                                           (let* ((var (string->symbol
                                                        (match:substring match 1)))
                                                  (expansion (assoc var env)))
                                             (if expansion
                                                 (cdr expansion)
                                                 (error "Unknown variable in template"
                                                        var))))
                                         'post))))))

(define (display-usage-and-exit prog)
  (format #t "Usage: ~a <rule-file> <template-file> <target-file>~%" prog)
  (format #t "where <rule-file> is a Scheme file containing expansion rules,~%")
  (format #t "      <template-file> is the template file, and~%")
  (format #t "      <target-file> is the file to generate.~%")
  (exit 1))

(define initial-environment
  '((do-not-edit . "DO NOT EDIT. Automatically generated file!")))

(define (main cmd-line)
  (let ((prog (car cmd-line))
        (args (cdr cmd-line)))
    (if (= (length args) 3)
        (let ((rule (first args))
              (tmpl (second args))
              (file (third args)))
          (load rule)
          (call-with-input-file tmpl
            (lambda (in-port)
              (call-with-output-file file
                (lambda (out-port)
                  (expand (append initial-environment (make-env file))
                          in-port
                          out-port))))))
        (display-usage-and-exit prog))))
