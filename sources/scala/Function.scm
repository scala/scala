;; This rule file defines the environment for the expansion of
;; Function.tmpl template.
;;
;; It defines the following variables:
;;
;;   Name          Meaning                Value for Function1
;;   --------------------------------------------------------
;;   n             Arity of the function  1
;;   type-params   Type parameters        [?A1, ?R]
;;   java-params   Java parameters        java.lang.Object a1
;;   scala-params  Scala parameters       (?A1) ?R

;; $Id$

(define (gen-variables stem n)
  (let loop ((i 1) (res '()))
    (if (> i n)
        (reverse! res)
        (loop (+ i 1) (cons (string-append stem (number->string i)) res)))))

(define (make-type-params n)
  (string-append "["
                 (string-join (append (gen-variables "?A" n)
                                      '("?R"))
                              ", ")
                 "]"))

(define (make-java-params n)
  (string-join (gen-variables "java.lang.Object a" n) ", "))

(define (make-scala-params n)
  (string-append "("
                 (string-join (gen-variables "?A" n) ", ")
                 ") ?R"))

(define file-name-rx (rx (* any) "Function" (submatch (+ digit)) ".java"))

(define (make-env target-file)
  (let ((match (regexp-search file-name-rx target-file)))
    (if match
        (let ((n (string->number (match:substring match 1))))
          (list (cons 'n (number->string n))
                (cons 'type-params (make-type-params n))
                (cons 'java-params (make-java-params n))
                (cons 'scala-params (make-scala-params n))))
        (error "invalid file name" target-file))))

