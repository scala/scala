;; This rule file defines the environment for the expansion of
;; Tuple.tmpl template.
;;
;; It defines the following variables:
;;
;;   Name          Meaning                Value for Tuple2
;;   -----------------------------------------------------
;;   n             Arity of the tuple     2
;;   type-params   Type parameters        [T1, T2]
;;   value-params  Value parameters       _1: T1, _2: T2
;;   equal         Equality test          (_1 == that._1) && (_2 == that._2)
;;   hash-code     Body of hashCode()     _1.hashCode() ^ _2.hashCode()
;;   to-string     Body of toString()     "(" + _1 + "," + _2 + ")"

(define (repeated-string f n sep)
  (string-join (map f (list-tabulate n (lambda (x) (+ x 1)))) sep))

(define (make-type-params n)
  (string-append "[" (repeated-string (lambda (i) (format #f "T~a" i)) n ", ") "]"))

(define (make-value-params n)
  (repeated-string (lambda (i) (format #f "_~a: T~a" i i)) n ", "))

(define (make-equal n)
  (repeated-string (lambda (i) (format #f "(_~a == that._~a)" i i)) n " && "))

(define (make-hash-code n)
  (repeated-string (lambda (i) (format #f "_~a.hashCode()" i)) n " ^ "))

(define (make-to-string n)
  (string-append
   "\"(\" + "
   (repeated-string (lambda (i) (format #f "_~a" i)) n " + \",\" + ")
   " + \")\""))

(define file-name-rx (rx "Tuple" (submatch (+ digit)) ".scala"))

(define (make-env target-file)
  (let ((match (regexp-search file-name-rx target-file)))
    (if match
        (let ((n (string->number (match:substring match 1))))
          (list (cons 'n (number->string n))
                (cons 'type-params (make-type-params n))
                (cons 'value-params (make-value-params n))
                (cons 'equal (make-equal n))
                (cons 'hash-code (make-hash-code n))
                (cons 'to-string (make-to-string n))))
        (error "invalid file name" target-file))))

