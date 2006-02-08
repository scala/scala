#!/bin/sh
exec scsh -e main -s "$0" "$@"
!#

;; Script to perform the nightly test/build of Scala.
;;
;; Always make sure that the latest version of this file is copied to
;; ~scalatest/bin/scala-nightly-test.scm
;;
;; $Id$

;; Makefile.private to use for the build.
(define private-makefile "Makefile.private")
(define source-private-makefile
  (expand-file-name (string-append "~scalatest/support/" private-makefile)))

;; CVS repository containing the Scala compiler.
(define scala-cvs-repository-dir
  (expand-file-name "~cvslamp/repositories/scala"))
;; CVS module containing the compiler.
(define scala-cvs-module-name "scala")

;; E-mail address to which the failure notification should be sent.
(define notify-email "scala-devel@groupes.epfl.ch")
; (define notify-email "Michel.Schinz@epfl.ch") ; DEBUG

;; Directory in which the distribution should be built.
(define nightly-build-dir
  (expand-file-name "~linuxsoft/archives/scala/nightly"))

;; End of configuration section.

(define (main cmd-line)
  (let ((prog (car cmd-line))
        (args (cdr cmd-line)))
    (if (= 1 (length args))
        (scala-test (first args))
        (display-usage-and-exit prog))))

(define (display-usage-and-exit prog)
  (format #t "Usage: ~a <result-dir>\n" prog)
  (exit 1))

(define (get-public-link file)
  (temp-file-iterate (lambda (link) (create-symlink file link) link)
                     (expand-file-name "~/public_html/log-~a.txt")))

(define (get-checkout-dir base-dir date)
  (expand-file-name (format-date "~Y-~m-~d" date) base-dir))

(define (start-section title)
  (format #t "\n* ~a\n\n" title))

(define (scala-test base-dir)
  (let* ((checkout-dir (get-checkout-dir base-dir (date)))
         (log-file (expand-file-name "log" checkout-dir)))
    (create-directory checkout-dir)
    (call-with-output-file log-file
      (lambda (log-port)
        (with-current-output-port log-port
          (with-current-error-port log-port
            (stdports->stdio)
            (with-cwd checkout-dir
              (if (not (call-with-current-continuation scala-do-test))
                  (let ((link (get-public-link log-file)))
                    (send-warning-mail log-file
                                       (file-name-nondirectory link)))))))))))

(define (scala-do-test return)
  (dynamic-wind
      (lambda ()
        (display "In Emacs, read this file in -*- Outline -*- mode\n")
        (start-section "Starting time")
        (display (format-date "~Y-~m-~d ~H:~M ~Z\n" (date))))
      (lambda ()
        (let ((fail-if-error (lambda (code) (if (not (zero? code))
                                                (return #f)))))
          (start-section "Checking out Scala module")
          (fail-if-error (run (cvs -d ,scala-cvs-repository-dir
                                   checkout
                                   -P
                                   ,scala-cvs-module-name)))
          (with-cwd scala-cvs-module-name
                    (start-section "Creating link(s)")
                    (create-symlink source-private-makefile private-makefile)
                    (start-section "Compiling Scala compiler")
                    (fail-if-error (run (make)))
                    (start-section "Testing Scala compiler (1)")
                    (fail-if-error (run (make test)))
                    (start-section "Testing Scala compiler (2)")
                    (fail-if-error
                     (run (./objects/main/bin/scalatest --color=none
                                                        --show-log)))
                    (start-section "Creating nightly distribution")
                    (run (rm -rf ,nightly-build-dir))
                    (create-directory nightly-build-dir)
                    (fail-if-error (run (make distrib) (< /dev/null)))
                    #t)))
      (lambda ()
        (start-section "Ending time")
        (display (format-date "~Y-~m-~d ~H:~M ~Z\n" (date))))))

(define (send-warning-mail log-file-name link-name)
  (send-mail
   notify-email
   `(("Subject"  . "Failure of nightly scala test")
     ("To"       . ,notify-email)
     ("Reply-To" . ,notify-email))
   (string-append
    "Tonight's automatic Scala test failed. More details can be found\n"
    "in file "log-file-name"\n"
    "which is available through the following URL:\n\n"
    "  http://lamp.epfl.ch/~scalatest/"link-name)))

(define (send-mail to headers body)
  (let ((mail-port (make-string-output-port)))
    (for-each (lambda (name/contents)
                (format mail-port "~a: ~a\n"
                        (car name/contents)
                        (cdr name/contents)))
              headers)
    (newline mail-port)
    (write-string body mail-port)
    (newline mail-port)
    (run (sendmail "-i" ,to)
         (<< ,(string-output-port-output mail-port)))))

;;; Local Variables:
;;; mode:scheme
;;; End:
