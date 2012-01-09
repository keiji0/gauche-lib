(define-module sexp.comment
  (export read-sexp-comment))

(select-module sexp.comment)

(define (read-sexp-comment inport)
  (let lp ((char (read-char inport)))
    (cond ((eof-object? char)
           char)
          ((and (char=? char #\#) (char=? (read-char inport) #\;))
           (read inport))
          (else
           (lp (read-char inport))))))

#;(test "read-sexp-comment" 'test
        (lambda ()
          (car (call-with-input-file "/Volumes/DATA/design/keiji/2006-07-28-gauche-lib/src/sexp/comment.scm"
                 read-sexp-comment))))

(provide "sexp/comment")
