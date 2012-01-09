(use file.util)
(use test.inline)

(define *scm-files* (directory-fold "src" (lambda (a b) (if (#/\.scm$/ a) (cons a b) b)) '()))

(define-target 'test
  (define-target (cut regexp-replace #/\.scm$/ <> ".test") <= *scm-files*
    (lambda (_ depend)
      (test-inline (string-append "./" depend)))))
