(define-target 'test
  (define-target (cut regexp-replace #/\.scm/ <> "") <= #/\.scm\.test$/
    (lambda (_ depend)
      (sys-system (format "gosh -I../src ~s >> test.log" depend)))))

(define-target 'clean '()
  "rm -rf *.log *.tmp")
