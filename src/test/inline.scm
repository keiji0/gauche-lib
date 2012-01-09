(define-module test.inline
  (use srfi-1)
  (use gauche.test)
  (use util.match)
  (use sexp.comment)
  (export test-inline))

(select-module test.inline)

(define (test-inline path . module-name)
  
  (define (find-module-name path)
    (call-with-input-file path
      (lambda (in)
        (let lp ((x (read in)))
          (if (eof-object? x)
              (error "error: (test-inline path module-name)")
              (match x
                (('define-module name . _) name)
                (_ (lp (read in)))))))))
  
  (let ((module-name (get-optional module-name (find-module-name path))))
    (unless (find-module module-name) (load path))
    (let ((module (find-module module-name))
          (sexps (filter list? (call-with-input-file path (pa$ port->list read-sexp-comment)))))
      (dynamic-wind
          (lambda ()
            (test-start (symbol->string module-name))
            (test-module module-name))
          (lambda ()
            (eval '(use gauche.test) module)
            (eval (cons 'begin sexps) module))
          (lambda ()
            (test-end))))))

(provide "test/inline")
