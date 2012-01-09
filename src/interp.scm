(define-module interp
  (use util)
  (export get-interp add-interp! *default-encoding*
          interp-generic make-content-type
          define-interp))

(select-module interp)

(define-table (get-interp add-interp!) eq?)

(define-constant *default-encoding*
  (let ((ec (gauche-character-encoding)))
    (if (eq? 'none ec) 'euc-jp ec)))

(define (make-content-type type)
  (string-append type "; charset="
                 (symbol->string *default-encoding*)))

(define (interp-generic args)
  (cond ((get-interp (car args) #f)
         => (cut apply <> (cdr args)))
        (else
         (error "interp not defined:" (car args)))))

;; return (values ((key val) ...)
;;                (<string> | <text-tree>))
(define-syntax define-interp
  (syntax-rules ()
    ((_ (name . args) . body)
     (define-interp name (lambda args . body)))
    ((_ name expr)
     (add-interp! 'name expr))))

(define-constant *TEXT-CONTENT-TYPE* (make-content-type "text/plain"))

(define-interp (text string)
  (values `((content-type . ,*TEXT-CONTENT-TYPE*)) string))

(provide "interp")
