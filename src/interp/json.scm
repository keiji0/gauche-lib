(define-module interp.json
  (use interp)
  (export sexp->json write-sexp->json))

(select-module interp.json)

(define-constant *JS-CONTENT-TYPE*
  (make-content-type "text/javascript"))

(define json-primitive-value?
  (any-pred string? number? symbol?))

(define-interp (json sexp)
  (values (sexp->json sexp) *JS-CONTENT-TYPE*))

(define (write-sexp->json s)
  (cond
   ((json-primitive-value? s)
    (write s))
   ((boolean? s)
    (display (if s "true" "false")))
   ((null? s)
    (display "[]"))
   ((pair? s)
    (if (pair? (car s))
        (begin (display "{")
               (let loop ((s s))
                 (cond ((null? s))
                       (else
                        (write (x->string (caar s)))
                        (display ":")
                        (write-sexp->json (cdar s))
                        (unless (null? (cdr s)) (display ","))
                        (loop (cdr s)))))
               (display "}"))
        (write-sexp->json (list->vector s))))
   ((vector? s)
    (display "[")
    (let1 m (- (vector-length s) 1)
      (let loop ((i 0))
        (if (= m i)
            (begin (write-sexp->json (vector-ref s i))
                   (display "]"))
            (begin (write-sexp->json (vector-ref s i))
                   (display ",")
                   (loop (+ i 1)))))))
   ((procedure? s)
    (s))
   (else
    (error "JSON - object not serializable:" s))))

(define (sexp->json s)
  (with-output-to-string (cut write-sexp->json s)))

(provide "interp/json")
