(define-module util
  (use srfi-1)
  (use util.list)
  (use gauche.collection)
  (autoload file.util current-directory)
  (export keywords-fold
          with-current-directory
          get-error-string
          check=>
          let$
          and*=>
          and=> if=>
          memorize
          receive-alist
          receive-slot
          slot-update!
          define-table
          make-table-closure
          case*
          list-split
          define-enum
          ))

(select-module util)

(define (keywords-fold proc seed keywords)
  (let lp ((seed seed)
           (rest keywords))
    (if (null? rest) seed
        (lp (proc (car rest) (cadr rest) seed)
            (cddr rest)))))

(define (with-current-directory dir thu)
  (let ((cd (current-directory)))
    (dynamic-wind
        (lambda () (current-directory dir))
        thu
        (lambda () (current-directory cd)))))

(define (get-error-string e . maybe-detail?)
  (if (get-optional maybe-detail? #f)
      (call-with-output-string
        (cut with-error-to-port <> (cut report-error e)))
      (ref e 'message)))

#;(use srfi-13)

#;(test* "get-error-string" "hoge"
       (with-error-handler
           (lambda (e)
             (get-error-string e))
           (lambda ()
             (error "hoge"))))

#;(test* "get-error-string long-message" "*** ERROR: hoge"
       (with-error-handler
           (lambda (e)
             (get-error-string e #t))
           (lambda ()
             (error "hoge")))
       (lambda (a b)
         (string-prefix? a b)))

(define check=>
  (case-lambda
   ((? r) (and (? r) r))
   ((? r d) (or (check=> ? r) d))))

#;(begin
    (test* "check=>" '(a b c)
           (check=> pair? '(a b c)))

    (test* "check=>" #f
           (check=> pair? #f))

    (test* "check=>" 88
           (check=> pair? #f 88)))

(define (let$ val . proc)
  (if (null? proc)
      val
      (begin ((car proc) val)
             (apply let$ val (cdr proc)))))

#;(test* "let$" '(1 2 3)
       (let$ (list 1 2 3)
             display))

(define (memorize proc . keywords)
  (let ((ht (make-hash-table (get-keyword :type keywords 'string=?))))
    (lambda (arg)
      (or (hash-table-get ht arg #f)
          (let$ (proc arg)
                (cut hash-table-put! ht arg <>))))))

(define (and*=> ? x . ?...)
  (cond ((not (? x))
         #f)
        ((null? ?...)
         x)
        (else
         (apply and*=> ? ((car ?...) x)
                (cdr ?...)))))

;; (define and=>
;;   (cut and*=> (lambda (x) (not (eq? x #f))) <> <...>))

(define-syntax and=>
  (syntax-rules ()
    ((_ x) x)
    ((_ x y) (and x (y x)))
    ((_ x y . z) (and x (and=> (y x) . z)))))

(define ones
  (lambda (proc)
    (let ((call? #f) (result #f))
      (lambda args
        (if call?
            result
            (begin (set! result (apply proc args))
                   (set! call? #t)
                   result))))))

#;(begin
    (test* "and*=>" #\a
           (and*=> (any-pred string? char?) "abc"
                   (cut string-ref <> 0)))
    (test* "and*=>" #f
           (and*=> string? 888))
    (test* "and=> 1" '20
           (and=> (assq 'b '((a . 8) (b . 20))) cdr))
    (test* "and=> 2" #f
           (and=> (assq 'k '((a . 8) (b . 20))) cdr))
    (test* "and=> 3" 3
           (and=> 3)))

(define-syntax if=>
  (syntax-rules ()
    ((_ b) b)
    ((_ b t) (and=> b t))
    ((_ b t e) (or (and=> b t) e))))

(define-syntax receive-alist
  (syntax-rules (=>)
    ((_ (name ...) alis => proc)
     (apply proc (map (pa$ assq-ref alis) '(name ...))))
    ((_ (name ...) alis body ...)
     (receive-alist (name ...) alis => (lambda (name ...) body ...)))))

#;(begin
    (test* "receive-alist 1" '(20 . 50)
           (receive-alist (a c) '((a . 20) (b . 80) (c . 50))
                          (cons a c)))

    (test* "receive-alist 2" '(20 50)
           (receive-alist (a c) '((a . 20) (b . 80) (c . 50))
                          => list)))

(define-syntax receive-slot
  (syntax-rules (=>)
    ((_ (name ...) obj => proc)
     (apply proc (map (pa$ slot-ref obj) '(name ...))))
    ((_ (name ...) obj body ...)
     (receive-slot (name ...) obj => (lambda (name ...) body ...)))))

#;(begin
    (define-class <hoge> ()
      ((a :init-keyword :a)
       (b :init-keyword :b)
       (c :init-keyword :c)))

    (test* "receive-slot 1" '(20 30 40)
           (receive-slot (a b c) (make <hoge> :a 20 :b 30 :c 40)
                         (list a b c)))

    (test* "receive-slot 2" '(20 40 . 30)
           (receive-slot (a c b) (make <hoge> :a 20 :b 30 :c 40)
                         => list*)))

(define (slot-update! o sn proc)
  (let$ (proc (slot-ref o sn))
        (pa$ slot-set! o sn)))

#;(test* "slot-update!" 88
       (ref (let$ (make <hoge> :a "jojo")
                  (cut slot-update! <> 'a (lambda _ 88)))
            'a))

(define-syntax define-table
  (syntax-rules ()
    ((_ (get put . delete) . type)
     (define-values (get put . delete)
       (let ((table (apply make-hash-table 'type)))
         (apply values (pa$ hash-table-get table)
                (pa$ hash-table-put! table)
                (if (null? 'delete) '()
                    (list (pa$ hash-table-delete! table)))))))))

#;(begin
    (test* "define-table (eq?)" 8
           (begin
             (define-table (get put!) eq?)
             (put! 'x 8)
             (get 'x)))

    (test* "define-table (string=?)" 8
           (begin
             (define-table (get put!) string=?)
             (put! "x" 8)
             (get "x")))

    (test* "define-table ()" 'b
           (begin
             (define-table (get put!))
             (put! 'b 'b)
             (get 'b)))

    (test* "define-table (get x . def)" "hoge"
           (begin
             (define-table (get put!))
             (get 'b "hoge")))

    (test* "define-table (add delete!)" '(80 . #f)
           (begin
             (define-table (get put! delete!))
             (put! 'x 80)
             (cons (get 'x)
                   (begin (delete! 'x)
                          (get 'x #f)))))
    )

(define (make-table-closure . keywords)
  (let ((table (make-hash-table (get-keyword* 'type keywords 'eq?))))
    (getter-with-setter
     (lambda k
       (if (null? k)
           table
           (apply hash-table-get table k)))
     (lambda (k v)
       (hash-table-put! table k v)))))

(define-syntax case*
  (syntax-rules (else)
    ((_ key) #f)
    ((_ key (else expr ...)) (begin expr ...))
    ((_ key ((sexp ...)) clause ...)
     (syntax-error "expressions are not found in case*" (sexp ...)))
    ((_ key ((sexp ...) expr ...) clause ...)
     (let ((evaled-key key))
       (if (member evaled-key '(sexp ...))
           (begin expr ...)
           (case* evaled-key clause ...))))
    ((_ other . more)
     (syntax-error error "bad clause in case*" other))))

#;(test* "case*" 'ok
       (case* (list 2 3 8)
         (((2 3 8)) 'ok)
         (else #f)))

(define (list-split lis sep?)
  (let loop ((lis lis)
             (slis '())
             (result '()))
    (cond ((null? lis)
           (reverse! (cons (reverse slis) result)))
          ((sep? (car lis))
           (loop (cdr lis) '() (cons (reverse! slis) result)))
          (else
           (loop (cdr lis) (cons (car lis) slis) result)))))

;; (define (string-limited-gen str reader limit . suffix)
;;   (if (< (string-length str) limit)
;;       str
;;       (call-with-string-io str
;;         (lambda (in out)
;;           (let* ((suffix (get-optional suffix ""))
;;                  (suffix-len (string-length suffix)))
;;             (let loop ((limit (- limit suffix-len))
;;                        (obj (reader in)))
;;               (cond ((eof-object? obj) 'done)
;;                     ((zero? limit) (display suffix out))
;;                     (else
;;                      (display obj out)
;;                      (loop (- limit 1)
;;                            (reader in))))))))))

;; (define string-limited (cut string-limited-gen <> read-char <...>))
;; (define string-limited-line (cut string-limited-gen <> read-line <...>))

;; (define (size-format size)
;;   (let loop ((size size)
;;              (type 'B))
;;     (if (zero? size)
;;         (if (eq? size 'B) (list size type) #f)
;;         (or (case type
;;               ((B) (loop (quotient size 1024) 'K))
;;               ((K) (loop (quotient size 1024) 'M))
;;               ((M) (loop (quotient size 1024) 'G))
;;               ((G) (loop (quotient size 1024) 'T))
;;               ((T) (loop (quotient size 1024) 'E)))
;;             (list size type)))))

(define-syntax define-enum
  (syntax-rules ()
    ((_ (count (id %val)))
     (define-syntax id
       (syntax-rules ()
         ((_) count)
         ((_ 'val) %val))))
    ((_ (count id))
     (define id count))
    ((_ (count (id val) . rest))
     (begin
       (define-enum (count (id val)))
       (define-enum ((+ count 1) . rest))))
    ((_ (count id . rest))
     (begin
       (define-enum (count id))
       (define-enum ((+ count 1) . rest))))
    ((_ ((id val) . rest1) . rest2)
     (define-enum (0 (id val) . rest1)))
    ((_ (count (id val) . rest1) . rest2)
     (begin
       (define-enum (count (id val) . rest1))
       (define-enum . rest2)))))

(provide "util")
