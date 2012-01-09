(define-module unix
  (use srfi-1)
  (use file.util)
  (use text.parse)
  (export))

(select-module unix)

(define *unix:commands* (make-hash-table))

(define unix:get-command (pa$ hash-table-get *unix:commands*))

(define unix:put-command! (pa$ hash-table-put! *unix:commands*))

(define-class <unix:command> ()
  ((name :init-keyword :name)
   (proc :init-keyword :proc)))

(define (unix:command-line-custom arg-names arg-values)
  (map (lambda (arg-name arg-value)
         (cond ((#/\*$/ (symbol->string arg-name))
                (if (pair? arg-value)
                    arg-value
                    (list arg-value)))
               (else
                arg-value)))
       arg-names arg-values))

(define (unix:keywords-normalization keywords)
  (let lp ((keywords keywords)
           (result '()))
    (cond ((null? keywords)
           (apply append (reverse! result)))
          (else
           (receive (key val) (car+cdr keywords)
             (if (keyword? key)
                 (if (or (null? val)
                         (keyword? (car val)))
                     (lp (cdr keywords)
                         (cons (list key #t) result))
                     (lp (cddr keywords)
                         (cons (list key (car val)) result)))
                 (error "unix:keywords-normalization")))))))

(define-syntax define-unix:command
  (syntax-rules (?)
    ((_ (name . args) ? (keyword ...) . body)
     (define-unix:command name
       (lambda args-2
         (apply (lambda (keyword ...)
                  (apply (lambda args . body)
                         (unix:command-line-custom 'args args-2)))
                (let1 keyword-rest (unix:keywords-normalization (drop args-2 (length 'args)))
                  (map (lambda (key)
                         (get-keyword key keyword-rest #f))
                       (map (compose make-keyword symbol->string) '(keyword ...))))))))
    ((_ (name . args) . body)
     (define-unix:command (name . args) ? () . body))
    ((_ name proc)
     (unix:put-command! 'name proc))))

(define (unix:eval command-line)
  (define (parse command-line)
    ;; syntax
    ;; | = pipe
    ;; > = redirect ; 
    (call-with-input-string command-line
      (lambda (in)
        (skip-while #[\s] in)
        (let lp ((commands '()))
          (if (eof-object? (peek-char in))
              (reverse! commands)
              (let lp-2 ((sexp (read in))
                         (command '()))
                (if (eof-object? sexp)
                    (lp (cons (reverse! command) commands))
                    (begin
                     (skip-while #[\s] in)
                     (let1 char (peek-char in)
                       (cond ((eof-object? char)
                              (lp (cons (reverse! (cons sexp command)) commands)))
                             ((char-set-contains? #[\|<>] char)
                              (read-char in)
                              (lp (cons* char (reverse! (cons sexp command)) commands)))
                             (else
                              (lp-2 (read in) (cons sexp command)))))))))))))

  (define (command-compile command-line)
    (map (lambda (sexp)
           (cond ((symbol? sexp)
                  (or (unix:get-command sexp #f)
                      (eval sexp (find-module 'user))))
                 ((and (string? sexp)
                       (#/\?|\*|\[.*\]/ sexp))
                  (sys-glob sexp))
                 ((regexp? sexp)
                  (directory-list (current-directory) :children? #t :filter sexp))
                 (else
                  sexp)))
         command-line))
  )
              
(define-unix:command (echo str)
  (print str))

(define-unix:command (mv source* target) ? (v)
  (dolist (source source*)
    (move-file source target)
    (when v
      (format #t "~a -> ~a\n" source target))))

(define-unix:command (pwd)
  (current-directory))

(define-unix:command (cd dir)
  (current-directory dir))

(provide "unix")
