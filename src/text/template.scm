(define-module text.template
  (use file.util)
  (export text-template-read&out
          text-template-apply
          text-template-string-apply)
  )

(select-module text.template)

;; [keyword](...)を評価する。carが文字列でcdrがnullである場合、それが評価される。
(define (text-template-read&out . keywords)
  (let-keywords* keywords ((keyword #\$)
                           (input (current-input-port))
                           (output (current-output-port))
                           (environment (find-module 'user)))
    (with-output-to-port output
      (lambda ()
        (let lp ((char (read-char input)))
          (cond ((eof-object? char))
                ((and (char=? keyword char)
                      (char=? #\( (peek-char input)))
                 (with-error-handler
                     (lambda (e)
                       (when (is-a? e <error>)
                         (format #t "error ~s: ~a" e (ref e 'message))))
                   (lambda ()
                     (let* ((sexp (read input))
                            (car-sexp (eval (car sexp) environment)))
                       (if (and ((any-pred string? number? symbol?) car-sexp) (null? (cdr sexp)))
                           (display car-sexp)
                           (eval sexp environment)))))
                 (lp (read-char input)))
                (else
                 (display char)
                 (lp (read-char input)))))))))

(define (text-template-apply path . to-name)
  (if (file-is-readable? path)
      (let ((name (sys-basename path))
            (dir (sys-dirname path)))
        (cond ((#/^(.+)\+scm$/ name)
               => (lambda (m)
                    (with-input-from-file path
                      (lambda ()
                        (with-output-to-file (string-append dir "/" (get-optional to-name (m 1)))
                          (cut text-template-read&out))))))
              (else #f)))
      (error "No such file or directory" path)))

(define (text-template-string-apply str)
  (with-string-io str text-template-read&out))

(provide "text/template")
