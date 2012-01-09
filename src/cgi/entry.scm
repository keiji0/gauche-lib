(define-module cgi.entry
  (use srfi-1)
  (use www.cgi)
  (use text.tree)
  (use util.list)
  (use util)
  (use gauche.parameter)
  (export define-entry))

(select-module cgi.entry)

;; HTTP-HEADERの連想リスト
(define %http-header (make-parameter '()))

;; HTTP-HEADERをセットする
(define http-header
  (getter-with-setter
   (lambda ()
     (%http-header))
   (lambda (k v)
     (%http-header (assq-set! (%http-header) k v)))))

(define (entry-main)

  (define (entry-apply path-info method parameter)
    (let* ((entry-name (car path-info))
           (entry (get-entry entry-name method)))
      (apply entry parameter (cdr path-info))))

  (define (publish result)
    ;; ヘッダとボディをまとめて出力
    (define (entry-reply head entity)
      ;; ヘッダを標準出力に出力する
      (print "Status: " (assq-ref head 'status "200 OK") "\r")
      ;; Header filed
      (unless (assq-ref head 'content-type)
        (print "content-type: text/plain\r"))
      (dolist (h (alist-delete 'status head))
        (print (car h) ": " (cdr h) "\r"))
      (print "\r")
      ;; ボディを出力、返り値によって
      (cond ((procedure? entity)
             (entity))
            ((pair? entity)
             (write-tree entity))
            ((string? entity)
             (display entity))
            ((input-port? entity)
             (copy-port entity (current-output-port))
             (close-input-port entity))
            (else
             (errorf "Illegal entity ~a" entity))))
    (cond ((string? result)
           (entry-reply (http-header) result))
          ((pair? result)
           (receive (head body) (interp-generic result)
             (entry-reply (append head (http-header)) body)))
          (else
           (entry-reply (http-header) ""))))

  (define (entry-error-handler e)
    (set! (http-header 'status) (get-status-code e))
    (cond ((get-viewer "error")
           => (cut <> e))
          (else
           (get-error-string e))))

  (with-error-handler (with-module www.cgi cgi-default-error-proc)
    (lambda ()
      (parameterize ((%http-header (http-header)))
        (publish
          (with-error-handler entry-error-handler
            (lambda ()
              (entry-apply
               (path-info->list (cgi-get-metavariable "PATH_INFO"))
               (cgi-get-metavariable "REQUEST_METHOD")
               (cgi-parse-parameters :part-handlers '((#f file=name)))))))))))

(define-values (get-entry add-entry!)
  (let ((et (make-hash-table 'string=?)))
    (values
      (lambda (entry method)
        (cond ((hash-table-get et entry #f)
               => (lambda (mt)
                    (or (hash-table-get mt method #f)
                        (entry-error "405 Method Not Allowed"))))
              (else
               (entry-error "404 Not found" "\"~a\" not found" entry))))
      (lambda (entry method proc)
        (let* ((key (symbol->string entry))
               (mt (if (hash-table-exists? et key)
                       (hash-table-get et key)
                       (let$ (make-hash-table 'string=?)
                             (pa$ hash-table-put! et key)))))
          (hash-table-put! mt (symbol->string method) proc))))))

(define-syntax define-entry
  (syntax-rules (=)
    ((_ ((entry) = %entry . args) . more)
     (define-entry GET ((entry) = %entry . args) . more))
    ((_ method ((entry) = %entry . args) . more)
     (let ((%entry 'entry))
       (define-entry method (entry . args) . more)))
    ((_ ((entry more-entry ...) = %entry . args) . more)
     (define-entry GET ((entry more-entry ...) = %entry . args) . more))
    ((_ method ((entry more-entry ...) = %entry . args) . more)
     (begin (define-entry method ((entry) = %entry . args) . more)
            (define-entry method ((more-entry ...) = %entry . args) . more)))
    ((_ (entry . args) . more)
     (define-entry GET (entry . args) . more))
    ((_ method (entry . args) . more)
     (add-entry! 'entry 'method (%make-entry args . more)))))

(define-syntax %make-entry
  (syntax-rules (?)
    ((_ args ? (query ...) . body)
     (lambda (params . args)
       (apply (lambda (query ...) . body)
              (map (lambda (q)
                     (cgi-get-parameter (symbol->string q) params))
                   '(query ...)))))
    ((_ args ? query . body)
     (lambda (query . args) . body))
    ((_ args . body)
     (lambda (_ . args) . body))))

(define-condition-type <entry-error> <error> #f (status))

(define-method get-status-code ((e <error>))
  "500 Internal Server Error")

(define-method get-status-code ((e <entry-error>))
  (slot-ref e 'status))

(define (entry-error status . args)
  (raise
   (condition
    (<entry-error> (status status)
                  (message (if (null? args)
                               status
                               (apply format args)))))))

(define (server-name)
  (or (cgi-get-metavariable "SERVER_NAME") "localhost"))

(define (script-name)
  (or (cgi-get-metavariable "SCRIPT_NAME") "/dummy.cgi"))

(define (server-uri)
  (let ((schema (if (cgi-get-metavariable "HTTPS") "https" "http"))
        (port (or (x->integer (cgi-get-metavariable "SERVER_PORT")) 80)))
    (string-append schema "://" (server-name)
                   (if (or (and (= port 80) (string=? schema "http"))
                           (and (= port 443) (string=? schema "https")))
                       ""
                       (string-append ":" (number->string port))))))

(define (self-uri . paths)
  (string-append (server-uri) (apply self-uri paths)))

(define self-dir build-path)

(define (entry-uri entry/... querys)
  (string-append (cgi-uri entry/...) "?" (build-query-string querys)))

(define (self-entry-uri entry/... querys)
  (string-append (script-name) "/" entry/... "?" (build-query-string querys)))

(define (build-query-string param-alist)
  (string-join
    (map (lambda (p)
           (string-append
            (uri-encode-string (symbol->string (car p))) "="
            (uri-encode-string (cdr p))))
         param-alist)
    "&"))

(define (path-info->list path-info)
  (let1 l (cdr (string-split (or path-info "") #\/))
    (if (or (null? l)
            (equal? (car l) ""))
        '("_")
        (filter-map (lambda (p) (if (equal? p "") #f p)) l))))

(define (http-redirect url . keywords)
  (let-keywords* keywords
      ((status "302 Moved")
       (message ""))
    (set! (http-header 'location) url)
    (set! (http-header 'status) url)
    message))

(provide "cgi/entry")
