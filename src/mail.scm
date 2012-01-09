(define-module mail
  (use rfc.base64)
  (use gauche.net)
  (use gauche.charconv)
  (export send-mail))

(select-module mail)

;; (send-mail
;;  "mail.bar.com"
;;  "hoge@mail.bar.com"
;;  '("foo@mail.xxx.jp" "jojo@mail.xxx.com")
;;  (open-input-string "test")
;;  :subject "あああああ"
;;  :port 25)
(define (send-mail host from recipients message . keywords)
  (let-keywords* keywords
      ((port 25)
       (subject "no subject")
       (content-type "text/plain; charset=ISO-2022-JP"))
    (with-error-handler
        (lambda (e) (errorf "send-mail failed: ~a" (slot-ref e 'message)))
      (lambda ()
        (call-with-client-socket (make-client-socket 'inet host port)
          (lambda (in out)
            (define (send-command command code)
              (when command (format out "~a\r\n" command))
              (let* ((line (read-line in))
                     (return-code (string->number (substring line 0 3))))
                (if (eq? return-code code)
                    line
                    (errorf "smtp-error: ~a => ~a" command line))))
            (send-command #f 220)
            (send-command (format "HELO ~a" (sys-gethostname)) 250)
            (send-command (format "MAIL FROM: <~a>" from) 250)
            (dolist (rcpt (if (string? recipients) (list recipients) recipients))
              (send-command (format "RCPT TO: <~a>" rcpt) 250))
            (send-command "DATA" 354)
            (send-header `((From . ,from)
                           (Subject . ,subject)
                           (Content-Type . ,content-type))
                         out)
            (send-body message out)
            (send-command "." 250)
            (send-command "QUIT" 221)))))))

(define (send-header headers out)
  (define (send k v) (format out "~a: ~a\r\n" k v))
  (dolist (header headers (display "\r\n" out))
    (case (car header)
      ((Subject subject)
       (send (car header) (subject-encode (cdr header))))
      (else
       (send (car header) (cdr header)))))
  )

(define (send-body iport out)
  (let1 iport (open-input-conversion-port iport "*JP" :to-code "ISO-2022-JP")
    (port-for-each (lambda (line)
                     (if (equal? "." line)
                         (format out "..\r\n")
                         (format out "~a\r\n" line)))
                   (cut read-line iport))))

(define (string-length-split l s)
  (let1 len (string-length s)
    (let loop ((start 0)
               (result '()))
      (let1 end (+ start l)
        (if (< end len)
            (loop end (cons (substring s start end) result))
            (reverse! (cons (substring s start len) result)))))))

(define (subject-encode s)
  (with-output-to-string
    (lambda ()
      (display (string-join (map (lambda (t)
                                   (format "=?ISO-2022-JP?B?~a?="
                                           (base64-encode-string (ces-convert t "*JP" "ISO-2022-JP"))))
                                 (string-length-split 12 s))
                            "\n ")))))

(provide "mail")
