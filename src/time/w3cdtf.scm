(define-module time.w3cdtf
  (use srfi-19)
  (use time)
  (export time->w3cdtf
          time->iso8601
          w3cdtf->time
          iso8601->time))

(select-module time.w3cdtf)

(define (time->w3cdtf time . keywords)
  (let-keywords* keywords ((type 3) ;; [1-5]
                           (zone-offset *timezone*)
                           (utc #f))
    (string-append
      (sys-strftime (case type
                      ((1) "%Y")
                      ((2) "%Y-%m")
                      ((3) "%Y-%m-%d")
                      ((4) "%Y-%m-%dT%H:%M")
                      ((5) "%Y-%m-%dT%H:%M:%S"))
                    ((if utc sys-gmtime sys-localtime) time))
      (cond ((< type 4) "")
            ((or utc (zero? zone-offset)) "Z")
            ((or (> zone-offset 43200)
                 (< zone-offset -43200))
             (error "zone-offset: -43200 < zone-offset < 43200"))
            (else
             (receive (h m) (quotient&remainder (abs zone-offset) 3600)
               (format "~a~2,'0d:~2,'0d"
                       (if (positive? zone-offset) "+" "-")
                       h
                       (quotient m 60))))))))

#;(test* "w3cdtf->time" #t
       (and
        (= (w3cdtf->time "2005-08-16T10:10:50+09:00")
           (- (w3cdtf->time "2005-08-17T10:10:50+09:00")
              (* 24 60 60)))
        (= (w3cdtf->time "2005-08-16T10:10:50Z")
           (+ (w3cdtf->time "2005-08-16T10:10:50+09:00")
              (* 60 60 9)))))

(define (w3cdtf->time w3cdtf . def)
  (rxmatch-if (#/(\d{4})(?:-(\d{2})(?:-(\d{2})(?:T(\d{2}):(\d{2})(?::(\d{2})(?:\.(\d+))?)?(Z|([+-]\d{2}:\d{2}))?)?)?)?/ w3cdtf)
      (_ ye mo da ho mi se ns tz)
    (time-second
     (date->time-utc
      (make-date
       (string->number (or ns "0"))
       (string->number (or se "0"))
       (string->number (or mi "0"))
       (string->number (or ho "0"))
       (string->number (or da "1"))
       (string->number (or mo "1"))
       (string->number ye)
       (if (or (equal? tz "Z") (not tz))
           0
           (rxmatch-let (#/([-+])(\d{2}):(\d{2})/ tz)
               (_ os tmi tho)
             ((if (equal? "+" os) + -)
              (+ (* (string->number tmi) 3600)
                 (* (string->number tho) 60))))))))
    (get-optional def (sys-time))))

(define time->iso8601 time->w3cdtf)
(define iso8601->time w3cdtf->time)

#;(test* "time->w3cdtf <> w3cdtf->time" "2005-08-21T17:38:30Z"
       (time->w3cdtf (w3cdtf->time "2005-08-21T17:38:30Z")
                     :type 5 :utc #t))

#;(test* "time->w3cdtf <local> w3cdtf->time" "2005-08-21T17:38:30+09:00"
       (time->w3cdtf (w3cdtf->time "2005-08-21T08:38:30Z")
                     :type 5 :zone-offset 32400))

(provide "time/w3cdtf")
