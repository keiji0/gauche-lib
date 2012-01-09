(define-module time
  (export *timezone*))

(select-module time)

(define-constant *timezone*
  (let ((ct (sys-time)))
    (- (sys-mktime (sys-localtime ct))
       (sys-mktime (sys-gmtime ct)))))

(provide "time")
