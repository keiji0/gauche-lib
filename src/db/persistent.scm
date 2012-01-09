(define-module db.persistent
  (use srfi-1)
  (use srfi-13)
  (use util.list)
  (use file.util)
  (use dbm)
  (use gauche.parameter)
  (use util)
  (export <persistent-meta>
          find-persistent-class
          <persistent-base>
          key-of
          find-instance
          delete-instance!
          with-db
          current-db
          db-sync
          with-transaction
          serialize-value
          ))

(select-module db.persistent)

;;; Persistent metaclass
(define-class <persistent-meta> (<class>)
  ((all-class :allocation :class :init-value (make-hash-table 'string=?))
   (normal-accessors :init-form (make-hash-table))))

(define-method initialize ((class <persistent-meta>) initargs)
  (next-method)
  (hash-table-put! (ref class 'all-class)
                   (symbol->string (class-name class))
                   class))

(define-method compute-get-n-set ((class <persistent-meta>) slot)

  (define (make-getter acc)
    (lambda (o)
      (force (slot-ref-using-accessor o acc))))

  (define (make-setter acc)
    (lambda (o v)
      (let1 db (slot-ref o '%db)
        (unless (memq o (ref db 'modified-instances))
          (push! (ref db 'modified-instances) o))
        (slot-set-using-accessor! o acc v))))

  (define (make-boundp acc)
    (lambda (o)
      (slot-bound-using-accessor? o acc)))

  (define (make-accessor name)
    (make <slot-accessor>
      :class class
      :name name
      :slot-number (slot-ref class 'num-instance-slots)
      :initializable #t))

  (if (eq? :persistent (slot-definition-allocation slot))
      (let* ((name (slot-definition-name slot))
             (acc (make-accessor name)))
        (hash-table-put! (ref class 'normal-accessors) name acc)
        (inc! (ref class 'num-instance-slots))
        (list (make-getter acc)
              (make-setter acc)
              (make-boundp acc)
              #t))
      (next-method)))

(define (%slot-set! obj sn val)
  (slot-set-using-accessor! obj
    (hash-table-get (slot-ref (class-of obj) 'normal-accessors) sn)
    val))

(define (find-persistent-class name)
  (hash-table-get (class-slot-ref <persistent-meta> 'all-class)
                  (if (symbol? name) (symbol->string name) name)
                  #f))

;; Persistent base class
(define-class <persistent-base> ()
  (
   (%key :getter key-of)
   (%floating-instance :init-keyword :%floating-instance :init-value #t)
   %db %db-key
   )
  :metaclass <persistent-meta>)

(define-method initialize ((obj <persistent-base>) initargs)
  (let-keywords* initargs ((%realization-values #f)
                           (%db (or (current-db) (error "No db is active")))
                           (key (error "make set :key")))
    (slot-set! obj '%key (if (string? key) key (error "persistent object -- :key <string>")))
    (slot-set! obj '%db %db)
    (slot-set! obj '%db-key (make-dbm-key (class-of obj) key))
    (next-method)
    (when %realization-values
      (realization-for-each (pa$ %slot-set! obj) %realization-values))
    (object-cache obj)))

(define-method delete-instance! ((obj <persistent-base>))
  (slot-update! (slot-ref obj '%db) 'delete-instances
                (lambda (x) (cons obj (delete! obj x)))))

(define (find-instance class key)
  (read-persistent-base (current-db) class key))

(define (object-cache obj)
  (hash-table-put! (ref (ref obj '%db) 'instance-by-key)
                   (ref obj '%db-key)
                   obj))


;; * Serialize value

(define primitive-value?
  (any-pred string? symbol? number? boolean? keyword?))

(define (serialize-value v)
  
  (define (persistent-collection? lis)
    (if (null? lis)
        #f
        (let lp ((lis lis))
          (cond ((null? lis) #t)
                ((pair? lis)
                 (and (is-a? (car lis) <persistent-base>)
                      (lp (cdr lis))))
                (else #f)))))

  (define (write-persistent-collection col)
    (display "#,(c ")
    (display (class-name (class-of (car col))))
    (dolist (c col) (display " ") (write (key-of c)))
    (display ")"))

  (define (write-pair v)
    (display "(")
    (let lp ((v v))
      (cond ((null? v))
            ((pair? v) (serialize-value (car v)) (display " ") (lp (cdr v)))
            (else (display ". ") (serialize-value v))))
    (display ")"))

  (cond
   ((primitive-value? v)
    (write v))
   ((null? v)
    (display "()"))
   ((pair? v)
    (if (persistent-collection? v)
        (write-persistent-collection v)
        (write-pair v)))
   ((is-a? v <persistent-base>)
    (display "#,(p ")
    (display (class-name (class-of v))) (display " ")
    (write (key-of v))
    (display ")"))
   ((vector? v)
    (display "#(") (for-each serialize-value v) (display ")"))
   ((hash-table? v)
    (format #t "#,(t ~a " (hash-table-type v))
    (hash-table-for-each v
      (lambda (key val)
        (format #t "(~s . " key)
        (serialize-value val)
        (display ")")))
    (display ")"))
   ((promise? v)
    (serialize-value (force v)))
   (else
    (write-object v))))

;; Persistent object: #,(p <class> "key")
(define-reader-ctor 'p
  (lambda (class key)
    (find-instance (find-persistent-class class) key)))

;; Persistent collection: #,(c <class> "key" ...)
(define-reader-ctor 'c
  (lambda (class key . keys)
    (map (pa$ find-instance (find-persistent-class class))
         (cons key keys))))

;; <hash-table>: #,(t type (key . val) ...)
(define-reader-ctor 't
  (lambda (type . pairs)
    (let1 ht (make-hash-table type)
      (for-each (lambda (pair)
                  (hash-table-put! ht (car pair) (cdr pair))) 
                pairs)
      ht)))

(define (realization-for-each proc rv)
  (with-input-from-string rv
    (lambda ()
      (let loop ((sn (read))
                 (sv (read-line)))
        (unless (eof-object? sn)
          (proc sn (delay (call-with-input-string sv read)))
          (loop (read) (read-line)))))))

(define (reload-persistent-instance obj)
  (let1 rv (dbm-get (ref obj '%db) (ref obj '%db-key) #f)
    (when rv
      (realization-for-each (pa$ %slot-set! obj) rv))))


;; * Object database

(define-class <object-db> (<dbm>)
  (
   (dbm :init-keyword :dbm)
   (instance-by-key :init-form (make-hash-table 'string=?))
   (instance-all :init-form (make-hash-table))
   (modified-instances :init-form '())
   (delete-instances :init-form '())
   ))

(define-method dbm-close ((self <object-db>))
  (dbm-close (ref self 'dbm)))

(define-method dbm-close? ((self <object-db>))
  (dbm-close? (ref self 'dbm)))

(define-method dbm-put! ((self <object-db>) key val)
  (dbm-put! (ref self 'dbm) key val))

(define-method dbm-get ((self <object-db>) key . args)
  (apply dbm-get (ref self 'dbm) key args))

(define-method dbm-exists? ((self <object-db>) key)
  (dbm-exists? (ref self 'dbm) key))

(define-method dbm-delete! ((self <object-db>) key)
  (dbm-delete! (ref self 'dbm) key))

(define-constant *retry-limit* 3)
(define-constant *EAVAIL-message* "resource temporarily unavailable")

(define (db-try-open dbtype dbpath . initargs)
  (define (try retry)
    (with-error-handler
      (lambda (e)
        (cond ((>= retry *retry-limit*) (raise e))
              ((string-contains-ci (ref e 'message) *EAVAIL-message*)
               (sys-sleep 1)
               (try (+ retry 1)))
              (else
               (raise e))))
      (lambda ()
        (make <object-db>
          :dbm (apply dbm-open dbtype :path dbpath :rw-mode :write initargs)))))
  (try 0))

(define (db-open dbtype dbpath . initargs)
  (let* ((db (apply db-try-open dbtype dbpath initargs))
         (instance-all (ref db 'instance-all)))
    (hash-table-for-each (class-slot-ref <persistent-meta> 'all-class)
      (lambda (key class)
        (hash-table-put! instance-all class
          (call-with-input-string (dbm-get db key "()") read))))
    db))

(define (db-close db commit)
  (when commit (db-sync db))
  (unless (dbm-closed? db) (dbm-close db)))

(define (db-sync . maybe-db)
  (let ((db (get-optional maybe-db (current-db))))
    (for-each write-persistent-base! (ref db 'modified-instances))
    (for-each delete-persistent-base! (ref db 'delete-instances))
    (hash-table-for-each (ref db 'instance-all)
      (lambda (class val)
        (dbm-put! db (symbol->string (class-name class)) (write-to-string val))))))

(define current-db (make-parameter #f))

(define-syntax with-db
  (syntax-rules ()
    ((_ (dbm path . initargs) . body)
     (let ((db (db-open dbm path . initargs)))
       (with-db (db) . body)))
    ((_ (db) . body)
     (parameterize ((current-db db))
       (with-error-handler
           (lambda (e)
             (db-close db #f)
             (raise e))
         (lambda ()
           (begin0 (begin . body)
                   (db-close db #t))))))))

(define (make-dbm-key class key)
  (string-append (symbol->string (class-name class)) ":" key))

(define (read-persistent-base db class key)
  (let ((db-key (make-dbm-key class key)))
    (or
     (hash-table-get (slot-ref db 'instance-by-key) db-key #f)
     (and=> (dbm-get db db-key #f)
            (pa$ make class
                 :key key
                 :%db db
                 :%floating-instance #f
                 :%realization-values)))))

(define (write-persistent-base! obj)
  (let ((class (class-of obj))
        (db    (ref obj '%db)))
    (dbm-put! db (ref obj '%db-key)
              (with-output-to-string
                (lambda ()
                  (for-each
                    (lambda (slot)
                      (when (eq? (slot-definition-allocation slot) :persistent)
                        (let1 sn (slot-definition-name slot)
                          (display sn) (display " ")
                          (serialize-value (slot-ref obj sn))
                          (newline))))
                    (class-slots class)))))
    (when (slot-ref obj '%floating-instance)
      (slot-set! obj '%floating-instance #f)
      (hash-table-update! (ref db 'instance-all) class
        (lambda (is)
          (cons (key-of obj) (delete! (key-of obj) is)))))
    (slot-update! db 'modified-instances (pa$ delete! obj))))

(define (delete-persistent-base! obj)
  (let* ((db (ref obj '%db))
         (dk (ref obj '%db-key))
         (ia (ref db 'instance-all)))
    (dbm-delete! db dk)
    (hash-table-update! ia (class-of obj) (cut delete! (key-of obj) <> equal?))
    (hash-table-delete! (slot-ref db 'instance-by-key) dk)
    ))

(define-syntax with-transaction
  (syntax-rules ()
    ((_ body ...)
     (with-error-handler
       (lambda (e)
         (db-rool-back (current-db))
         (raise e))
       (lambda ()
         (begin0 (begin body ...) (db-sync)))))))

(define (db-rool-back db)
  (let ((ht (ref db 'instance-by-key)))
    (dolist (obj (ref db 'modified-instances)
                 (slot-set! db 'modified-instances '()))
      (if (ref obj '%floating-instance)
          (hash-table-delete! ht (ref obj '%db-key))
          (reload-persistent-instance obj)))))

;; Persistent collection

(use gauche.collection)
(export make-collection)

(define-class <persistent-collection> (<collection>)
  ((class :init-keyword :class)
   (instances :init-keyword :instances :init-value '())))

(define-method make-collection ((class <persistent-meta>) . select?)
  (let1 ai (hash-table-get (ref (current-db) 'instance-all) class)
    (make <persistent-collection>
      :class class
      :instances (if (null? select?)
                     ai
                     (filter (car select?) ai)))))

(define-method call-with-iterator ((coll <persistent-collection>) proc . opts)
  (let ((p (ref coll 'instances))
        (class (ref coll 'class)))
    (apply call-with-iterator p
           (lambda (end? next)
             (proc end?
                   (lambda ()
                     (if (end?) #f (find-instance class (next))))))
           opts)))

(provide "db/persistent")
