(define-module makefile
  (use srfi-1)
  (use file.util)
  (use gauche.parameter)
  (use gauche.parseopt)
  (export target-make
          makefile-apply
          makefile-get
          makefile-get-targets
          makefile-get-target-depends
          define-target
          current-makefile
          <makefile>
          makefile-command-interface))

(select-module makefile)

(define (%file-exists? p)
  (and (string? p) (sys-access p |F_OK|)))

(define (%string-or-symbol-comp-gen comp)
  (lambda (a b)
    (comp (x->string a) (x->string b))))

(define-class <makefile> ()
  ((targets :init-keyword :targets :init-form (make-hash-table 'equal?))))

(define (makefile-get-targets . makefile)
  (hash-table-keys (ref (current-makefile) 'targets)))

(define (makefile-get-target-depends target . def)
  (cond ((hash-table-get (ref (current-makefile) 'targets) target #f)
         => (lambda (t)
              (ref t 'depends)))
        (else (get-optional def #f))))

(define current-makefile (make-parameter (make <makefile>)))

(define (makefile-reset) (current-makefile (make <makefile>)))

(define (makefile-error s . f)
  (apply errorf (string-append "Makefile: " s) f))

(define-class <target> ()
  ((path :init-keyword :path)
   (recipe :init-keyword :recipe)
   (depends :init-keyword :depends :init-value '())
   (makep :init-keyword :makep :init-value #f)
   (mapping :init-keyword :mapping :init-value #f)))

(define-method makefile-push! ((makefile <makefile>) (target <target>))
  (hash-table-put! (ref makefile 'targets) (ref target 'path) target))

(define-method makefile-get ((makefile <makefile>) path . def)
  (let ((targets (slot-ref makefile 'targets)))
    (or (hash-table-get targets path #f)
        (apply hash-table-get targets (string->symbol path) def))))

(define (x->list x) (if (list? x) x (list x)))

(define (makefile-target-glob lst)
  (let ((ht (make-hash-table 'equal?)))
    (dolist (a (x->list lst))
      (dolist (b (cond ((string? a)
                        (if (#/(\*|\[.+\])/ a) (sys-glob a) (list a)))
                       ((symbol? a)
                        (list a))
                       (else
                        (directory-list (current-directory) :children? #t :filter a))))
        (hash-table-put! ht b #t)))
    (sort (hash-table-keys ht) (%string-or-symbol-comp-gen string<?))))

(define-syntax define-target
  (syntax-rules (<= = =>)
    ((_ targets = depends)
     (define-target targets = depends (lambda _ #t)))
    ((_ targets = depends recipe)
     (let ((%targets (x->list targets)) (%depends (x->list depends)))
       (for-each
         (lambda (t d)
           (makefile-push! (current-makefile)
                           (make <target> :path t :depends (list d) :recipe recipe :mapping #t)))
         %targets %depends)
       %targets))
    ((_ targets => depends-gen . recipe)
     (let* ((%targets (makefile-target-glob targets))
            (%depends (map depends-gen %targets)))
       (define-target %targets = %depends . recipe)))
    ((_ targets-gen <= depends . recipe)
     (let* ((%depends (makefile-target-glob depends))
            (%targets (map targets-gen %depends)))
       (define-target %targets = %depends . recipe)))
    ((_ targets depends)
     (define-target targets depends (lambda _ #t)))
    ((_ targets depends recipe)
     (let ((%targets (makefile-target-glob targets)) (%depends (makefile-target-glob depends)))
       (dolist (t %targets %targets)
         (makefile-push! (current-makefile) (make <target> :path t :depends %depends :recipe recipe)))))
    ))

(define-method target-check? ((target <target>))
  (for-each (lambda (depend)
              (cond ((makefile-get (current-makefile) depend #f) => target-check?)
                    ((%file-exists? depend))
                    (else (errorf "makefile-depend: ~a <- ~a" (ref target 'path) depend))))
            (ref target 'depends)))

(autoload text.template text-template-string-apply)

(define-method target-cook ((target <target>) . keywords)
  (define regexp-replace-all
    (with-module gauche.regexp
      (lambda (rx string proc)
        (define (regexp-replace-rec match subpat out rec)
          (display (rxmatch-before match) out)
          (subpat match out)
          (rec (rxmatch-after match)))
        (let ((match (rxmatch rx string)))
          (if match
              (call-with-output-string
                (lambda (out)
                  (define (loop str)
                    (unless (equal? str "")
                      (cond ((rxmatch rx str)
                             => (lambda (match)
                                  (when (= (rxmatch-start match) (rxmatch-end match))
                                    (error "regexp-replace-all: matching zero-length string causes infinite loop:" rx))
                                  (regexp-replace-rec match proc out loop)))
                            (else (display str out)))))
                  (regexp-replace-rec match proc out loop)))
              string)))))
  (define (command-replace com t d)
    (text-template-string-apply
     (regexp-replace-all #/\$(@|<|DEPEND|TARGET)/ com
       (lambda (m out)
         (let ((k (m 1)))
           (cond ((string=? k "@") (display t out))
                 ((string=? k "<") (display (if (list? d) (string-join d " ") d) out))
                 ((string=? k "DEPEND") (display #\' out) (write d out) (display #\' out))
                 ((string=? k "TARGET") (display #\' out) (write t out) (display #\' out))))))))
  (let-keywords* keywords ((silent #f))
    (begin0
      (let ((recipe (ref target 'recipe))
            (depends ((if (ref target 'mapping) car values) (ref target 'depends)))
            (path (ref target 'path)))
        (unless silent
          (format #t "make: ~a <- ~a <- ~s\n" path recipe depends))
        (cond ((string? recipe)
               (sys-system (command-replace recipe path depends)))
              ((procedure? recipe)
               (recipe path depends))
              ((and (list? recipe) (every string? recipe))
               (for-each (lambda (c)
                           (sys-system (command-replace c path depends)))
                         recipe))
              (else
               (makefile-error "target-cook: invalid recipe"))))
      (slot-set! target 'makep #t))))

(define-method target-make ((target <target>) . keywords)
  (target-check? target)
  (let-keywords* keywords ((silent #f))
    ((rec loop
       (lambda (target)
         (if (ref target 'makep)
             #f
             (let* ((path (ref target 'path))
                    (target-exists (%file-exists? path))
                    (depends (ref target 'depends)))
               (dolist (depend depends)
                 (cond ((makefile-get (current-makefile) depend #f)
                        => loop)
                       (else
                        (unless (%file-exists? depend)
                          (makefile-error "No rule to make target `~a', needed by `~a'. Stop." path depend)))))
               (unless (and (string? path)
                            (every (lambda (depend)
                                     (and (%file-exists? depend)
                                          target-exists
                                          (file-mtime<? depend path)))
                                   depends))
                 (target-cook target :silent silent))))))
     target)))

(define (makefile-apply target-name . keywords)
  (let-keywords* keywords ((makefile (current-makefile)))
    (let ((target (makefile-get makefile target-name #f)))
      (if target
          (apply target-make target keywords)
          (makefile-error "No rule to make target `~a'. Stop." target-name)))))

(define (makefile-command-interface args)
  (let-args (cdr args)
      ((verbose "v|verbose" #f)
       (printp "p|print" #f) . target)
    (cond (printp
           (receive (commands files) (partition symbol? (makefile-get-targets))
             (format #t "commands: ~s\n" (map string->symbol (sort (map symbol->string commands))))
             (format #t "files: ~s\n" (sort files))))
          (else
           (for-each (cut makefile-apply <> :silent (not verbose))
                     (if (null? target)
                         (makefile-error "No targets specified and no makefile found. Stop.")
                         target))))))

;; alias make.scm="gosh -usrfi-1 -usrfi-13 -umakefile -e'(define main makefile-command-interface)' Makefile.scm"

(provide "makefile")
