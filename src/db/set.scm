(define-module db.set
  (use srfi-1)
  (use util.list)
  (use gauche.collection)
  (use gauche.sequence)
  (use util)
  (use db.persistent)
  (export <elt>
          <set>
          size-of
          find-set
          find-set-all
          set-copy
          set<=
          set=
          set-adjoin
          set-adjoin!
          set-union
          set-difference
          set-xor
          set-diff+intersection
          set-intersection
          subseq*
          map-set$
          ))

(select-module db.set)

(define-class <elt> (<persistent-base>)
  ((< :allocation :virtual
      :slot-ref
      (lambda (self)
        (set-copy (slot-ref self '%<)))
      :slot-set!
      (lambda (self set)
        (check-arg set-set? set)
        (let1 es (slot-ref self '<)
          (let ((old-set (es '- set))
                (new-set (set '- es)))
            (for-each (cut slot-update! <> '> (pa$ delete! self)) old-set)
            (for-each (cut set-adjoin! <> self) new-set)
            (slot-set! self '%< (set))))))
   (%< :allocation :persistent
       :init-value '()
       :init-keyword :<)))

(define-method delete-instance! ((elt <elt>))
  (next-method)
  (dolist (set (slot-ref elt '%<))
    (slot-update! set '> (pa$ delete! elt))
    (dec! (ref set 'len))))

(define-class <dummy-set> (<collection>)
  ((>   :init-keyword :>
        :init-value '())
   (len :init-keyword :len
        :init-value 0)))

(define (set-copy elts)
  (make <dummy-set> :> elts :len (delay (length elts))))

(define-method size-of ((set <dummy-set>))
  (force (slot-ref set 'len)))

(define-class <set> (<elt> <dummy-set>)
  ((>     :init-keyword :>
          :init-value '()
          :allocation :persistent)
   (len   :init-keyword :len
          :init-value 0
          :allocation :persistent)
   (ctime :init-form (sys-time))))

(define-method size-of ((set <set>))
  (slot-ref set 'len))

(define (make-set class name . args)
  (apply make class :key name args))

(define-method delete-instance! ((set <set>))
  (next-method)
  (for-each (cut slot-update! <> '%< (pa$ delete! set))
            (ref set '>)))

(define (set-set? set)
  (any (cut is-a? <> <set>) (slot-ref set '>)))

(define (find-set class sn . create?)
  (or (find-instance class sn)
      (and (get-optional create? #f)
           (let$ (make-set class sn)
                 (pa$ set-adjoin! (find-set-all class))))))

(define (find-set-all class)
  (find-set <set> (symbol->string (class-name class)) #t))

(define (set-* lset-*)
  (lambda sets
    (set-copy (apply lset-* eq? (map (cut slot-ref <> '>) sets)))))

(define set<= (set-* lset<=))
(define set=  (set-* lset=))
(define set-union (set-* lset-union))
(define set-difference (set-* lset-difference))
(define set-xor (set-* lset-xor))
(define set-intersection (set-* lset-intersection))

(define (set-adjoin set . elts)
  (set-copy (append elts (remove (cut memq <> elts) (ref set '>)))))

(define (set-adjoin! set . elts)
  (set '<= (apply set-adjoin set elts))
  (dolist (elt elts)
    (slot-update! elt '%< (lambda (x) (cons set (delete! set x))))))

(define (set-diff+intersection . sets)
  (receive (a b)
      (apply lset-diff+intersection eq? (map (cut slot-ref <> '>) sets))
    (values (set-copy a)
            (set-copy b))))

(define (set-intersection . sets)
  (let ((sets (sort sets
                    (lambda (a b)
                      (< (size-of a) (size-of b))))))
    (let ((set (car sets))
          (rset (cdr sets)))
      (if (null? rset) set
          (set-copy
            (filter (lambda (elt)
                      (let ((set2 (slot-ref elt '%<)))
                        (let lp ((rset rset))
                          (or (null? rset)
                              (and (memq (car rset) set2)
                                   (lp (cdr rset)))))))
                    (slot-ref set '>)))))))

(define-method object-apply ((set <dummy-set>))
  (slot-ref set '>))

(define-method object-apply ((set <dummy-set>) (n <integer>) . n2)
  (apply subseq* set n n2))

(define-method object-apply ((set1 <dummy-set>) o (set2 <dummy-set>))
  (case o
    ((<=)
     (if (is-a? set1 <set>)
         (begin (for-each (cut slot-update! <> '%< (pa$ delete! set1))
                          (set1 '- set2))
                (for-each (cut slot-update! <> '%< (pa$ cons set1))
                          (set2 '- set1))
                (slot-set! set1 'len (size-of set2)))
         (slot-set! set1 'len (slot-ref set2 'len)))
     (slot-set! set1 '> (slot-ref set2 '>)))
    ((=>)
     (set2 '<= set1))
    ((& AND)
     (set-intersection set1 set2))
    ((&- AND&DIFF)
     (set-diff+intersection set1 set2))
    ((|\|| OR)
     (set-union set1 set2))
    ((^ XOR)
     (set-xor set1 set2))
    ((- DIFF)
     (set-difference set1 set2))
    ((<)
     (set<= set1 set2))
    ((>)
     (set<= set2 set1))
    ((=)
     (set= set1 set2))
    ((!=)
     (not (set= set1 set2)))
    (else
     (error "not found operator:" o))))
     
(define-method call-with-iterator ((coll <dummy-set>) proc . args)
  (apply call-with-iterator (slot-ref coll '>) proc args))

(define (subseq* set s . e)
  (let ((set (drop* (if (is-a? set <dummy-set>) (ref set '>) set) s)))
    (set-copy
     (or (and=> (get-optional e #f)
                (cut - <> 1)
                (pa$ take* set))
         set))))

(define (map-set$ class names)
  (set-copy (map (cut find-set class <> #t) names)))

(provide "db/set")
