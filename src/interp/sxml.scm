(define-module interp.sxml
  (use srfi-1)
  (use srfi-13)
  (use sxml.tools)
  (use sxml.sxpath)
  (use util)
  (use interp)
  (export sxml->xml make-interp-sxml))

(select-module interp.sxml)

;; (define (write-sxml->xml tree)
  
;;   (define (write-attr->xml attr)
;;     (let* ((val (cadr attr))
;;            (q (if (string-any #\' val) "\"" "'")))
;;       (display " ")
;;       (display (car attr))
;;       (display "=")
;;       (display q)
;;       (display (sxml:string->xml val))
;;       (display q)))
  
;;   (cond ((string? tree)
;;          (display (sxml:string->xml tree)))
;;         ((pair? tree)
;;          (if (symbol? (car tree))
;;              (let ((name (sxml:name tree))
;;                    (content (sxml:content-raw tree)))
;;                (case name
;;                  ((!CDATA)
;;                   (display "<![CDATA[")
;;                   (display (cdr tree))
;;                   (display "]]>"))
;;                  ((CDATA)
;;                   (display (cdr tree)))
;;                  (else
;;                   (display "<")
;;                   (display name)
;;                   (for-each write-attr->xml (sxml:attr-list tree))
;;                   (if (null? content)
;;                       (display " />")
;;                       (begin (display ">")
;;                              (write-sxml->xml content)
;;                              (display "</")
;;                              (display name)
;;                              (display ">"))))))
;;              (for-each write-sxml->xml tree)))
;;         (else
;;          (display (sxml:string->xml (x->string tree))))))

(define (sxml->xml tree)
  (define (attr->xml attr)
    (let* ((key (car attr))
           (val (cadr attr))
           (q (if (string-any #\' val) "\"" "'")))
      (list " " (car attr) "=" q (sxml:string->xml val) q)))
  (cond ((string? tree)
         (sxml:string->xml tree))
        ((pair? tree)
         (if (symbol? (car tree))
             (let ((name (sxml:name tree))
                   (content (sxml:content-raw tree)))
               (case name
                 ((!CDATA)
                  `("<![CDATA[" ,(cdr tree) "]]>"))
                 ((CDATA)
                  (cdr tree))
                 (else
                  `("<" ,name ,(map attr->xml (sxml:attr-list tree))
                    ,@(if (null? content)
                          '(" />")
                          `(">" ,@(sxml->xml content) "</" ,name ">"))))))
             (map sxml->xml tree)))
        ((null? tree)
         '())
        (else
         (sxml:string->xml (x->string tree)))))

(define-constant *XML-VERSION*
  #`"<?xml version=\"1.0\" encoding=\",|*default-encoding*|\"?>\n")

(define-constant *XML-CONTENT-TYPE*
  (make-content-type "text/xml"))

(define (make-interp-sxml . keywords)
  (let-keywords* keywords ((top-node 'xml)
                           (content-type *XML-CONTENT-TYPE*)
                           (interp (compose sxml->xml (pa$ cons top-node)))
                           (doctype '()))
    (lambda nodes
      (values `((content-type . ,content-type))
              (cons* *XML-VERSION* doctype (interp nodes))))))

(define-constant *XML-CONTENT-TYPE* (make-content-type "text/xml"))

(add-interp! 'xml (make-interp-sxml :content-type *XML-CONTENT-TYPE*
                                    :interp (compose sxml->xml car)))

(define-constant *RSS-CONTENT-TYPE* (make-content-type "application/rss+xml"))

(let ((interp (cut make-interp-sxml :top-node <> :content-type *RSS-CONTENT-TYPE*)))
  (add-interp! 'rss (interp 'rss))
  (add-interp! 'rdf:RDF (interp 'rdf:RDF)))

(define-constant *XHTML-CONTENT-TYPE* (make-content-type "text/html"))
;;(define-constant *XHTML-DOCTYPE* "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n")
(define-constant *XHTML-DOCTYPE* "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n")

(let ((interp (make-interp-sxml :top-node 'html :content-type *XHTML-CONTENT-TYPE* :doctype *XHTML-DOCTYPE*)))
  (add-interp! 'html interp)
  (add-interp! 'xhtml interp))

(provide "interp/sxml")
