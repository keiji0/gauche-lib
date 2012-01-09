(define-module file.index
  (use srfi-1)
  (use dbm)
  (use dbm.gdbm)
  (use binary.pack)
  (autoload util.list drop*)
  (autoload srfi-13 string-downcase)
  (autoload gauche.parseopt (:macro let-args))
  (export restore-paths-index
          paths-index-add!
          path-index-query
          path->index-keys
          path-index-query&delete!
          file-index-command-interface))

(select-module file.index)

;; DB-KEYの形式
;;   path: "/.+" => path-id
(define %path-key? #/^\//)
;;   path-id: "%/[1-9][1-0]*" => path
;;   index-count: "%//"
(define-constant *index-count-key* "%//")
;;   index-key: "[^/]+" => pack "L*" path-id ...  ;; おそらく膨大な数になるのでpackする
(define (make-path-index path)
  (dbm-open <gdbm> :path path :rw-mode :write))

(define (file-exists? path)
  (sys-access path |F_OK|))

(define (path-get-index db path)
  (cond ((dbm-get db path #f) => string->number)
        (else #f)))

(define (index-get-path db index)
  (dbm-get db (make-index-key index)))

(define (make-index-key index)
  (string-append "c/" (x->string index)))

;; 効率の為一括登録 -> 新規登録
(define (register-paths db paths)
  (let ((count (x->number (dbm-get db *index-count-key* 0))))
    (dolist (path paths)
      (unless (dbm-exists? db path)
        (let ((c (number->string (inc! count))))
          (dbm-put! db path c)
          (dbm-put! db (make-index-key c) path))))
    (dbm-put! db *index-count-key* (number->string count))))

(define (unregister-path! db path split)
  (cond ((dbm-get db path #f)
         => (lambda (index)
              (dbm-delete! db path)
              (dbm-delete! db (make-index-key index))
              (dolist (key (split path))
                (cond ((key-paths-get db key #f)
                       => (lambda (path-list)
                            (hash-table-delete! path-list key)
                            (paths-index-put! db key path-list)))))))))

;; インデックスを再構築
(define (restore-paths-index db split)
  (let ((paths (dbm-fold db (lambda (a b)
                              (if (%path-key? a) (cons a b) b))
                         '()))
        (db-path (ref db 'path)))
    (dbm-close db)
    (sys-unlink db-path)
    (let ((new-db (make-path-index db-path)))
      (paths-index-add! db paths split))))

;; DBからパスリストをゲット -> (<hash-table> '(<int/index> . <string/path>) ...)
(define (key-paths-get db key . def)
  (cond ((dbm-get db key #f)
         => (lambda (data)
              (let ((path-ht (make-hash-table 'eqv?)))
                (dolist (index (unpack "L*" :from-string data) path-ht)
                  (hash-table-put! path-ht index (index-get-path db index))))))
        (else
         (get-optional def (make-hash-table 'eqv?)))))

;; パスリストをパックしてDBに登録
(define (paths-index-put! db key key-paths)
  (dbm-put! db key (pack "L*" (hash-table-keys key-paths) :to-string? #t)))

;; DBにPATHSを登録する
(define (paths-index-add! db paths path-split)
  (let ((key-cache (make-hash-table 'string=?))) ;; キーをキャッシュするテープル
    (register-paths db paths)
    (dolist (path paths)
      (let ((path-index (path-get-index db path)))
        ;; パスを分割し各キーを登録
        (dolist (key (path-split path))
          (let ((key-paths (hash-table-get key-cache key #f)))
            ;; キャッシュされていなければ、キャッシュする
            (unless key-paths
              (set! key-paths (key-paths-get db key))
              (hash-table-put! key-cache key key-paths))
            ;; インデックスに登録
            (hash-table-put! key-paths path-index path)))))
    ;; DBに書き込む
    (hash-table-for-each key-cache
                         (lambda (key key-paths)
                           (paths-index-put! db key key-paths)
                           ))))

;; パスをいくつかのキーに分割
(define (path->index-keys path deep)
  (let ((keys (drop* (filter (compose not (pa$ string=? "")) (string-split path #\/)) deep))
        (name (sys-basename path)))
    (append
     (if (#/\..+$/ name)
         (let1 x (string-split name #\.) (list (car x) (string-append "." (cadr x))))
         '())
     (append-map (lambda (key)
                   (map string-downcase
                        (if (#/^.+[,_-].+$/ key)
                            (cons key (string-split key #[-_,]))
                            (list key))))
                 keys))))

;; パスを探す -> keysは論理和
(define (path-index-query db keys)
  (call/cc
    (lambda (no-result)
      (let ((key-paths
             ;; キーが存在しなかった場合無駄な計算を避ける
             (sort (map (lambda (key)
                          (or (key-paths-get db key #f) (no-result '())))
                        keys)
                   (lambda (a b)
                     (< (hash-table-num-entries a)
                        (hash-table-num-entries b)))))
            (result '()))
        (receive (base rest) (car+cdr key-paths)
          (filter-map (lambda (index)
                        (and (every (cut hash-table-exists? <> index) rest)
                             (hash-table-get base index)))
                      (hash-table-keys base)))))))

(define (path-index-query&delete! db keys split)
  (filter (lambda (path)
            (if (file-exists? path)
                #t
                (begin (unregister-path! db path split)
                       (print path)
                       #f)))
          (path-index-query db keys)))

(define (file-index-command-interface args)
  (let-args (cdr args)
      ((index-path "index-path=s" (or (sys-getenv "GAUCHE_PATH_INDEX_PATH")
                                      (error "set $GAUCHE_PATH_INDEX_PATH")))
       (query "query" #f)
       (restore "restore" #f)
       (blank "blank=s" "\n")
       (deep "deep=i" 3) . paths-or-keys)
    (let ((db (make-path-index index-path))
          (split (cut path->index-keys <> deep)))
      (cond (query
             (let1 keys paths-or-keys
               (print (string-join (path-index-query&delete! db keys split) blank))))
            (restore
             (restore-paths-index db split))
            (else
             (let1 paths paths-or-keys
               (let1 paths (if (null? paths)
                               (port->list read-line (current-input-port))
                               paths)
                 (dolist (path paths)
                   (unless (file-exists? path)
                     (error "file.index: No such file or directory " path)))
                 (paths-index-add! db paths split)))))
      )))

(provide "file/index")
