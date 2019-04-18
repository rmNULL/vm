#lang racket
(require db)
(require "./schema.sql.rkt")
(provide init-db close-db)

;; TODO: load variables from environment, instead of hardcoding

;; not a safe installation procedure
(define INSTALL-DIR (build-path (find-system-path 'home-dir) ".vm"))
(unless (directory-exists? INSTALL-DIR)
  (make-directory INSTALL-DIR))
(define DBNAME "vm.db")

(define (load-schema! D)
  (for ([tbl-qry TABLE])
    (query-exec D (cdr tbl-qry))))

(define (init-db [config-dir #f])
  (define D (sqlite3-connect
             #:database (if config-dir config-dir (build-path INSTALL-DIR DBNAME))
             #:mode 'create))
  (query-exec D "pragma foreign_keys = on")
  (load-schema! D)
  D)

(define (close-db D)
  (when (and (connection? D)
             ( connected? D))
    (disconnect D)))