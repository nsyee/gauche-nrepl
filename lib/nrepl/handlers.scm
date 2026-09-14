;;; Operation handlers: each takes a request dictionary plus the server
;;; context and returns the list of response dictionaries to send back.
;;; Only session bookkeeping mutates state; no socket I/O happens here.

(define-module nrepl.handlers
  (use gauche.threads)
  (use rfc.uuid)
  (use nrepl.bencode)
  (export make-server-context server-context-sessions
          handle-clone handle-eval handle-close handle-describe
          route-message nrepl-message?))
(select-module nrepl.handlers)

;;; Server context ----------------------------------------------------------

(define-class <server-context> ()
  ((sessions :init-form (make-hash-table 'string=?))
   (mutex    :init-form (make-mutex))))

(define (make-server-context) (make <server-context>))

(define (server-context-sessions ctx) (~ ctx 'sessions))

(define (with-sessions ctx thunk)
  (with-locking-mutex (~ ctx 'mutex) thunk))

(define (session-ref ctx id)
  (and id (with-sessions ctx (^[] (hash-table-get (~ ctx 'sessions) id #f)))))

;;; Sessions ----------------------------------------------------------------

;; Each session is a fresh anonymous module inheriting `gauche`, so bindings
;; persist per session and are isolated between sessions. `exit` is shadowed:
;; evaluated code must not be able to take the whole server down.
(define (make-session-module)
  (rlet1 m (make-module #f)
    (eval '(define (exit . _) (error "exit is not available inside an nREPL session")) m)))

;;; Responses ---------------------------------------------------------------

(define (nrepl-message? x)
  (and (bencode-dict? x) (string? (dict-ref x "op"))))

(define (msg-id msg) (or (dict-ref msg "id") ""))

(define (response msg . kvs)
  (rlet1 d (bencode-dict "id" (msg-id msg))
    (and-let* ([s (dict-ref msg "session")]) (dict-set! d "session" s))
    (let loop ([kvs kvs])
      (unless (null? kvs)
        (dict-set! d (car kvs) (cadr kvs))
        (loop (cddr kvs))))))

(define (done msg . kvs)
  (apply response msg "status" '("done") kvs))

;;; clone -------------------------------------------------------------------

(define (handle-clone msg ctx)
  (let1 id (uuid->string (uuid4))
    (with-sessions ctx (^[] (hash-table-put! (~ ctx 'sessions) id (make-session-module))))
    (list (bencode-dict "id" (msg-id msg) "new-session" id "status" '("done")))))

;;; eval --------------------------------------------------------------------

(define (read-forms code)
  (with-input-from-string code
    (^[] (port->sexp-list (current-input-port)))))

(define (eval-forms forms module)
  (let loop ([forms forms] [result (undefined)])
    (if (null? forms)
      result
      (loop (cdr forms) (eval (car forms) module)))))

(define (format-error e)
  (cond [(condition-has-type? e <message-condition>)
         (format "~a: ~a"
                 (class-name (class-of e))
                 (condition-ref e 'message))]
        [else (format "~s" e)]))

(define (handle-eval msg ctx)
  (let ([module (session-ref ctx (dict-ref msg "session"))]
        [code (dict-ref msg "code")])
    (cond
     [(not module)
      (list (response msg "status" '("error" "unknown-session" "done")))]
     [(or (not code) (string=? code ""))
      (list (done msg))]
     [else
      (let* ([out (open-output-string)]
             [result
              (guard (e [#t (cons 'err (format-error e))])
                (with-output-to-port out
                  (^[] (cons 'value
                             (write-to-string
                              (eval-forms (read-forms code) module))))))]
             [printed (get-output-string out)]
             [outs (if (string=? printed "")
                     '()
                     (list (response msg "out" printed)))])
        (append outs
                (if (eq? (car result) 'value)
                  (list (response msg "value" (cdr result)) (done msg))
                  (list (response msg "err" (cdr result) "status" '("eval-error"))
                        (done msg)))))])))

;;; close -------------------------------------------------------------------

(define (handle-close msg ctx)
  (and-let* ([s (dict-ref msg "session")])
    (with-sessions ctx (^[] (hash-table-delete! (~ ctx 'sessions) s))))
  (list (response msg "status" '("session-closed" "done"))))

;;; describe ----------------------------------------------------------------

(define (handle-describe msg)
  (list (done msg
              "ops" (bencode-dict "clone" (bencode-dict)
                                  "eval" (bencode-dict)
                                  "close" (bencode-dict)
                                  "describe" (bencode-dict))
              "versions" (bencode-dict
                          "gauche" (bencode-dict
                                    "version-string" (gauche-version))))))

;;; Routing -----------------------------------------------------------------

(define (route-message ctx)
  (^[msg]
    (case (string->symbol (dict-ref msg "op"))
      [(clone) (handle-clone msg ctx)]
      [(eval) (handle-eval msg ctx)]
      [(close) (handle-close msg ctx)]
      [(describe) (handle-describe msg)]
      [else (list (response msg "status" '("error" "unknown-op" "done")))])))
