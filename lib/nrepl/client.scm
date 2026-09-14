;;; nREPL client: a synchronous API plus an interactive line-oriented REPL.

(define-module nrepl.client
  (use gauche.net)
  (use gauche.uvector)
  (use rfc.uuid)
  (use srfi-13)
  (use nrepl.bencode)
  (use nrepl.server)
  (export nrepl-connect nrepl-send nrepl-clone nrepl-eval nrepl-close
          nrepl-disconnect nrepl-client-connected?
          response-status response-done? response-ref
          nrepl-client-main))
(select-module nrepl.client)

(define read-chunk-size 4096)

(define-class <nrepl-client> ()
  ((socket   :init-keyword :socket)
   (buffered :init-form (make-u8vector 0))
   ;; Responses that arrived for ids other than the one being awaited.
   (pending  :init-form (make-hash-table 'string=?))
   (closed   :init-value #f)))

(define (nrepl-connect :optional (port default-port) (host "127.0.0.1"))
  (make <nrepl-client> :socket (make-client-socket 'inet host port)))

(define (nrepl-client-connected? client) (not (~ client 'closed)))

(define (response-ref res key :optional (default #f)) (dict-ref res key default))
(define (response-status res) (or (response-ref res "status") '()))
(define (response-done? res) (member "done" (response-status res)))

(define (mark-closed! client)
  (set! (~ client 'closed) #t)
  (guard (e [#t #f]) (socket-close (~ client 'socket))))

;; Read one chunk from the socket into the decode buffer. Returns the list of
;; complete responses; raises when the connection has gone away.
(define (read-responses! client)
  (let1 chunk (read-uvector <u8vector> read-chunk-size
                            (socket-input-port (~ client 'socket)))
    (when (eof-object? chunk)
      (mark-closed! client)
      (error "connection closed before the response was complete"))
    (receive (values rest)
        (bencode-decode-all (u8vector-append (~ client 'buffered) chunk))
      (set! (~ client 'buffered) rest)
      (filter bencode-dict? values))))

(define (stash! client res)
  (let* ([pending (~ client 'pending)]
         [id (or (dict-ref res "id") "")])
    (hash-table-update! pending id (cut cons res <>) '())))

(define (take-pending! client id)
  (let* ([pending (~ client 'pending)]
         [rs (hash-table-get pending id '())])
    (hash-table-delete! pending id)
    (reverse rs)))

;; Send MSG (a bencode dict; "id" is generated when absent) and return every
;; response up to and including the one carrying `status: ["done"]`.
(define (nrepl-send client msg)
  (when (~ client 'closed) (error "client is disconnected"))
  (let1 id (or (dict-ref msg "id")
               (rlet1 id (uuid->string (uuid4)) (dict-set! msg "id" id)))
    (let1 out (socket-output-port (~ client 'socket))
      (write-uvector (bencode-encode msg) out)
      (flush out))
    ;; ACC holds matching responses newest-first; stop at the first `done`.
    (let collect ([acc '()] [incoming (take-pending! client id)])
      (cond
       [(and (pair? acc) (response-done? (car acc)))
        (for-each (cut stash! client <>) incoming)
        (reverse acc)]
       [(null? incoming) (collect acc (read-responses! client))]
       [(equal? (dict-ref (car incoming) "id") id)
        (collect (cons (car incoming) acc) (cdr incoming))]
       [else
        (stash! client (car incoming))
        (collect acc (cdr incoming))]))))

(define (nrepl-clone client)
  (let1 rs (nrepl-send client (bencode-dict "op" "clone"))
    (or (any (cut dict-ref <> "new-session") rs)
        (error "clone did not return a new-session"))))

(define (nrepl-eval client code session)
  (nrepl-send client (bencode-dict "op" "eval" "code" code "session" session)))

(define (nrepl-close client session)
  (nrepl-send client (bencode-dict "op" "close" "session" session)))

(define (nrepl-disconnect client)
  (unless (~ client 'closed)
    (guard (e [#t #f]) (socket-shutdown (~ client 'socket) SHUT_RDWR))
    (mark-closed! client))
  (undefined))

;;; Interactive REPL --------------------------------------------------------

(define (print-responses responses)
  (for-each (^r (and-let* ([o (dict-ref r "out")]) (display o))
                (and-let* ([e (dict-ref r "err")]) (format (current-error-port) "~a\n" e))
                (and-let* ([v (dict-ref r "value")]) (format #t "=> ~a\n" v)))
            responses)
  (flush))

(define (repl port)
  (let* ([client (nrepl-connect port)]
         [session (nrepl-clone client)])
    (format #t "connected to nREPL on port ~a (session ~a)\n" port session)
    (unwind-protect
        (let loop ()
          (display "nrepl> ") (flush)
          (let1 line (guard (e [(and (<unhandled-signal-error> e)
                                     (eqv? (~ e 'signal) SIGINT))
                                (eof-object)])
                       (read-line))
            (cond
             [(eof-object? line) (newline)]
             [(member (string-trim-both line) '(":quit" ":exit"))]
             [(string=? (string-trim-both line) "") (loop)]
             [else
              (let1 ok (guard (e [#t (format (current-error-port) "~a\n"
                                             (condition-ref e 'message))
                                     #f])
                         (print-responses (nrepl-eval client line session))
                         #t)
                (when ok (loop)))])))
      (guard (e [#t #f]) (nrepl-close client session))
      (nrepl-disconnect client))))

;; Entry point for bin/nrepl-client.
(define (nrepl-client-main args)
  (let1 port (or (and (pair? (cdr args)) (string->number (cadr args)))
                 (and-let* ([p (sys-getenv "NREPL_PORT")]) (string->number p))
                 default-port)
    (repl port)
    0))
