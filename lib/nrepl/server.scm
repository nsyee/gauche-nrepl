;;; TCP server: accepts connections, frames bencoded messages and writes the
;;; handlers' responses back to the socket. One thread per connection.

(define-module nrepl.server
  (use gauche.net)
  (use gauche.threads)
  (use gauche.uvector)
  (use nrepl.bencode)
  (use nrepl.handlers)
  (export default-port
          start-nrepl-server nrepl-server-port nrepl-server-stop!
          nrepl-server-context nrepl-server-wait
          nrepl-server-main))
(select-module nrepl.server)

(define default-port 7888)
(define read-chunk-size 4096)

(define-class <nrepl-server> ()
  ((socket  :init-keyword :socket)
   (context :init-keyword :context)
   (thread  :init-value #f)
   (running :init-value #t)))

(define (nrepl-server-context server) (~ server 'context))

(define (nrepl-server-port server)
  (sockaddr-port (socket-address (~ server 'socket))))

(define (write-responses out responses)
  (for-each (^r (write-uvector (bencode-encode r) out)) responses)
  (flush out))

;; Per-connection loop. Chunks are accumulated because a bencoded message may
;; be split across TCP packets; `bencode-decode-all` returns the complete
;; messages plus the leftover bytes.
(define (serve-connection ctx sock)
  (let ([in (socket-input-port sock)]
        [out (socket-output-port sock)]
        [route (route-message ctx)])
    (guard (e [(bencode-error? e)
               (format (current-error-port) "decode error: ~a\n"
                       (condition-ref e 'message))]
              [(<system-error> e)
               (format (current-error-port) "socket error: ~a\n"
                       (condition-ref e 'message))])
      (let loop ([buffered (make-u8vector 0)])
        (let1 chunk (read-uvector <u8vector> read-chunk-size in)
          (unless (eof-object? chunk)
            (receive (msgs rest) (bencode-decode-all (u8vector-append buffered chunk))
              (for-each (^m (when (nrepl-message? m)
                              (write-responses out (route m))))
                        msgs)
              (loop rest))))))
    (guard (e [#t #f]) (socket-close sock))))

(define (accept-loop server)
  (let1 lsock (~ server 'socket)
    (let loop ()
      (when (~ server 'running)
        (let1 sock (guard (e [(<system-error> e) #f]) (socket-accept lsock))
          (when sock
            (thread-start!
             (make-thread (^[] (serve-connection (~ server 'context) sock)))))
          (loop))))))

;; Start listening on PORT (0 picks a free port). Returns an <nrepl-server>;
;; the accept loop runs on its own thread.
(define (start-nrepl-server :optional (port default-port) (host "127.0.0.1")
                            :key (context (make-server-context)))
  (let* ([lsock (car (make-server-sockets host port :reuse-addr? #t))]
         [server (make <nrepl-server> :socket lsock :context context)])
    (set! (~ server 'thread) (thread-start! (make-thread (^[] (accept-loop server)))))
    server))

(define (nrepl-server-stop! server)
  (set! (~ server 'running) #f)
  (socket-shutdown (~ server 'socket) SHUT_RDWR)
  (socket-close (~ server 'socket))
  (guard (e [#t #f]) (thread-join! (~ server 'thread) 1))
  (undefined))

(define (nrepl-server-wait server)
  (thread-join! (~ server 'thread)))

;; Entry point for bin/nrepl-server.
(define (nrepl-server-main args)
  (let* ([port (or (and (pair? (cdr args)) (string->number (cadr args)))
                   (and-let* ([p (sys-getenv "NREPL_PORT")]) (string->number p))
                   default-port)]
         [server (start-nrepl-server port)])
    (format #t "nREPL Gauche server listening on port ~a\n" (nrepl-server-port server))
    (flush)
    (nrepl-server-wait server)
    0))
