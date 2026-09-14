;;; Bencode encoder / decoder.
;;;
;;; Supported types (as used by nREPL):
;;;   - byte strings : <length>:<contents>   <-> Scheme string (UTF-8)
;;;   - integers     : i<value>e             <-> exact integer
;;;   - lists        : l<items>e             <-> proper list
;;;   - dictionaries : d<key><value>...e     <-> hash table (string=? keys)
;;;
;;; The decoder is incremental: `bencode-decode-all` returns every complete
;;; value found in a u8vector together with the unconsumed remainder, so a TCP
;;; stream can be fed to it chunk by chunk.

(define-module nrepl.bencode
  (use gauche.uvector)
  (use gauche.sequence)
  (use srfi-13)
  (export bencode-encode bencode-encode-string
          bencode-decode bencode-decode-value bencode-decode-all
          bencode-dict bencode-dict? dict-ref dict-set! dict-keys
          <bencode-error> <bencode-incomplete>
          bencode-error? bencode-incomplete?))
(select-module nrepl.bencode)

(define-condition-type <bencode-error> <error> bencode-error?)
(define-condition-type <bencode-incomplete> <bencode-error> bencode-incomplete?)

(define (bencode-error fmt . args)
  (raise (make-condition <bencode-error> 'message (apply format #f fmt args))))

(define (incomplete)
  (raise (make-condition <bencode-incomplete> 'message "incomplete bencode value")))

;;; Dictionaries -----------------------------------------------------------

(define (bencode-dict . kvs)
  (let1 ht (make-hash-table 'string=?)
    (let loop ([kvs kvs])
      (unless (null? kvs)
        (hash-table-put! ht (car kvs) (cadr kvs))
        (loop (cddr kvs))))
    ht))

(define (bencode-dict? x) (hash-table? x))
(define (dict-ref d key :optional (default #f)) (hash-table-get d key default))
(define (dict-set! d key val) (hash-table-put! d key val))
(define (dict-keys d) (sort (hash-table-keys d) string<?))

;;; Encoding ----------------------------------------------------------------

(define (encode-to-port value port)
  (cond
   [(string? value)
    (let1 bytes (string->u8vector value)
      (display (u8vector-length bytes) port)
      (write-char #\: port)
      (write-uvector bytes port))]
   [(and (integer? value) (exact? value))
    (write-char #\i port) (display value port) (write-char #\e port)]
   [(list? value)
    (write-char #\l port)
    (for-each (cut encode-to-port <> port) value)
    (write-char #\e port)]
   [(hash-table? value)
    (write-char #\d port)
    (for-each (^k (encode-to-port k port)
                  (encode-to-port (hash-table-get value k) port))
              (dict-keys value))
    (write-char #\e port)]
   [else (bencode-error "unsupported value: ~s" value)]))

;; Encode VALUE into a u8vector.
(define (bencode-encode value)
  (string->u8vector (bencode-encode-string value)))

;; Encode VALUE into a (byte) string.
(define (bencode-encode-string value)
  (call-with-output-string (cut encode-to-port value <>)))

;;; Decoding ----------------------------------------------------------------

(define (byte-at buf i) (u8vector-ref buf i))
(define (digit? b) (<= 48 b 57))

(define (index-of buf byte start)
  (let1 n (u8vector-length buf)
    (let loop ([i start])
      (cond [(>= i n) #f]
            [(= (byte-at buf i) byte) i]
            [else (loop (+ i 1))]))))

(define (ascii-slice buf start end)
  (u8vector->string buf start end))

(define (decode-string buf start)
  (let1 colon (index-of buf (char->integer #\:) start)
    (unless colon (incomplete))
    (let1 len-text (ascii-slice buf start colon)
      (unless (and (> (string-length len-text) 0)
                   (string-every char-numeric? len-text))
        (bencode-error "invalid string length: ~a" len-text))
      (let* ([len (string->number len-text)]
             [end (+ colon 1 len)])
        (when (> end (u8vector-length buf)) (incomplete))
        (values (u8vector->string buf (+ colon 1) end) end)))))

(define (decode-integer buf start)
  (let1 end (index-of buf (char->integer #\e) start)
    (unless end (incomplete))
    (let1 text (ascii-slice buf (+ start 1) end)
      (unless (#/^-?\d+$/ text)
        (bencode-error "invalid integer: ~a" text))
      (values (string->number text) (+ end 1)))))

(define (decode-list buf start)
  (let loop ([offset (+ start 1)] [items '()])
    (when (>= offset (u8vector-length buf)) (incomplete))
    (if (= (byte-at buf offset) (char->integer #\e))
      (values (reverse items) (+ offset 1))
      (receive (item next) (bencode-decode-value buf offset)
        (loop next (cons item items))))))

(define (decode-dictionary buf start)
  (let1 dict (make-hash-table 'string=?)
    (let loop ([offset (+ start 1)])
      (when (>= offset (u8vector-length buf)) (incomplete))
      (if (= (byte-at buf offset) (char->integer #\e))
        (values dict (+ offset 1))
        (receive (key koff) (decode-string buf offset)
          (receive (val voff) (bencode-decode-value buf koff)
            (hash-table-put! dict key val)
            (loop voff)))))))

;; Decode one value starting at START. Returns (values value next-offset).
;; Raises <bencode-incomplete> when the buffer ends before the value does.
(define (bencode-decode-value buf :optional (start 0))
  (when (>= start (u8vector-length buf)) (incomplete))
  (let1 b (byte-at buf start)
    (cond [(= b (char->integer #\i)) (decode-integer buf start)]
          [(= b (char->integer #\l)) (decode-list buf start)]
          [(= b (char->integer #\d)) (decode-dictionary buf start)]
          [(digit? b) (decode-string buf start)]
          [(= b (char->integer #\-))
           (bencode-error "unexpected \"-\" outside of integer")]
          [else (bencode-error "unexpected byte 0x~x at offset ~a" b start)])))

(define (->u8vector x)
  (if (string? x) (string->u8vector x) x))

;; Decode a single, fully contained bencode value from a u8vector or string.
(define (bencode-decode buf)
  (let1 buf (->u8vector buf)
    (receive (value offset) (bencode-decode-value buf 0)
      (unless (= offset (u8vector-length buf))
        (bencode-error "trailing data after value at offset ~a" offset))
      value)))

;; Decode every complete value in BUF.
;; Returns (values list-of-values remainder-u8vector).
(define (bencode-decode-all buf)
  (let1 buf (->u8vector buf)
    (let loop ([offset 0] [acc '()])
      (let1 next (and (< offset (u8vector-length buf))
                      (guard (e [(bencode-incomplete? e) #f])
                        (receive (v next) (bencode-decode-value buf offset)
                          (cons v next))))
        (if next
          (loop (cdr next) (cons (car next) acc))
          (values (reverse acc) (u8vector-copy buf offset)))))))
