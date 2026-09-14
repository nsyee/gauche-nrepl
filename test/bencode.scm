(use gauche.test)
(use gauche.uvector)
(test-start "nrepl.bencode")
(use nrepl.bencode)
(test-module 'nrepl.bencode)

(test-section "encode")
(test* "string" "5:hello" (bencode-encode-string "hello"))
(test* "utf-8 string counts bytes" "6:日本" (bencode-encode-string "日本"))
(test* "integer" "i42e" (bencode-encode-string 42))
(test* "negative integer" "i-7e" (bencode-encode-string -7))
(test* "list" "li1e1:ae" (bencode-encode-string '(1 "a")))
(test* "dict keys are sorted" "d1:ai1e1:b1:x1:cli2eee"
       (bencode-encode-string (bencode-dict "c" '(2) "a" 1 "b" "x")))
(test* "nested dict" "d1:kd1:vi1eee"
       (bencode-encode-string (bencode-dict "k" (bencode-dict "v" 1))))
(test* "unsupported value" (test-error <bencode-error>)
       (bencode-encode-string 1.5))
(test* "encode returns u8vector" #u8(105 49 101) (bencode-encode 1))

(test-section "decode")
(test* "string" "hello" (bencode-decode "5:hello"))
(test* "utf-8 string" "日本" (bencode-decode "6:日本"))
(test* "integer" 42 (bencode-decode "i42e"))
(test* "list" '(1 "a") (bencode-decode "li1e1:ae"))
(test* "dict" '(("a" . 1) ("b" . "x"))
       (sort (hash-table->alist (bencode-decode "d1:ai1e1:b1:xe"))
             string<? car))
(test* "trailing data" (test-error <bencode-error>) (bencode-decode "i1ei2e"))
(test* "bad byte" (test-error <bencode-error>) (bencode-decode "xyz"))
(test* "bad length" (test-error <bencode-error>) (bencode-decode "a:x"))
(test* "incomplete" (test-error <bencode-incomplete>) (bencode-decode "5:hel"))
(test* "roundtrip"
       '(("code" . "(+ 1 2)") ("id" . "1") ("op" . "eval") ("status" "done"))
       (sort (hash-table->alist
              (bencode-decode
               (bencode-encode (bencode-dict "op" "eval" "id" "1"
                                             "code" "(+ 1 2)" "status" '("done")))))
             string<? car))

(test-section "decode-all")
(test* "complete values and remainder" '((1 2 "ab") #u8(51 58 97 98))
       (receive (vs rest) (bencode-decode-all "i1ei2e2:ab3:ab") (list vs rest)))
(test* "empty buffer" '(() #u8())
       (receive (vs rest) (bencode-decode-all "") (list vs rest)))
(test* "incomplete dict is kept" '(() #u8(100 50 58 111 112))
       (receive (vs rest) (bencode-decode-all "d2:op") (list vs rest)))
(test* "malformed data raises" (test-error <bencode-error>)
       (bencode-decode-all "i1ex"))

(test-end :exit-on-failure #t)
