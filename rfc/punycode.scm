;;;
;;; punycode.scm - A Bootstring encoding of Unicode for IDNA
;;;
;;;   Copyright (c) 2013-2014,2020 Masahiro Hayashi <mhayashi1120@gmail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; Punycode encoding/decoding function
;; RFC 3490 http://www.rfc-editor.org/rfc/rfc3490.txt
;; RFC 3492 http://www.rfc-editor.org/rfc/rfc3492.txt

;; TODO: 6.4 Overflow handling


(define-module rfc.punycode
  (use srfi-14)
  (use srfi-43)
  (use srfi-1)
  (use gauche.uvector)
  (use text.unicode)
  (use gauche.sequence)
  (export
   punycode-decode punycode-encode
   punycode-decode-string punycode-encode-string
   idna-encode-string idna-decode-string))

(select-module rfc.punycode)

;; Punycode: 5. Parameter values for Punycode
(define-constant punycode-base 36)
(define-constant punycode-tmin 1)
(define-constant punycode-tmax 26)
(define-constant punycode-damp 700)
(define-constant punycode-skew 38)
(define-constant punycode-initial-bias 72)
(define-constant punycode-initial-n 128)
(define-constant punycode-delimiter #\-)

(define-constant punycode-chars
  #(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

;; IDNA 3.1.1 Requirements
(define-constant idna-label-separator
  (list->char-set
   (map ucs->char '(#x2e #x3002 #xFF0E #xFF61))))

(define (punycode-encode-string string)
  (with-output-to-string
    (^()
      (encode0 (open-input-string string)))))

(define (punycode-decode-string string)
  (with-output-to-string
    (^()
      (decode0 (open-input-string string)))))

(define (punycode-encode)
  (encode0 (current-input-port)))

(define (punycode-decode)
  (decode0 (current-input-port)))

;; IDNA 3.1 Requirements
(define (idna-encode-string string)

  (define (encode x)
    (if (#/^[\x00-\x7f]*$/ x)
      x
      (with-output-to-string
        (^ ()
          (display "xn--")
          (encode0 (open-input-string x))))))

  (string-join
   (map encode
        ;; it MUST contain only ASCII characters
        (string-split string idna-label-separator))
   "."))

;; IDNA 3.1 Requirements
(define (idna-decode-string string)
  (define (decode x)
    (if-let1 m (#/^xn--/i x)
      (with-output-to-string
        (^ ()
          (decode0 (open-input-string (m 'after)))))
      x))

  (string-join
   (map decode
        (string-split string "."))
   "."))


;;;
;;; Encoder/Decoder
;;;

(define (decode0 iport)
  (receive (ascii non-ascii) (split-delimiter iport)
    (let1 iport (open-input-string non-ascii)
      (decode1 ascii iport))))

(define (encode0 iport)
  (let1 points (->codepoints iport)
    (receive (ascii non-ascii) (partition (^x (< x 128)) points)
      (encode1 points ascii non-ascii))))

;; Punycode: 6.2 Decoding procedure
(define (decode1 ascii iport)

  (define *buffer* (unicode-chars ascii))

  (define (read-a-char i bias)
    (let loop ([weight 1]
               [k punycode-base]
               [acc i])
      (let1 c (read-char iport)
        (when (eof-object? c)
          (error "Bad punycode (Unexpected finish input)"))
        (let1 digit (char->num c)
          (unless digit
            (error "Bad punycode (Invalid char)" c))
          (let* ([acc (+ acc (* digit weight))]
                 [t (punycode-threshold k bias)])
            (cond
             [(< digit t)
              acc]
             [else
              (loop (* weight (- punycode-base t))
                    (+ k punycode-base)
                    acc)]))))))

  (define (output-char i c)
    (set! *buffer* (insert-at *buffer* i c)))

  (let loop ([n punycode-initial-n]         ; start from non-ascii code
             [i 0]
             [bias punycode-initial-bias]
             ;; consume all code points before the last delimiter (if there is one)
             ;;   and copy them to output, fail on any non-basic code point
             ;; if more than zero code points were consumed then consume one more
             ;;   (which will be the last delimiter)
             )

    (cond
     [(eof-object? (peek-char iport))
      (display (apply string *buffer*))]
     [else
      (let* ([old-i i]
             [i (read-a-char i bias)]
             [outend (1+ (length *buffer*))]
             [bias (punycode-adapt (- i old-i) outend (zero? old-i))]
             [n (+ n (div i outend))]
             [c (ucs->char n)]
             [i (mod i outend)])
        (output-char i c)
        (loop n (1+ i) bias))])))

;; Punycode: 6.3 Encoding procedure
(define (encode1 codepoints ascii non-ascii)

  ;; write a char
  (define (write-a-char delta bias)
    (let loop ([q delta]
               [k punycode-base])
      (let1 t (punycode-threshold k bias)
        (cond
         [(< q t)
          (output (num->char q))]
         [else
          (let* ([a1 (- q t)]
                 [a2 (- punycode-base t)])
            (receive (quot rem) (quotient&remainder a1 a2)
              (let* ([digit (+ t rem)]
                     [char (num->char digit)])
                (output char)
                (loop quot (+ k punycode-base)))))]))))

  ;; write a char and index of inserting
  (define (write-char-group m bias old-n h delta indexes)
    (cond
     [(null? indexes)
      bias]
     [else
      (let* ([delta (+ delta (* (- m old-n) h))]
             [n m]
             [i (car indexes)]
             ;; substitution of incrementing `delta'
             [delta-1 (+ i delta)])
        (write-a-char delta-1 bias)
        (let1 bias (punycode-adapt delta-1 h
                                   (= old-n punycode-initial-n))
          (write-char-group m bias m (1+ h) 0 (cdr indexes))))]))

  (define (indexes-of x)
    (let loop ([i 0]
               [lis codepoints]
               [res '()]
               [delta 0])
      (cond
       [(null? lis)
        (values delta (reverse! res))]
       [(> x (car lis))
        (loop (1+ i) (cdr lis) res (1+ delta))]
       [(= x (car lis))
        (loop 0 (cdr lis) (cons i res) 1)]
       [else
        (loop i (cdr lis) res delta)])))

  (define (write-body n bias codepoints h delta)
    (when (pair? codepoints)
      ;; write char and index of inserting
      (let1 m (car codepoints)
        (receive (next-delta indexes) (indexes-of m)
          (let1 bias (write-char-group m bias n (1+ h) delta indexes)
            (write-body (1+ m) bias (cdr codepoints)
                        (+ h (length indexes)) next-delta))))))

  (define (output x)
    (display x))

  (when (pair? ascii)
    (dolist (x (unicode-chars ascii))
      (output x))
    (when (pair? non-ascii)
      (output punycode-delimiter)))

  (let* ([ordered (sort! non-ascii <)]
         [codepoints (delete-duplicates! ordered)])
    (write-body punycode-initial-n punycode-initial-bias
                codepoints (length ascii) 0)))

(define (punycode-threshold k bias)
  (cond
   [(<= k bias)
    punycode-tmin]
   [(>= k (+ bias punycode-tmax))
    punycode-tmax]
   [else
    (- k bias)]))

;; Punycode: 6.1 Bias adaptation function
(define (punycode-adapt delta numpoints firsttime)
  (let* ([delta (quotient delta (if firsttime punycode-damp 2))]
         [delta (+ delta (quotient delta numpoints))]
         [diff (- punycode-base punycode-tmin)])
    (let loop ([delta delta]
               [k 0])
      (cond
       [(<= delta (quotient (* diff punycode-tmax) 2))
        (+ k (quotient (* (1+ diff) delta)
                       (+ delta punycode-skew)))]
       [else
        (loop (quotient delta diff) (+ k punycode-base))]))))

;;;
;;; Utilities
;;;

(define (insert-at lis i n)
  (cond
   [(= i 0)
    (cons n lis)]
   [else
    (cons (car lis) (insert-at (cdr lis) (1- i) n))]))

(define (split-delimiter iport)

  (define (splitter prev)
    (let1 c (read-char iport)
      (cond
       [(eof-object? c)
        (values #f '())]
       [else
        (unless (char-set-contains? char-set:ascii c)
          (error "Invalid punycode (Not a ascii char)" c))
        (receive (prev2 rest2) (splitter (cons c prev))
          (cond
           [(eq? c punycode-delimiter)
            (values (cond
                     [prev2 (cons c prev2)]
                     ;; empty encoded body e.g. "-> $1.00 <-"
                     ;; this means input is ascii text.
                     [(null? rest2) (cons c '())]
                     [else '()])
                    rest2)]
           [(not prev2)
            (if (char->num c)
              (values #f (cons c rest2))
              (values (cons c rest2) '()))]
           [else
            (values (cons c prev2) rest2)]))])))

  (receive (before after) (splitter '())
    (values (apply string (or before '()))
            (apply string after))))

(define (unicode-chars s)
  (map (^x
        (cond
         [(char? x) x]
         [(number? x) (ucs->char x)]
         [else (error "Not supported" x)]))
       s))

(define (->codepoints iport)
  (let loop ([res '()])
    (let1 c (read-char iport)
      (cond
       [(eof-object? c)
        (reverse! res)]
       [else
        (loop (cons (char->ucs c) res))]))))

(define (char->num c)
  (let1 c (char-downcase c)
    (vector-index (^x (eq? x c)) punycode-chars)))

(define (num->char n)
  (vector-ref punycode-chars n))

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))
