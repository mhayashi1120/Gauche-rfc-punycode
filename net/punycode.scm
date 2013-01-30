;;; RFC 3492 Punycode: A Bootstring encoding of Unicode for IDNA
;;; http://tools.ietf.org/html/rfc3492

(define-module net.punycode
  (use srfi-1)
  (use gauche.uvector)
  (use text.unicode)
  (use gauche.sequence)
  (export
   punycode-decode punycode-encode
   punycode-decode-string punycode-encode-string
   punycode-decode-domain punycode-encode-domain))

(select-module net.punycode)

(define-constant punycode-chars
  '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define-constant punycode-base (length punycode-chars))
(define-constant punycode-tmin 1)
(define-constant punycode-tmax 26)
(define-constant punycode-damp 700)
(define-constant punycode-skew 38)
(define-constant punycode-initial-bias 72)
(define-constant punycode-initial-n 128)
(define-constant punycode-delimiter #\-)

(define (punycode-encode-string string)
  (if (#/^[\x00-\x7f]*$/ string)
    string
    (let* ([points (->codepoints string)]
           [oport (open-output-string)])
      (display "xn--" oport)
      (receive (ascii non-ascii) (partition (^x (< x 128)) points)
        (encode1 oport points ascii non-ascii)
        (get-output-string oport)))))

(define (punycode-decode-string string)
  (if-let1 m (#/^xn--(?:([\x00-\x7f]+)-)?/ string)
    (let* ([ascii (or (m 1) "")]
           [punybody (m 'after)]
           [iport (open-input-string punybody)])
      (decode1 ascii iport))
    string))

(define (punycode-encode iport)
  (punycode-encode-string (get-remaining-input-string iport)))

(define (punycode-decode iport)
  (punycode-decode-string (get-remaining-input-string iport)))

(define (punycode-encode-domain domain)
  (string-join
   (map punycode-encode-string
        (string-split domain #[.]))
   "."))

(define (punycode-decode-domain domain)
  (string-join
   (map punycode-decode-string
        (string-split domain #[.]))
   "."))

;;;
;;; Encoder/Decoder
;;;

(define (decode1 ascii iport)

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

  (define *buffer* (unicode-chars ascii))

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

    (let* ([old-i i]
           [i (read-a-char i bias)]
           [outend (1+ (length *buffer*))]
           [bias (punycode-adapt (- i old-i) outend (zero? old-i))]
           [n (+ n (div i outend))]
           [c (ucs->char n)]
           [i (mod i outend)])
      (output-char i c)
      (cond
       [(eof-object? (peek-char iport))
        (apply string *buffer*)]
       [else
        (loop n (1+ i) bias)]))))

(define (encode1 oport codepoints ascii non-ascii)

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
    (display x oport))

  (when (pair? ascii)
    (dolist (x (unicode-chars ascii))
      (output x))
    (output punycode-delimiter))

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

(define (unicode-chars s)
  (map (^x
        (cond
         [(char? x) x]
         [(number? x) (ucs->char x)]
         [else (error "Not supported" x)]))
       s))

(define (->codepoints s)
  (map char->ucs s))

(define (char->num c)
  (let1 c (char-downcase c)
    (list-index (^x (eq? x c)) punycode-chars)))

(define (num->char n)
  ;;TODO chars table to vector?
  (list-ref punycode-chars n))

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))
