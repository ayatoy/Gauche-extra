(define-module extra.string
  (use srfi-13)
  (use srfi-27)
  (export string-erase
          string-take
          string-ngram
          string-random))
(select-module extra.string)

(define (string-erase str start end)
  (call-with-output-string
    (^[out]
      (call-with-input-string str
        (^[in]
          (let loop ([c (read-char in)] [w #t])
            (unless (eof-object? c)
              (cond [(and w (char=? c start))
                     (loop (read-char in) #f)]
                    [(and (not w) (char=? c end))
                     (loop (read-char in) #t)]
                    [else
                     (when w (write-char c out))
                     (loop (read-char in) w)]))))))))

(define (string-take st s e)
  (if (> (+ s e) (string-length st))
    st
    (substring st s e)))

(define (string-ngram s n)
  (let* ([len (string-length s)]
         [i1  (- len (- n 1))])
    (if (<= n len)
      (let loop ([i 0] [acc '()])
        (if (= i i1)
          (reverse acc)
          (loop (+ i 1) (cons (substring s i (+ i n)) acc))))
      (list s))))

(random-source-randomize! default-random-source)

(define (random-char)
  (let1 i (random-integer 62)
    (cond [(<=  0 i  9) (integer->char (+ 48 (random-integer 10)))]
          [(<= 10 i 35) (integer->char (+ 65 (random-integer 26)))]
          [(<= 36 i 61) (integer->char (+ 97 (random-integer 26)))])))

(define (string-random n)
  (letrec ([random-char-list (^[i] (if (= i 0)
                                     '()
                                     (cons (random-char)
                                           (random-char-list (- i 1)))))])
    (list->string (random-char-list n))))
