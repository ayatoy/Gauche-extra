(define-module extra.dev
  (use gauche.sequence)
  (use srfi-1)
  (use gauche.time)
  (export profiling
          profiling*
          load*
          time-begin))
(select-module extra.dev)

(define-macro (profiling expr)
  `(dynamic-wind
       profiler-start
       (lambda () ,expr)
       profiler-stop))

(define-macro (profiling* expr)
  `(begin (profiler-reset)
          (dynamic-wind
              profiler-start
              (lambda () ,expr)
              profiler-stop)
          (profiler-show)))

(define-macro (load* file module-name)
  `(begin (load ,file)
          (import ,module-name)))

(define-syntax time-begin
  (syntax-rules ()
    [(_ body ...)
     (/ (apply + (subseq (sort (map (^[i] (~ (time-this 1 (^[] body ...))'real))
                                    (iota 5))
                               >)
                         1
                         4))
        3)]))
