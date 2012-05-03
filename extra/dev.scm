(define-module extra.dev
  (export profiling
          profiling*
          load*))
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
