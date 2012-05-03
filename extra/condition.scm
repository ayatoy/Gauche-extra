(define-module extra.condition
  (export report-error-string))
(select-module extra.condition)

(define (report-error-string e :optional (detail #t))
  (if detail
      (call-with-output-string
        (^[out] (with-error-to-port out (^[] (report-error e)))))
      (condition-ref e 'message)))
