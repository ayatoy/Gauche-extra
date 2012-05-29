(define-module extra.daemon
  (export daemon))

(select-module extra.daemon)

(define (daemon :key (nochdir 0) (noclose 0))
  (when (positive? (sys-fork))
    (sys-exit 0))
  (sys-setsid)
  (when (zero? nochdir)
    (sys-chdir "/"))
  (sys-umask 0)
  (when (zero? noclose)
    (call-with-input-file "/dev/null"
      (^[in]
        (port-fd-dup! (standard-input-port) in)))
    (call-with-output-file "/dev/null"
      (^[out]
        (port-fd-dup! (standard-output-port) out)
        (port-fd-dup! (standard-error-port) out)))))
