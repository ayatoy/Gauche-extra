(define-module extra.list
  (use gauche.collection)
  (use gauche.threads)
  (use srfi-1)
  (use control.job)
  (use control.thread-pool)
  (use sxml.ssax)
  (use util.list)
  (use util.queue)
  (export alist
          %
          alref
          alput
          alcut
          set-keyword
          set-keywords
          set-keywords*
          thread-pool-map
          thread-pool-map1
          thread-pool-track-map
          lset-similar))
(select-module extra.list)

;;;; alist

(define (alist . kvs)
  (reverse (fold (^[kv r] (cons (cons (car kv) (cadr kv)) r))
                 '()
                 (slices kvs 2))))

(define % alist)

(define (alref alist . ks)
  (fold (^[k alist] (and (list? alist) (assoc-ref alist k)))
        alist
        ks))

(define (alset alist . args) )

(define (aldel alist . ks) )

(define (alput1 alist k v)
  (let loop ([alist alist] [acc '()] [e #f])
    (if (null? alist)
        (reverse (or (and e acc)
                     (cons (cons k v) acc)))
        (let* ([x  (car alist)]
               [k1 (car x)]
               [p  (equal? k k1)])
          (loop (cdr alist)
                (cons (cons k1 (if p v (cdr x)))
                      acc)
                (or e p))))))

(define (alput alist . kvs)
  (fold (^[kv r] (apply alput1 r kv))
        alist
        (slices kvs 2)))

(define (alcut alist . ks)
  (reverse (fold (^[kv r] (let1 k (car kv)
                            (if (any (^[x] (equal? k x)) ks)
                              r
                              (cons kv r))))
                 '()
                 alist)))

;;;; keywords

(define (set-keyword key kv-list obj)
  `(,@(delete-keyword key kv-list) ,key ,obj))

(define (set-keywords keys kv-list)
  (fold (^[key kv-list] (set-keyword (car key) kv-list (cadr key)))
        kv-list
        (slices keys 2)))

(define (set-keywords* kv-list . keys)
  (let loop ((kv-list kv-list) (keys keys))
    (if (null? keys)
      kv-list
      (loop (set-keywords (car keys) kv-list)
            (cdr keys)))))

;;;; mt

(define (thread-pool-map size proc col . cols)
  (let* ([pool (make-thread-pool size)]
         [jobs (apply map
                      (^ args (add-job! pool (^[] (apply proc args)) #f))
                      (cons col cols))])
    (when (wait-all pool)
      (begin0 (map job-result jobs)
              (terminate-all! pool)))))

(define (thread-pool-map1 proc col :key (size 1)
                                   (force-timeout #f)
                                   (force-timeout-val #f))
  (let* ([pool (make-thread-pool size)]
         [jobs (map (^[x] (add-job! pool (^[] (proc x)) #f)) col)])
    (terminate-all! pool :force-timeout force-timeout)
    (map (^[job] (let1 status (job-status job)
                   (case status
                     [(done error) (job-result job)]
                     [else force-timeout-val])))
         jobs)))

(define (track-for-each proc col :key (key identity)
                                      (test eqv?)
                                      (interval #e5e8))
  (let1 ht (make-hash-table 'eq?)
    (for-each (^[g] (hash-table-put! ht (list->queue g) 0))
              (group-collection col :key key :test test))
    (let loop ()
      (unless (= (hash-table-num-entries ht) 0)
        (sys-nanosleep interval)
        (hash-table-for-each
         ht
         (^[q x] (cond [(= (queue-length q) 0) (hash-table-delete! ht q)]
                       [else (proc (dequeue! q))])))
        (loop)))))

(define (thread-pool-track-map proc col :key (size 1)
                                             (key identity)
                                             (test eqv?)
                                             (interval #e5e8)
                                             (timeout #f)
                                             (timeout-val #f))
  (let ([pool (make-thread-pool size)]
        [jobs (make-queue)])
    (track-for-each (^[x] (enqueue! jobs (add-job! pool (^[] (proc x)) #f)))
                    col
                    :key key
                    :test test
                    :interval interval)
    (terminate-all! pool :force-timeout timeout)
    (map (^[job] (let1 status (job-status job)
                   (case status
                     [(done error) (job-result job)]
                     [else timeout-val])))
         (queue->list jobs))))

;;;; lset

(define (lset-similar . lsets)
  (/ (* (length lsets)
        (length (apply lset-intersection equal? lsets)))
     (apply + (map length lsets))))
