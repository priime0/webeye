#lang racket

(require racket/logging)
(require racket/sandbox)
(require net/http-easy)

(struct page (title url notify-path) #:transparent)

;; TODO: parse this from a file
(define pages '())

(define (main)
  (with-logging-to-port (current-output-port)
    run
    'debug))

(define (notify+log url message #:level [level 'debug])
  (define priority (level->priority level))
  (define headers (hasheq 'Priority (number->string priority)))
  (post url #:data message #:headers headers)
  (log-message (current-logger) level message))

(define (level->priority level)
  (match level
    ['debug   1]
    ['info    2]
    ['warning 3]
    ['error   4]
    ['fatal   5]
    ['none    1]))

(define (run)
  (define delay-secs 30)
  (let loop ()
    (for ([pg pages])
      (check-update pg))
    (sleep delay-secs)
    (loop)))

(define (check-update pg)
  (match-define [page title url notify-path] pg)
  (define ((notify+log/exn #:level [level 'debug]) e)
    (define message (format "raised by ~a:\n~a" title (exn-message e)))
    (notify+log notify-path message #:level level))
  (with-handlers ([exn:fail:resource? (notify+log/exn #:level 'warning)]
                  [exn?               (notify+log/exn #:level 'error)])
    (define filename (format "~a.html" title))
    (cond [(file-exists? filename)
           (call-with-input-file filename
             (lambda (in)
               (define contents-port (open-output-string))
               (pull-update pg contents-port)
               (define new-contents (get-output-bytes contents-port))
               (define old-contents (port->bytes in))
               (unless (equal? new-contents old-contents)
                 (log-debug "Page at ~s updated... updating file" url)
                 (post notify-path
                       #:data (format "~a was updated!" title))
                 (call-with-output-file filename
                   #:exists 'replace
                   (lambda (out)
                     (write-bytes new-contents out)
                     (void))))))]
          [else
           (log-debug "File ~s does not exist... pulling and creating" filename)
           (call-with-output-file filename
             (curry pull-update pg))])))

(define (pull-update pg out)
  (match-define [page _title url notify-path] pg)
  (log-debug "Retrieving page at url: ~s" url)
  (with-limits 5 #f
    (define res (get url))
    (cond [(= 200 (response-status-code res))
           (define html (response-body (get url)))
           (write-bytes html out)]
          [else
           (define message (format "Non-OK (~s) response status code at url: ~s"
                                   (response-status-code res)
                                   url))
           (notify+log notify-path message #:level 'error)]))
  (void))

(module+ main
  (main))
