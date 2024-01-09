#lang racket

(require racket/logging)
(require net/http-easy)

(struct page (title url notify-path) #:transparent)

;; TODO: parse this from a file
(define pages '())

(define (main)
  (with-logging-to-port (current-output-port)
    run
    'debug))

(define (run)
  (define delay-secs 30)
  (let loop ()
    (for ([pg pages])
      (check-update pg))
    (sleep delay-secs)
    (loop)))

(define (check-update pg)
  (match-define [page title url notify-path] pg)
  (define filename (format "~a.html" title))
  (cond [(file-exists? filename)
         (call-with-input-file filename
           (lambda (in)
             (define contents-port (open-output-string))
             (pull-update url contents-port)
             (define new-contents (get-output-bytes contents-port))
             (define old-contents (port->bytes in))
             (unless (equal? new-contents old-contents)
               (log-info "Page at ~s updated... updating file" url)
               (call-with-output-file filename
                 #:exists 'replace
                 (lambda (out)
                   (write-bytes new-contents out)
                   (void))))))]
        [else
         (log-info "File ~s does not exist... pulling and creating" filename)
         (call-with-output-file filename
           (curry pull-update url))]))

(define (pull-update url out)
  (log-info "Retrieving page at url: ~s" url)
  (define res (get url))
  (unless (= 200 (response-status-code res))
    (log-error "Non-OK response status code at url: ~s" url))
  (define html (response-body (get url)))
  (write-bytes html out)
  (void))

(module+ main
  (main))
