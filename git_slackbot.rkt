#! /usr/bin/racket
#lang racket

 ; Copyright (C) 2016 Wesley Kerfoot
 ; This program is free software: you can redistribute it and/or modify
 ; it under the terms of the GNU General Public License as published by
 ; the Free Software Foundation, either version 3 of the License, or
 ; (at your option) any later version.
 ;
 ; This program is distributed in the hope that it will be useful,
 ; but WITHOUT ANY WARRANTY; without even the implied warranty of
 ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ; GNU General Public License for more details.
 ;
 ; You should have received a copy of the GNU General Public License
 ; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require json)
(require net/url)

(struct Author
  (name email))

(struct SlackCommit
  (commit-url
   author
   time
   message))

(define max-len 177)

(define services-key "YOUR SERVICE HOOK KEY HERE")

(define webhook-url
  (string->url
   (format
    "https://hooks.slack.com/services/~a"
    services-key)))

(define repo-url
  "YOUR GITHUB REPO") ; e.g. https://github.com/exampleuser/repo

; Run a command and get the string written to stdout
(define (system-result command)
  (match
      (process command)
    [(list out in pid errport _)

     (let ([result (port->string out)]
           [err-result (port->string out)])

       (close-input-port out)
       (close-output-port in)
       (close-input-port errport)
       (cond
         [(not (string=? ""
                         (string-trim result)))
                result]
         [else #f]))]))

(define (line? str)
  (not
   (regexp-match #rx"^[:space:]*$" str)))

(define (truncate msg)
  (list->string
   (take
    (string->list msg)
    max-len)))

(define (limit-size msg)
  (define strl (string-length msg))
  (cond
    [(> strl max-len)
      (format "~a..." (truncate msg))]
    [else msg]))

(define (format-commit msg)
  (format
   "Commit message: ~a"
   (limit-size
    (string-trim msg))))

(define (parse-author str)
  (apply
   Author
   (string-split
    (string-trim
     (cadr
      (string-split
       str ":")))
    " ")))

(define (hash->url str)
  (let ([hash
         (cadr
          (string-split str " "))])
    (format
     "~acommit/~a"
     repo-url
     hash)))

(define (get-last-commit)
  (filter
   line?
   (string-split
   (system-result
    "git log -1 HEAD")
   "\n")))

(define (get-slackmsg)
  (match (get-last-commit)
    [(list
      hash
      author
      time
      msg ...)
     (SlackCommit (hash->url hash)
                  (parse-author author)
                  time
                  (format-commit (string-join msg " ")))]
    [_ '()]))

(define (slackmsg->json msg)
  (string->bytes/utf-8
   (jsexpr->string
   `#hash(
         [text . "New commit to the Github repository"]
         [attachments .
           [
            #hash(
                  [text . ,(SlackCommit-message msg)]
                  [author_name . ,(Author-name (SlackCommit-author msg))]
                  [title . "See what changed"]
                  [title_link . ,(SlackCommit-commit-url msg)]
                  )
            ]
           ]
         ))))

(define get-commit
  (compose1 slackmsg->json
            get-slackmsg))

(post-pure-port
 webhook-url
 (get-commit))