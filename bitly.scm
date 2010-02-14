;;;
;;; bitly.scm - bit.ly API Module for Gauche (v0.0.1)
;;;
;;; Copyright (c) 2010 Takuya Mannami <mtakuya@users.sourceforge.jp>
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;;
;;; 2. Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.
;;;
;;; 3. Neither the name of the authors nor the names of its contributors
;;; may be used to endorse or promote products derived from this
;;; software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(define-module bitly
  (use rfc.uri)
  (use rfc.http)
  (use sxml.ssax) 
  (use sxml.sxpath)
  (export make-bitly _make-bitly shorten expand info stats))

(select-module bitly)

(define-class <bitly> ()
  ((user-name :init-keyword :user-name)
   (api-key   :init-keyword :api-key)
   (url       :init-keyword :url)
   (sxmlcache :init-value #f)))

;path
(define %path-host    "api.bit.ly")
(define %path-shorten "/shorten")
(define %path-expand  "/expand")
(define %path-info    "/info")
(define %path-stats   "/stats")
(define %path-errors  "/errors")

;value
(define *shortUrl* "shortUrl")
(define *longUrl*  "longUrl")
(define *version*  "2.0.1")
(define *format*   "xml")

;symbol
(define *shorten* 'SHORTEN)
(define *expand*  'EXPAND)
(define *info*    'INFO)
(define *stats*   'STATS)
(define *errors*  'ERRORS)

(define-syntax make-bitly
  (syntax-rules ()
    ((_ user api) (_make-bitly user api))
    ((_ user api url) (_make-bitly user api :url url))))
 
(define (_make-bitly  user api :key (url #f))
  (make <bitly> :user-name user :api-key api :url url))

(define-method shorten ((obj <bitly>)) (get-bitly obj %path-shorten *shorten*))
(define-method shorten ((obj <bitly>) (url <string>))
  (get-bitly obj %path-shorten *shorten* :url url))

(define-method expand ((obj <bitly>)) (get-bitly obj %path-expand *expand*))
(define-method expand ((obj <bitly>) (url <string>))
  (get-bitly obj %path-expand *expand* :url url))

(define-method info ((obj <bitly>)) (get-bitly obj %path-info *info*))
(define-method info ((obj <bitly>) (url <string>))
  (get-bitly obj %path-info *info* :url url))

(define-method stats ((obj <bitly>)) (get-bitly obj %path-stats *stats*))
(define-method stats ((obj <bitly>) (url <string>))
  (get-bitly obj %path-stats *stats* :url url))

(define-method errors ((obj <bitly>))
  (get-bitly obj %path-errors *errors*))

;private
(define (get-bitly obj path sym :key (url #f))
  (define (make-path) 
    (receive (_ path _ _)
             (uri-decompose-hierarchical (values-ref (uri-scheme&specific (ref obj 'url)) 1))
             (string->symbol (substring path 1 -1))))
  (define (make-param q) (format "&~a=~a&" q (ref obj 'url)))
  (define (parse-xml xml path-lst)
    (let* ((in (open-input-string xml)) (out (ssax:xml->sxml in '())))
      (let1 sxml ((sxpath path-lst) out)
            (set! (ref obj 'sxmlcache) sxml) ;;ca
            sxml)))
  (define (get-bitly-xml path param)
    (values-ref 
     (http-get %path-host
               (string-append
                #`",|path|?version=,|*version*|,|param|"
                #`"login=,(ref obj 'user-name)&"
                #`"apiKey=,(ref obj 'api-key)&format=,|*format*|")) 2))
  (when url (set! (ref obj 'url) url))
  (cond ((equal? sym *shorten*)
         (cadar (parse-xml (get-bitly-xml path (make-param *longUrl*))
                           '(bitly results nodeKeyVal shortUrl))))
        ((equal? sym *expand*)
         (cadar (parse-xml (get-bitly-xml path (make-param *shortUrl*))
                           `(bitly results ,(make-path) longUrl))))
        ((equal? sym *info*)
         (parse-xml (get-bitly-xml path (make-param *shortUrl*))
                           '(bitly)))
        ((equal? sym *stats*)
         (parse-xml (get-bitly-xml path (make-param *shortUrl*))
                           '(bitly)))
        ;((equal? sym *errors*) (parse-xml (get-bitly-xml path "") '()))
        (else (error "api error"))))

(provide "bitly")
